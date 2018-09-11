##################################################################################################
# Title: Reusable pipelines for streaming analyses
# Version: 18.08.01
# Created on: July 12, 2018
# Description: An R package version - Currently supports Apache Spark Structured Streaming
##################################################################################################

#' @name StreamingAnalysisPipeline
#' @title Function to initialize \code{StreamingAnalysisPipeline} class with the input Spark DataFrame
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initialize}
#' @slot input The input Spark DataFrame on which analysis is to be performed
#' @slot workingInput Internal slot for having a working version of the input
#' @slot pipeline A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions for Streaming Analyses
#' @export StreamingAnalysisPipeline
#' @exportClass StreamingAnalysisPipeline

StreamingAnalysisPipeline <- setClass("StreamingAnalysisPipeline",
                           slots = c(
                             input = "SparkDataFrame",
                             workingInput = "SparkDataFrame",
                             pipeline = "tbl",
                             registry = "tbl",
                             output = "list"
                           ))

#' @name initialize
#' @title Constructor for the \code{StreamingAnalysisPipeline} object
#' @param .Object The \code{StreamingAnalysisPipeline} object
#' @param input The Spark DataFrame on which operations need to be performed
#' @details
#'      \code{input} needs to be provded and the argument needs to be of class \code{SparkDataFrame}, which is
#'      generally created through operations using SparkR
#' @return an object of class "\code{StreamingAnalysisPipeline}", initialized with the input Spark DataFrame provided
#' @family Package core functions for Streaming Analyses
#' @export

setMethod(
  f = "initialize",
  signature = "StreamingAnalysisPipeline",
  definition = function(.Object, input)
  {
    .Object@input <- input
    .Object@pipeline <- tibble(
      order = numeric(),
      operation = character(),
      heading = character(),
      parameters = list(),
      outAsIn = logical()

    )
    .Object@registry <- tibble(
      functionName = character(),
      heading = character(),
      outAsIn = logical(),
      engine = character(),
      userDefined = logical()

    )

    for(rowNo in 1:nrow(streamingPredefFunctions)){
      .Object %>>% registerStreamingFunction(streamingPredefFunctions[['functionName']][[rowNo]],
                                         streamingPredefFunctions[['heading']][[rowNo]],
                                         streamingPredefFunctions[['outAsIn']][[rowNo]],
                                         streamingPredefFunctions[['engine']][[rowNo]],
                                         userDefined = F) -> .Object
    }
    return(.Object)
  }
)


#' @name registerStreamingFunction
#' @title Register a user-defined function to be used with \code{StreamingAnalysisPipeline} objects,
#'  currently for Spark Structured Streaming through Spark DataFrames
#' @details
#'       The specified operation along with the heading and parameters is updated in the pipeline slot
#'       of the \code{StreamingAnalysisPipeline} object, where the sequence of operations to be performed is stored. This function
#'       is used to register functions which are designed to operate on Spark Structured Streaming DataFrames
#' @param object object that contains input, pipeline, registry and output
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param outAsIn whether to use original input or output from previous function
#' @param loadPipeline logical parameter to see if function is being used in loadPipeline or not
#' @param session to load shiny session in the function (Currently not implemented)
#' @return Updated \code{StreamingAnalysisPipeline} object
#' @family Package core functions for Streaming Analyses
#' @export

setGeneric(
  name = "registerStreamingFunction",
  def = function(object, functionName,  heading ="", outAsIn=F,
                 engine = 'spark-structured-streaming',
                 loadPipeline=F,
                 userDefined = T, session=session)
  {
    standardGeneric("registerStreamingFunction")
  }
)


### TO DO - Parameterize the data frame class name, if other streaming data frame types are to be incorporated
setMethod(
  f = "registerStreamingFunction",
  signature = "StreamingAnalysisPipeline",
  definition = function(object, functionName,  heading ="", outAsIn=F,
                        engine = 'spark-structured-streaming', loadPipeline=F,
                        userDefined = T, session=session)
  {
    parametersName <- names(as.list(args(eval(parse(text=functionName)))))
    parametersName <- paste0(parametersName[c(-1,-length(parametersName))],collapse=",")
    if(parametersName != ""){
      parametersName <- paste0(", ", parametersName)
    }
    methodBody <- paste0(capture.output(body(eval(parse(text=functionName)))),collapse="\n")
    firstArg <- names(as.list(args(eval(parse(text=functionName)))))[1]
    methodBody <- paste0("{",firstArg,"=object",substring(methodBody,2))
    methodArg <- paste0(capture.output(args(eval(parse(text=functionName)))),collapse="")
    methodArg <- strsplit(strsplit(methodArg,firstArg)[[1]][2],"NULL")[[1]][1]
    registerFunText <- paste0("setGeneric(
                              name = \"",functionName,"\",
                              def = function(object",parametersName,")
                              {
                              standardGeneric(\"",functionName,"\")
                              }
                              )

                              setMethod(
                              f = \"",functionName,"\",
                              signature = \"StreamingAnalysisPipeline\",
                              definition = function(object",parametersName,")
                              {
                              parametersList <- unlist(strsplit(\"",sub(", ", "", parametersName),"\",\",\"))
                              parametersPassed <- lapply(parametersList,function(x){eval(parse(text = x))})

                              return(updateObject(object, \"",functionName,"\", \"",heading,"\", parametersPassed ,",outAsIn,"))
                              }
                              )
                              setMethod(
                              f = \"",functionName,"\",
                              signature = \"SparkDataFrame\",
                              definition = function(object ",methodArg,"",methodBody,")
                              ")

    eval(parse(text = registerFunText), envir=.GlobalEnv)
    if(loadPipeline==F){
      object@registry %>>% add_row(functionName = paste0(functionName),
                                   heading = heading,
                                   outAsIn = outAsIn,
                                   engine = engine,
                                   userDefined = userDefined) -> object@registry
    }
    return(object)
  }
)


#'### TODO
#'  @name loadStreamingPipeline
#' @title Loads the \code{StreamingAnalysisPipeline} object from the file system
#' @details
#'       The \code{StreamingAnalysisPipeline} object is loaded into the file system from the file system
#'       based on the path specified.
#' @param RDSPath the path at which the .RDS file containing the pipeline is located
#' @param input Spark Sructuded Streaming DataFrame with which the pipeline object should be initialized
#' @return An \code{StreamingAnalysisPipeline} object, optinally initialized with the data frame provided
#' @family Package core functions for Streaming Analyses
#' @export
loadStreamingPipeline <- function(RDSPath, input){
  object <- readRDS(RDSPath)
    object@input <- input
  registeredFunctions <- object@registry
  for(rowNo in 1:nrow(registeredFunctions)){
    object %>>% registerStreamingFunction(registeredFunctions[['functionName']][[rowNo]],
                                      registeredFunctions[['heading']][[rowNo]],
                                      registeredFunctions[['outAsIn']][[rowNo]],
                                      registeredFunctions[['engine']][[rowNo]],
                                      userDefined =  registeredFunctions[['userDefined']][[rowNo]],
                                      loadPipeline = T) -> object
  }
  return(object)
}


.generateStreamingOutput = function(object)
{
  inputToExecute <- object@input

  ## Check engine setup
  object %>>% assessEngineSetUp ->  engineAssessment
  engineAssessment %>>% dplyr::filter(requiredForPipeline == T) -> requiredEngines

  if(!all(requiredEngines$isSetup)){
    stop(paste0("All engines required for the pipelines have not been configured. ",
                "Please use the analysisPipelines::assessEngine() function to check"))
  }
  pipelineRegistryJoin <- dplyr::left_join(object@pipeline, object@registry, by = c("operation" = "functionName"))

  if(nrow(pipelineRegistryJoin) > 0){
    for(rowNo in 1:nrow(pipelineRegistryJoin)){

      ## Check outAsIn and engine conversion accordingly
      if(pipelineRegistryJoin[['outAsIn.x']][rowNo] == T && rowNo > 1){
          inputToExecute <- object@output[[rowNo-1]]
      }else{
          inputToExecute <- object@input
      }
      object@output[[rowNo]] <- do.call(pipelineRegistryJoin[['operation']][[rowNo]],
                                        append(list(inputToExecute),
                                               pipelineRegistryJoin[['parameters']][[rowNo]]))
    }
  }else{
    stop("No functions have been added to the pipeline")
  }

  return(object)
}

