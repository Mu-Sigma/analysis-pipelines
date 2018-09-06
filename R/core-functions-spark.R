##################################################################################################
# Title: Reusable pipelines for generating analysis reports
# Version: 18.08.01
# Created on: July 12, 2018
# Description: An R package version
##################################################################################################

#' @name readInputSpark
#' @title Function to initialize \code{SparkAnalysisPipeline} class with the input Spark DataFrame
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initialize}
#' @slot input The input Spark DataFrame on which analysis is to be performed
#' @slot workingInput Internal slot for having a working version of the input
#' @slot pipeline A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions for Spark
#' @export
readInputSpark <- setClass("SparkAnalysisPipeline",
                           slots = c(
                             input = "SparkDataFrame",
                             workingInput = "SparkDataFrame",
                             pipeline = "tbl",
                             registry = "tbl",
                             output = "list"
                           ))

#' @name initialize
#' @title Constructor for the \code{SparkAnalysisPipeline} object
#' @param .Object The \code{SparkAnalysisPipeline} object
#' @param input The Spark DataFrame on which operations need to be performed
#' @details
#'      \code{input} needs to be provded and the argument needs to be of class \code{SparkDataFrame}, which is
#'      generally created through operations using SparkR
#' @return an object of class "\code{SparkAnalysisPipeline}", initialized with the input Spark DataFrame provided
#' @family Package core functions for Spark
#' @export
setMethod(
  f = "initialize",
  signature = "SparkAnalysisPipeline",
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
      userDefined = logical()

    )

    for(rowNo in 1:nrow(sparkPredefFunctions)){
      .Object %>>% registerFunctionSpark(sparkPredefFunctions[['functionName']][[rowNo]],
                                         sparkPredefFunctions[['heading']][[rowNo]],
                                         sparkPredefFunctions[['outAsIn']][[rowNo]],
                                         userDefined = F) -> .Object
    }
    return(.Object)
  }
)


#' @name registerFunctionSpark
#' @title Register a user-defined function to be used with \code{SparkAnalysisPipeline} objects specifically for Spark
#'        DataFrames
#' @details
#'       The specified operation along with the heading and parameters is updated in the pipeline slot
#'       of the \code{SparkAnalysisPipeline} object, where the sequence of operations to be performed is stored. This function
#'       is used to register functions which are designed to operate on Spark DataFrames
#' @param object object that contains input, pipeline, registry and output
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param outAsIn whether to use original input or output from previous function
#' @param loadPipeline logical parameter to see if function is being used in loadPipeline or not
#' @param session to load shiny session in the function (Currently not implemented)
#' @return Updated \code{SparkAnalysisPipeline} object
#' @family Package core functions for Spark
#' @export

setGeneric(
  name = "registerFunctionSpark",
  def = function(object, functionName,  heading ="", outAsIn=F, loadPipeline=F,
                 userDefined = T, session=session)
  {
    standardGeneric("registerFunctionSpark")
  }
)

setMethod(
  f = "registerFunctionSpark",
  signature = "SparkAnalysisPipeline",
  definition = function(object, functionName,  heading ="", outAsIn=F, loadPipeline=F,
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
                              signature = \"SparkAnalysisPipeline\",
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
                                   userDefined = userDefined) -> object@registry
    }
    return(object)
  }
)


#'### TODO
#'  @name loadPipelineSpark
#' @title Loads the \code{SparkAnalysisPipeline} object from the file system
#' @details
#'       The \code{SparkAnalysisPipeline} object is loaded into the file system from the file system
#'       based on the path specified.
#' @param RDSPath the path at which the .RDS file containing the pipeline is located
#' @param input Spark DataFrame with which the pipeline object should be initialized
#' @return An \code{SparkAnalysisPipeline} object, optinally initialized with the data frame provided
#' @family Package core functions for Spark
#' @export
loadPipelineSpark <- function(RDSPath, input){
  object <- readRDS(RDSPath)
    object@input <- input
  registeredFunctions <- object@registry
  for(rowNo in 1:nrow(registeredFunctions)){
    object %>>% registerFunctionSpark(registeredFunctions[['functionName']][[rowNo]],
                                      registeredFunctions[['heading']][[rowNo]],
                                      registeredFunctions[['outAsIn']][[rowNo]],
                                      userDefined =  registeredFunctions[['userDefined']][[rowNo]],
                                      loadPipeline = T) -> object
  }
  return(object)
}
