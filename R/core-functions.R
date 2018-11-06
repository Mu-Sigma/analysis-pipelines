##################################################################################################
# Title: Reusable pipelines for generating analyses outputs and reports
# Version: 18.09.01
# Created on: July 12, 2018
# Description: An R package version which works both on R data frames, and a Spark environment i.e.
#              Spark DataFrames including Structured Streaming
##################################################################################################
#' @name BaseAnalysisPipeline
#' @title Base class for \code{AnalysisPipeline} and \code{StreamingAnalysisPipeline} objects
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself, and serves
#' as the base class for various types of Pipeline objects such as Batch and Streaming.
#' @details This base class which contains the slots related to the registry, pipeline and output can be extended
#' to create custom class for specific scenarios if required.
#' @details The details of the constructor for this class can be found at \link{initializeBaseAnalysisPipeline}
#' @details In the documentation, objects of classes which are subclasses of this class are referred to as 'Pipeline' objects
#' @slot pipeline A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions
#' @exportClass BaseAnalysisPipeline
#' @export BaseAnalysisPipeline

BaseAnalysisPipeline <- setClass("BaseAnalysisPipeline",
                             slots = c(
                               pipeline = "tbl",
                               registry = "tbl",
                               pipelineExecutor = "list",
                               output = "list"
                             ))

#' @name initializeBaseAnalysisPipeline
#' @title This is the constructor for the \link{BaseAnalysisPipeline} class
#' @param .Object The \code{BaseAnalysisPipeline} object
#' @details
#'      This is a constructor function for the base class for various types of Analysis Pipelines. This method gets
#'      internally called by \code{AnalysisPipeline} and \code{StreamingAnalysisPipeline} constructors.
#' @return an object of class "\code{BaseAnalysisPipeline}"
#' @family Package core functions
#' @export

setMethod(
  f = "initialize",
  signature = "BaseAnalysisPipeline",
  definition = function(.Object)
  {
    .Object@pipeline <- tibble(
      id = character(),
      operation = character(),
      heading = character(),
      parameters = list(),
      outAsIn = logical(),
      storeOutput = F
    )

    .Object@pipelineExecutor <- list(
      topologicalOrdering = tibble(id = character(),
                                   level = character()),
      dependencyLinks = tibble(from = character(),
                               to = character())
      # parallelizationDetails = list()
      # outputStorageDetails = tibble(id = numeric(),
      #                                isRequiredUntilLevel = character(),
      #                                isToBeStoredPermanently = logical()),
      # cache = list()
    )

    .Object@registry <- tibble(
      ##id required?
      functionName = character(),
      heading = character(),
      outAsIn = logical(),
      engine = character(),
      exceptionHandlingFunction = character(),
      userDefined = logical()


    )
    .Object@output <- list()

    return(.Object)
  }
)

#' @name registerFunction
#' @title Register a user-defined function to be used with a \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details
#'       The specified operation along with the heading and parameters is updated in the pipeline slot
#'       of the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object, where the sequence of operations to be performed is stored
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param outAsIn whether to use original input or output from previous function
#' @param engine specifies which engine the function is to be run on. Available engines include "r", "spark", and "python"
#' @param loadPipeline logical parameter to see if function is being used in loadPipeline or not
#' @param session to load shiny session in the function
#' @return Updated \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @family Package core functions
#' @export

setGeneric(
  name = "registerFunction",
  def = function(object, functionName,  heading = "", outAsIn = F,
                 engine = "r", #options are 'r', 'spark', 'python'
                 exceptionFunction = substitute(genericPipelineException),
                 loadPipeline = F, userDefined = T,
                 storeOutput = F, session = session)
  {
    standardGeneric("registerFunction")
  }
)

setMethod(
  f = "registerFunction",
  signature = "BaseAnalysisPipeline",
  definition = function(object, functionName, heading = "",
                        # outAsIn = F,
                        engine = "r",
                        exceptionFunction = as.character(substitute(genericPipelineException)),
                        loadPipeline = F, userDefined = T
                        # storeOutput = F,
                        # session = session
                        )
  {

    #Define data frame class according to engine type
    childClass <- class(object)
    attr(childClass, "package") <- NULL

    dataFrameClass <- "data.frame"
    if(engine == "spark"){
      dataFrameClass <- "SparkDataFrame"
    }

    ##Assigning the original function to a variable
    # assign("origFunction", get(x = functionName,
                                  # envir = environment()),
           # envir = environment())


    parametersName <- names(as.list(args(eval(parse(text=functionName)))))
    parametersName <- paste0(parametersName[c(-1,-length(parametersName))],collapse=",")
    if(parametersName != ""){
      parametersName <- paste0(", ", parametersName)
    }
    methodBody <- paste0(capture.output(body(eval(parse(text=functionName)))),collapse="\n")

    originalArgs <- names(as.list(args(eval(parse(text=functionName)))))
    firstArg <- originalArgs[1]

    methodBody <- gsub(pattern = "\\{", replacement = paste0("{", firstArg , " = object"), x = methodBody)

    # originalArgsString <- paste0(originalArgs[c(-1,-length(originalArgs))],collapse=",")

    methodArg <- paste0(capture.output(args(eval(parse(text=functionName)))),collapse="")
    methodArg <- strsplit(strsplit(methodArg,firstArg)[[1]][2],"NULL")[[1]][1]


    ##Assigning the exception function to the global Environment
    assign(exceptionFunction, get(x = exceptionFunction,
                                                envir = environment()),
           envir = globalenv())

    #Register function
    registerFunText <- # Generic
                            paste0('setGeneric(name = "', functionName,'",',
                                  'signature = "object",',
                                 'def = function(object ', ', ... ', ', outAsIn = F, storeOutput = F)',
                                 # 'def = function(object, ',firstArg, ', ...)',
                                 'standardGeneric("', functionName,'"));',

                        # Adding to pipeline when run on a Analysis Pipeline object
                                 'setMethod(f = "', functionName,'",',
                                            'signature = "', childClass, '",',
                                            'definition = function(object',
                                                                  parametersName, ',',
                                                                  'outAsIn, storeOutput){',
                                             'parametersList <- unlist(strsplit(x = "', sub(", ", "", parametersName), '", split = ","' ,'));',
                                             'parametersPassed <- lapply(parametersList, function(x){eval(parse(text = x))});',
                                             'return(updateObject(object,
                                                                  operation = "', functionName, '",',
                                                                  'heading = "', heading, '",',
                                                                  'parameters = parametersPassed, outAsIn = outAsIn, storeOutput = storeOutput));});',

                        #Executing the actual function when data is passed
                                'setMethod(f = "',functionName,'",',
                                 'signature = "', dataFrameClass, '",',
                                 'definition = function(object ', parametersName,')',
                                                    methodBody, ')'
    )

    eval(parse(text = registerFunText), envir=.GlobalEnv)


    if(loadPipeline==F){
      object@registry %>>% add_row(functionName = paste0(functionName),
                                   heading = heading,
                                   # outAsIn = outAsIn,
                                   engine = engine,
                                   exceptionHandlingFunction = exceptionFunction,
                                   userDefined = userDefined) -> object@registry
    }
    return(object)
  }
)


#' @name setInput
#' @title Sets the input for an \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details
#'      Assigns the input to the pipeline for an  \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @param input the input data frame
#' @param filePath path to the file which needs to be read (currently supports .csv files)
#' @return Updated \code{AnalysisPipeline} \code{StreamingAnalysisPipeline} object
#' @family Package core functions
#' @export
setGeneric(
  name = "setInput",
  def = function(object,
                input,
                filePath = "")
  {
    standardGeneric("setInput")
  }
)

.setInput = function(object, input, filePath = "")
{
  input <- initDfBasedOnType(input, filePath)
  object@input <- input
  return(object)
}

setMethod(
  f = "setInput",
  signature = "BaseAnalysisPipeline",
  definition = .setInput
)

#' @name updateObject
#' @title Update the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object by adding an operation to the pipeline
#' @details
#'       The specified operation along with the heading and parameters is updated in the pipeline slot
#'       of the  \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object, where the sequence of operations
#'      to be performed is stored
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @param operation function name to be updated in tibble
#' @param heading heading of that section in report
#' @param parameters parameters passed to that function
#' @param outAsIn whether to use original input or output from previous function
#' @param storeOutput whether the output of this operation is to be stored
#' @return Updated \code{AnalysisPipeline} \code{StreamingAnalysisPipeline} object
#' @family Package core functions
#' @export
setGeneric(
  name = "updateObject",
  def = function(object,
                 operation,
                 heading = "",
                 parameters,
                 outAsIn = F,
                 storeOutput = F)
  {
    standardGeneric("updateObject")
  }
)

.updateObject = function(object, operation, heading = "", parameters, outAsIn = F,  storeOutput = F)
{
  if(nrow(object@pipeline) == 0){
    id = 1
  }else{
    id = max(as.numeric(object@pipeline$id)) + 1
  }
  object@pipeline %>>% add_row(id = id,
                             operation = operation,
                             heading = heading,
                             parameters = list(parameters),
                             outAsIn = outAsIn,
                             storeOutput = storeOutput) -> object@pipeline
  return(object)
}

setMethod(
  f = "updateObject",
  signature = "BaseAnalysisPipeline",
  definition = .updateObject
)

#' @name assessEngineSetUp
#' @title Assesses engine (R, Spark, Python, Spark Structured Streaming) set up
#' @details
#'       Assesses whether engines required for executing functions in an \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline}
#'       object have been set up
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @return Tibble containing the details of available engines, whether they are required for a recipe, a logical reporting
#'         whether the engine has been set up, and comments.
#' @family Package core functions
#' @export

setGeneric(
  name = "assessEngineSetUp",
  def = function(object)
  {
    standardGeneric("assessEngineSetUp")
  }
)

.assessEngineSetUp = function(object)
{
  startEngineAssessment <- Sys.time()
  flog.info("||  Engine Assessment for pipeline STARTED  ||" , name='logger.engine')

  engineAssessment <- tibble(engine = character(),
                             requiredForPipeline = logical(),
                             isSetup = logical(),
                             comments = character())
  if(nrow(object@pipeline) == 0){
    stop("No functions have been added to the pipeline")
  }else{
    pipelineRegistryJoin = dplyr::left_join(object@pipeline, object@registry, by = c("operation" = "functionName"))
    requiredEngines <- unique(pipelineRegistryJoin$engine)

    # R
    isRSetup <- T
    rComments <- ""
    engineAssessment %>>% dplyr::add_row(engine = "r",
                                         requiredForPipeline = ifelse("r" %in% requiredEngines, T, F),
                                         isSetup = isRSetup,
                                         comments = rComments)           -> engineAssessment

    #Spark Batch and Structured Streaming

    isSparkSetup <- T
    sparkComments <- ""
    checkSession <- tryCatch(SparkR::sparkR.conf(), error = function(e) e)
    if("SparkSession not initialized" %in% checkSession){
      isSparkSetup <- F
      sparkComments <- paste0("There does not seem to be a Spark Session initialized through SparkR ",
                              "which is required to execute pipelines containing Spark functions. ",
                              "Please initialize a SparkR session. The analysisPipelines::sparkRSessionCreateIfNotPresent() ",
                              "helper function can be used.")
    }else{
      sparkComments <- paste0("SESSION DETAILS : ", paste0(checkSession, collapse = " "))
    }

    engineAssessment %>>% dplyr::add_row(engine = "spark",
                                         requiredForPipeline = ifelse("spark" %in% requiredEngines, T, F),
                                         isSetup = isSparkSetup,
                                         comments = sparkComments)           -> engineAssessment

    engineAssessment %>>% dplyr::add_row(engine = "spark-structured-streaming",
                                         requiredForPipeline = ifelse("spark-structured-streaming" %in% requiredEngines, T, F),
                                         isSetup = isSparkSetup,
                                         comments = sparkComments)           -> engineAssessment

    #TO DO -  Python

  }


  endEngineAssessment <- Sys.time()
  engineAssessmentTime <- endEngineAssessment - startEngineAssessment
  flog.info("||  Engine Assessment COMPLETE. Time taken : %s seconds||", engineAssessmentTime, name='logger.engine')

  return(engineAssessment)

}


setMethod(
  f = "assessEngineSetUp",
  signature = "BaseAnalysisPipeline",
  definition = .assessEngineSetUp
)

#' @name savePipeline
#' @title Saves the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object to the file system without outputs
#' @details
#'       The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object is saved to the file system in the paths specified
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @param path the path at which the .Rda file containing the pipeline should be stored, along with the name of the file including
#' a .Rda extension
#' @return Does not return a value
#' @family Package core functions
#' @export

setGeneric(
  name = "savePipeline",
  def = function(object, path)
  {
    standardGeneric("savePipeline")
  }
)

.savePipeline = function(object, path){
  object@output <- list()
  object@input <- data.frame()
  listToBeSaved <- c("object", object@registry$functionName, object@registry$exceptionHandlingFunction)
  save(list = listToBeSaved,file = path)
}

setMethod(
  f = "savePipeline",
  signature = "BaseAnalysisPipeline",
  definition = .savePipeline
)


#' @name getPipeline
#' @title Obtain the pipeline
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details
#'      Obtains the pipeline from the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object as a tibble
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @return Tibble describing the pipeline
#' @family Package core functions
#' @export

setGeneric(
  name = "getPipeline",
  def = function(object)
  {
    standardGeneric("getPipeline")
  }
)

.getPipeline = function(object){
  return(object@pipeline)
}

setMethod(
  f = "getPipeline",
  signature = "BaseAnalysisPipeline",
  definition = .getPipeline
)


#' @name getRegistry
#' @title Obtains the function registry
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline}  object
#' @details
#'      Obtains the function registry from the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object as a tibble,
#'      including both predefined and user defined functions
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @return Tibble describing the registry
#' @family Package core functions
#' @export

setGeneric(
  name = "getRegistry",
  def = function(object)
  {
    standardGeneric("getRegistry")
  }
)

.getRegistry = function(object){
  return(object@registry)
}

setMethod(
  f = "getRegistry",
  signature = "BaseAnalysisPipeline",
  definition = .getRegistry
)

#' @name getInput
#' @title Obtains the initializedInput
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details
#'      Obtains the input from the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @return Dataframe for an \code{AnalysisPipeline} & SparkDataFrame for a \code{StreamingAnalysisPipeline}
#' @family Package core functions
#' @export

setGeneric(
  name = "getInput",
  def = function(object)
  {
    standardGeneric("getInput")
  }
)

.getInput = function(object){
  return(object@input)
}

setMethod(
  f = "getInput",
  signature = "BaseAnalysisPipeline",
  definition = .getInput
)

#' @name getOutputById
#' @title Obtains a specific output
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @param id The position of the function for which the output is desired in the sequence of operations in the pipeline.
#' @param includeCall Logical which defines whether the call used to generate the output should be returned. By, default this is false
#' @details
#'      Obtains a specific output from the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object by passing the position
#'      of the function for which the output is desired, in the sequence of operations in the pipeline. This can be obtained by passing the number
#'      under the 'id' column in the pipeline table corresponding to the required function
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @return If includeCall = F, the output object generated by the function is returned
#' @return If includeCall = T, it is a list containing to elements
#'         - call: tibble with 1 row containing the function call for the output desired
#'         - output: output generated
#' @family Package core functions
#' @export

setGeneric(
  name = "getOutputById",
  def = function(object, id, includeCall = F)
  {
    standardGeneric("getOutputById")
  }
)

.getOutputById = function(object, id, includeCall = F){
  op <- list(call = data.frame(),
             output = list())
  object@pipeline %>% dplyr::filter(id == id) -> call
  unlist(object@output[[paste0("f", id, ".out")]], recursive = F) -> output

  if(includeCall){
    op <- list(call = call,
               output = output)
    return(op)
  }else{
    return(output)
  }

}

setMethod(
  f = "getOutputById",
  signature = "BaseAnalysisPipeline",
  definition = .getOutputById
)

#' @name visualizePipeline
#' @title Visualizes the pipeline as a graph, showing dependencies
#' @details
#' @param
#' @family Package core functions
#' @export

setGeneric(
  name = "visualizePipeline",
  def = function(object)
  {
    standardGeneric("visualizePipeline")
  }
)

.visualizePipeline <- function(object){

  if(nrow(object@pipelineExecutor$dependencyLinks) == 0){
    object %>>% prepExecution -> object
  }

  node_df <- object@pipeline
  node_df %>>% dplyr::mutate(group = ifelse(storeOutput == T, "Stored output", "Auxiliary step")) %>>%
    dplyr::select(id, operation, group) -> node_df
  colnames(node_df) <- c("id", "label", "group")

  edge_df <- object@pipelineExecutor$dependencyLinks


  lnodes <- data.frame(label = c("Stored output", "Auxiliary step"),
                       shape = c("dot"), color = c("#A1AEFF","#ff4d4d"),
                       title = "Pipeline")


  vis <-visNetwork(node_df, edge_df) %>%
    visGroups(groupname = "Stored output", color = "#A1AEFF", shadow=T) %>%
    visGroups(groupname = "Auxiliary step", color = "#ff4d4d", shadow=T) %>%
    visLegend(addNodes = lnodes, position = "right", ncol = 3,
              zoom = F, useGroups = F)%>%
    visNodes(font = list(size =18)) %>%
    visEdges(arrows=list(to=list(enabled = T, scaleFactor = 0.25)),
             widthConstraint = 1.2,
             length = c(3)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 6, improvedLayout = T) %>%
    # to stop auto update and making nodes sticky
    visPhysics(enabled = F) %>%
    visInteraction(navigationButtons = TRUE)

  return(vis)
}

setMethod(
  f = "visualizePipeline",
  signature = "BaseAnalysisPipeline",
  definition = .visualizePipeline
)

########### Changing generics ############################################

#' @rdname prepExecution
#' @name prepExecution
#' @title
#' @details
#' @family Package core functions
#' @exportMethod prepExecution

setGeneric(
  name = "prepExecution",
  def = function(object)
  {
    standardGeneric("prepExecution")
  }
)

#' @rdname generateOutput
#' @name generateOutput
#' @title Generate a list of outputs from Pipeline objects
#' @details \code{generateOutput} is a generic function that is implemented for various types of pipeline objects
#' such as \code{AnalysisPipeline} and \code{StreamingAnalysisPipeline}
#' @details
#'       The sequence of operations stored in the pipeline object
#'       are run and outputs generated, stored in a list
#' @param object object that contains input, pipeline, registry and output
#' @return Updated Pipeline object with the outputs at each step stored in the \code{output} slot.
#' @return Specific outputs can be obtained by using the \link{getOuputByOrderId} function
#' @family Package core functions
#' @include core-functions.R
#' @exportMethod generateOutput

setGeneric(
  name = "generateOutput",
  def = function(object)
  {
    standardGeneric("generateOutput")
  }
)

#' @rdname checkSchemaMatch
#' @name checkSchemaMatch
#' @title Checks the schema of the input to a Pipeline object against the original
#' @param object A Pipeline object
#' @param newData The newData that the pipeline is to be initialized with
#' @details Checks the schema of the new data frame that the pipeline is to be initialized with against
#'          the original schema that the pipeline was saved with. Provides a detailed comparison
#' @return Returns a list with details on added columns, removed columns, comparison between column classes, and a logical
#'         whether the schema has remained the same from the old dataframe to the new one
#' @family Package core functions for batch/one-time analyses
#' @exportMethod checkSchemaMatch
setGeneric(
  name = "checkSchemaMatch",
  def = function(object, newData)
  {
    standardGeneric("checkSchemaMatch")
  }
)

######## Auxiliary functions ############################################

#' @name genericPipelineException
#' @title Default exception for pipeline functions
#' @details This functions defines the default function which will be called in case of an exception occurring while
#' executing any of the pipeline functions. While a function is registered, a custom function to deal with exceptions
#' incurred during the call of the function being registered can be passed by the user. If passed, the custom function
#' will be called instead of this function
#' @param error Error encountered during the execution of a particular pipeline function
#' @family Package core functions
#' @export
genericPipelineException <- function(error){
  message <- error$message
  print("generic exception")
  stop(paste0("EXCEPTION OCCURED WHILE RUNNING THE PIPELINE FUNCTION WITH PROVIDED PARAMETERS: ", message))
}

#' @name loadPipeline
#' @title Loads the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object from the file system
#' @details
#'       The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object is loaded into the file system from the file system
#'       based on the path specified.
#' @details Optionally, the \code{input} parameter can be provided to
#'       initialize the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object with an R data frame
#'       or Streaming Spark DataFrame (in case of \code{StreamingAnalysisPipeline} object) present in the R session.
#' @details Another provided option, is to specify a filePath where the input dataset is present (in a .CSV format)
#'       and the object will be initialized with this data frame. The \code{filePath} parameter takes precedence over
#'       \code{input} parameter. This is applicable only from \code{AnalysisPipeline} objects
#' @param path the path at which the .Rda file containing the pipeline is located
#' @param input (optional) data frame with which the pipeline object should be initialized
#' @param filePath (optional) path where a dataset in .CSV format is present which is to be loaded
#' @return An \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object, optinally initialized with the data frame provided
#' @family Package core functions
#' @export

loadPipeline <- function(path, input = data.frame() , filePath = ""){

  load(path, envir = environment())
  functionNames = setdiff(ls(envir = environment()), c("RDSPath", "object", "input", "filePath"))

  lapply(functionNames, function(x){
    assign(x, get(x, environment()), globalenv())
  })

  input <- initDfBasedOnType(input, filePath)
  schemaCheck <- object %>>% checkSchemaMatch(input)
  if(!schemaCheck$isSchemaSame){
    if(length(schemaCheck$removedColumns) > 0){
      warning(paste0("Some columns which were present in the original schema ",
                      "for the pipeline, ",
                      "are not present in the new data frame. Some pipeline functions ",
                      "may not execute as expected. Use the checkSchemaMatch function to obtain ",
                      "a detailed comparison"))
    }

    if(length(schemaCheck$addedColumns) > 0){
      warning(paste0("Some new columns have been added to the new data frame ",
                       "as compared to the original schema for the pipeline. ",
                     "Use the checkSchemaMatch function to obtain ",
                      "a detailed comparison"))
    }

    if(length(schemaCheck$addedColumns) == 0 && length(schemaCheck$removedColumns) == 0){
      warning(paste0("Colummn names are the same but types have changed",
                     "Some pipeline functions may not execute as expected. ",
                     "Use the checkSchemaMatch function to obtain ",
                      "a detailed comparison"))
    }

  }

  object@input <- input
  return(object)
}


#' @name initDfBasedOnType
#' @title initializes the \code{AnalysisPipeline} object with the input based on the provided type
#' @details
#'      Transforms provided inputs into R data frame regardless of the input provided, be it Spark DataFrames
#'      or Python data frames
#' @param input Input dataframe
#' @param filePath File path where the .csv file is stored
#' @return \code{AnalysisPipeline} object initialized with input
#' @family Package core functions
#' @keywords internal

initDfBasedOnType <- function(input, filePath){

  if(filePath == ""){
    if(!all(dim(input) == c(0,0))){
      #Check for R, Spark, Python data frame
      if(class(input) == "SparkDataFrame"){
        input <- SparkR::as.data.frame(input)
      }else if(class(input) == "data.frame" || class(input) == "tibble"){
        #do nothing for R
      }else{
        stop("The provided input is not of class - data.frame or SparkDataFrame")
      }
    }
  }
  else{
    input <- read.csv(filePath)
  }

  return(input)
}


#' @name checkPipelineCompatibilityWithNewData
#' @title Checks whether the pipeline is compatible new dataset
#' @details
#'      Internally compares the schema of the new dataset with the original dataset with which the pipeline
#'      was initialized. Additionally, it checks whether all functions in the pipeline are compatible with the new
#'      dataset.
#' @param pipeline An \code{AnalysisPipeline} object
#' @param dfToBeChecked Dataframe to be checked for compatibility
#' @return logical value (T or R) specifying whether the new dataset is compatible with the pipeline
#' @family Package core functions
#' @keywords internal
#'

#' @name updatePackageRegistry
#' @title Updates the package registry
#' @details
#'       Updates the registry of predefined functions available in the package (For developer use)
#' @param functionName the name of the function
#' @param functionHeader the header caption that will feature in the report for this function's output
#' @param flag a boolean which dictates if the 'functionName' function returns a data.frame to be used an input
#' @return An \code{AnalysisPipeline} object, optinally initialized with the data frame provided
#' @family Package core functions
#' @keywords internal
#'
#' ####TODO - This function needs rework to comply with R package structures and mechanisms. Currently, the name of
#' the function needs to be manually added in the data-raw
#'
# updatePackageRegistry <- function(functionName, functionHeader, flag){
#   tryCatch({
#     functionsDefined <- readRDS("support/predefFunctions.RDS")
#     invisible(source("EDA.R"))
#     functionList <- ls(envir = .GlobalEnv)
#     if(functionName %in% functionList){
#       functionsDefined <- add_row(functionsDefined, functionName = functionName, heading = functionHeader, outAsIn = flag)
#       print(functionsDefined)
#       saveRDS(functionsDefined, "support/predefFunctions.RDS")
#       print("Successfully Registered function into package!")
#     }else
#       print(paste0("Failed to register function into package. Could not find function '", functionName, "' in the environment."))
#   }, error = function(e){
#     stop(e)
#   }, warning = function(e){
#     warning(e)
#   })
# }




