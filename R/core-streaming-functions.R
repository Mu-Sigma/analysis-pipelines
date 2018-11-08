##################################################################################################
# Title: Reusable pipelines for streaming analyses
# Version: 18.09.01
# Created on: July 12, 2018
# Description: An R package version - Currently supports Apache Spark Structured Streaming
##################################################################################################

# TO DO
# - Add schema checks
# - Add ability to initialized without input and check for generate output if there is not input initialized
# - Remove workingInput - DONE
# - Test loadPipeline function

#' @include core-functions.R


#' @name StreamingAnalysisPipeline
#' @title Class for constructing Analysis Pipelines for streaming analyeses
#' @details Inherits the base class \link{BaseAnalysisPipeline} class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself
#' @details This class currently only supports Apache Spark Structured Streaming, implemented through the SparkR interface
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initializeStreamingAnalysisPipeline}
#' @slot input The input Spark DataFrame on which analysis is to be performed
#' @slot originalSchemaDf Empty Spark DataFrame representing the schema of the input
#' @family Package core functions for Streaming Analyses
#' @include core-functions.R
#' @exportClass StreamingAnalysisPipeline
#' @export StreamingAnalysisPipeline

StreamingAnalysisPipeline <- setClass("StreamingAnalysisPipeline",
                           slots = c(
                             input = "SparkDataFrame",
                             originalSchemaDf = "SparkDataFrame"
                           ), contains = "BaseAnalysisPipeline")

#' @name initializeStreamingAnalysisPipeline
#' @title Constructor for the \code{StreamingAnalysisPipeline} object
#' @param .Object The \code{StreamingAnalysisPipeline} object
#' @param input The Spark DataFrame on which operations need to be performed
#' @details
#'      \code{input} needs to be provded and the argument needs to be of class \code{SparkDataFrame}, which is
#'      generally created through operations using SparkR
#' @return an object of class "\code{StreamingAnalysisPipeline}", initialized with the input Spark DataFrame provided
#' @family Package core functions for Streaming Analyses
#' @include core-functions.R
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

#' @rdname generateOutput
setMethod(
  f = "generateOutput",
  signature = "StreamingAnalysisPipeline",
  definition = .generateStreamingOutput
)

