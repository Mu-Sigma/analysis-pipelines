##################################################################################################
# Title: Reusable pipelines for streaming analyses
# Author: Naren Srinivasan
# Created on: July 12, 2018
# Description: An R package version - Currently supports Apache Spark Structured Streaming
##################################################################################################

# TO DO
# - Add schema checks
# - Add ability to initialized without input and check for generate output if there is not input initialized
# - Remove workingInput - DONE
# - Test loadPipeline function

#' @include core-functions.R
NULL

#' @name StreamingAnalysisPipeline-class
#' @rdname StreamingAnalysisPipeline-class
#' @title Class for constructing Analysis Pipelines for streaming analyeses
#' @details Inherits the base class \link{BaseAnalysisPipeline} class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself
#' @details This class currently only supports Apache Spark Structured Streaming, implemented through the SparkR interface
#' @slot input The input Spark DataFrame on which analysis is to be performed
#' @slot originalSchemaDf Empty Spark DataFrame representing the schema of the input
#' @family Package core functions for Streaming Analyses
#' @include core-functions.R
#' @exportClass StreamingAnalysisPipeline
#' @export StreamingAnalysisPipeline


StreamingAnalysisPipeline <- setClass("StreamingAnalysisPipeline",
                           slots = c(
                             input = "ANY",
                             #Should be a SparkDataFrame, but unable to specify as SparkR is not distributed on CRAN
                             originalSchemaDf = "ANY"
                           ), contains = "BaseAnalysisPipeline")

#' StreamingAnalysisPipeline constructor
#' @docType methods
#' @rdname initialize-methods
#' @title Constructor for the \code{StreamingAnalysisPipeline} object
#' @include core-functions.R
#' @keywords internal

setMethod(
  f = "initialize",
  signature = "StreamingAnalysisPipeline",
  definition = function(.Object,input)
  {
    .Object@input <- input

    ## Calling the parent constructor
    .Object <- methods::callNextMethod(.Object)
    return(.Object)
  }
)

.checkSparkDataFrame <- function(obj){
  if(class(obj) != "SparkDataFrame"){
    futile.logger::flog.error("||  The input should be of class 'SparkDataFrame' from the 'SparkR' package  ||",
                              name = "logger.base")
    stop()
  }
}

.executeStream<- function(object){

  tryCatch({

    futile.logger::flog.info("||  Pipeline Execution STARTED  ||" , name='logger.execution')

    outputCache <- .getCache()

    topOrder <- object@pipelineExecutor$topologicalOrdering
    dplyr::left_join(object@pipeline, getRegistry(), by = c("operation" = "functionName")) %>>%
      dplyr::left_join(object@pipelineExecutor$topologicalOrdering, by = c("id" = "id")) -> pipelineRegistryOrderingJoin

    batches <- unique(pipelineRegistryOrderingJoin$level)
    numBatches <- max(as.numeric(batches))


    # Iterate across batches i.e. sets of independent functions
    lapply(batches, function(x, object, pipelineRegistryOrderingJoin, outputCache){

      pipelineRegistryOrderingJoin %>>% dplyr::filter(.data$level == x) -> functionsInBatch

      ## Function execution in a stream
      lapply(functionsInBatch$id, function(y, object, functionsInBatch, outputCache){

        functionsInBatch %>>% dplyr::filter(.data$id == y) %>>% as.list -> funcDetails

        futile.logger::flog.info("||  Function ID '%s' named '%s' STARTED on the '%s' engine ||",
                                 funcDetails$id, funcDetails$operation, funcDetails$engine,
                                 name='logger.func')


        # Set parameters

        params <- unlist(funcDetails$parameters, recursive = F)
        dep <- unique(unlist(funcDetails$dependencies, recursive = F))
        depTerms <- paste0("f", dep)

        # Datasets passed as a formula are updated here

        params <- lapply(params, function(p, depTerms, outputCache){
          if(class(p) == "formula"){
            isDepParam <- analysisPipelines::isDependencyParam(p)
            if(isDepParam){
              formulaTerm <- analysisPipelines::getTerm(p)
              argName <-  analysisPipelines::getResponse(p)
              if(formulaTerm %in% depTerms){

                ## Formula of previous function in pipeline
                actualParamObjectName <- paste0(formulaTerm, ".out")
                p <-  get(actualParamObjectName, envir = outputCache)
              }
            }
          }

          return(p)
        }, depTerms, outputCache)

        # No type conversion for Streaming pipelines

        if(funcDetails$isDataFunction){
          # Not passed as a formula
          if(any(class(params[[1]]) == "rlang_fake_data_pronoun")){
            # Checking for outAsIn
            if(funcDetails$outAsIn && funcDetails$id  != "1"){
              dataOpFn <- paste0("f", as.numeric(funcDetails$id) - 1)
              actualDataObjectName <- paste0(dataOpFn, ".out")
              params[[1]] <- get(actualDataObjectName, envir = outputCache)
            }else{
              # On original input
              params[[1]]<- object@input
            }
          }
        }

        #Call
        startFunc <- Sys.time()
        args <- params
        output <- tryCatch({do.call(what = funcDetails$operation,
                                    args = args)},
                           error = function(e){
                             futile.logger::flog.error("||  ERROR Occurred in Function ID '%s' named '%s'. EXITING PIPELINE EXECUTION. Calling Exception Function - '%s'  ||",
                                                       funcDetails$id, funcDetails$operation, funcDetails$exceptionHandlingFunction,
                                                       name='logger.func')
                             do.call(funcDetails$exceptionHandlingFunction,
                                     list(error = e))

                           })

        endFunc <- Sys.time()
        funcExecTime <- endFunc - startFunc

        opName <- paste0("f", funcDetails$id, ".out") #eg: f1.out
        if(funcDetails$storeOutput){
          assign(opName, value = output, envir = outputCache)
        }else{
          #Check if there are dependent children
          fromList <- object@pipelineExecutor$dependencyLinks$from
          if(funcDetails$id %in% fromList){
            assign(opName, value = output, envir = outputCache)
          }
        }


        futile.logger::flog.info("||  NEW MICRO_BATCH PROCESSED for Function ID '%s' named '%s' in %s seconds  ||",
                                 funcDetails$id, funcDetails$operation, funcExecTime,
                                 name='logger.func')

      }, object, functionsInBatch, outputCache)

    }, object, pipelineRegistryOrderingJoin, outputCache)

    object@output <- mget(ls(outputCache), envir = outputCache)
    rm(list = ls(outputCache), envir = outputCache)

    return(object)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}

.generateStreamingOutput <- function(object){
  tryCatch({

    object %>>% initializeLoggers

    inputToExecute <- object@input

    if(class(inputToExecute) != "SparkDataFrame"){
      m <- "This streaming pipeline has not been initialized with a SparkDataFrame. Please use the setInput() function to do so."
      futile.logger::flog.error(m, name = 'logger.pipeline')
      stop(m)
    }

    ## Check engine setup
    object %>>% assessEngineSetUp ->  engineAssessment
    engineAssessment %>>% dplyr::filter(.data$requiredForPipeline == T) -> requiredEngines

    if(!all(requiredEngines$isSetup)){
      m <- paste0("All engines required for the pipelines have not been configured. ",
                  "Please use the analysisPipelines::assessEngine() function to check")
      futile.logger::flog.error(m, name = 'logger.engine.assessment')
      stop(m)
    }

    if(nrow(object@pipelineExecutor$topologicalOrdering) == 0){
      object %>>% prepExecution -> object
    }

    object %>>% .executeStream -> object

    return(object)

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}

#' @rdname generateOutput
setMethod(
  f = "generateOutput",
  signature = "StreamingAnalysisPipeline",
  definition = .generateStreamingOutput
)
