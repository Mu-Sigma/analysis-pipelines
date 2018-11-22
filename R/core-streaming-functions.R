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
  definition = function(.Object,input)
  {
    .Object@input <- input

    ## Calling the parent constructor
    .Object <- methods::callNextMethod(.Object)
    return(.Object)
  }
)


.executeStream<- function(object){

  tryCatch({

    futile.logger::flog.info("||  Pipeline Execution STARTED  ||" , name='logger.execution')

    outputCache <- .getCache()

    topOrder <- object@pipelineExecutor$topologicalOrdering
    dplyr::left_join(object@pipeline, getRegistry(), by = c("operation" = "functionName")) %>>%
      dplyr::left_join(object@pipelineExecutor$topologicalOrdering, by = c("id" = "id")) -> pipelineRegistryOrderingJoin

    # pipelineRegistryJoin <- dplyr::left_join(object@pipeline, getRegistry(), by = c("operation" = "functionName"))
    batches <- unique(pipelineRegistryOrderingJoin$level)
    numBatches <- max(as.numeric(batches))


    # Iterate across batches i.e. sets of independent functions
    lapply(batches, function(x, object, pipelineRegistryOrderingJoin, outputCache){

      # startBatch <- Sys.time()
      pipelineRegistryOrderingJoin %>>% dplyr::filter(level == x) -> functionsInBatch
      # futile.logger::flog.info("||  Executing Batch Number : %s/%s containing functions '%s' ||",
                               # x, numBatches, paste(functionsInBatch$operation, collapse = ", "),
                               # name='logger.batch')

      ## Function execution in a stream
      lapply(functionsInBatch$id, function(y, object, functionsInBatch, outputCache){

        functionsInBatch %>>% dplyr::filter(id == y) %>>% as.list -> funcDetails

        futile.logger::flog.info("||  Function ID '%s' named '%s' STARTED on the '%s' engine ||",
                                 funcDetails$id, funcDetails$operation, funcDetails$engine,
                                 name='logger.func')

        # Set Input data
        inputToExecute <- object@input


        if(funcDetails$outAsIn && funcDetails$id  != "1"){
          dataOpFn <- paste0("f", as.numeric(funcDetails$id) - 1)
          actualDataObjectName <- paste0(dataOpFn, ".out")
          inputToExecute <-  get(actualDataObjectName, envir = outputCache)
        }

        # Set parameters

        params <- unlist(funcDetails$parameters, recursive = F)
        dep <- unique(unlist(funcDetails$dependencies, recursive = F))
        depTerms <- paste0("f", dep)

        params <- lapply(params, function(p, depTerms, outputCache){
          if(class(p) == "formula"){
            isDepParam <- analysisPipelines:::isDependencyParam(p)
            if(isDepParam){
              formulaTerm <- analysisPipelines:::getTerm(p)
              argName <-  analysisPipelines:::getResponse(p)
              if(formulaTerm %in% depTerms){

                ## Formula of previous function in pipeline
                actualParamObjectName <- paste0(formulaTerm, ".out")
                p <-  get(actualParamObjectName, envir = outputCache)
              }
            }
          }

          return(p)
        }, depTerms, outputCache)


        #Call
        startFunc <- Sys.time()
        #Assign as named parameters
        #Get names of params
        # paramNames <- lapply(params, function(p){
        #   return(names(p))
        # })  %>>% unlist
        # params <-lapply(params, function(p){
        #   names(p) <- NULL
        #   return(p)
        # })
        # names(params) <- paramNames
        args <- params
        if(funcDetails$isDataFunction){
          formals(funcDetails$operation) %>>% as.list %>>% names %>>% dplyr::first() -> firstArgName
          firstArg <- list(inputToExecute)
          names(firstArg) <- firstArgName
          args <- append(firstArg, params)
        }
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

        # ##outAsIn
        # if(funcDetails$outAsIn){
        #   outputCache$workingInput <- output
        # }

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
    engineAssessment %>>% dplyr::filter(requiredForPipeline == T) -> requiredEngines

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

# .generateStreamingOutput = function(object)
# {
#   inputToExecute <- object@input
#
#   ## Check engine setup
#   object %>>% assessEngineSetUp ->  engineAssessment
#   engineAssessment %>>% dplyr::filter(requiredForPipeline == T) -> requiredEngines
#
#   if(!all(requiredEngines$isSetup)){
#     stop(paste0("All engines required for the pipelines have not been configured. ",
#                 "Please use the analysisPipelines::assessEngine() function to check"))
#   }
#   pipelineRegistryJoin <- dplyr::left_join(object@pipeline, getRegistry(), by = c("operation" = "functionName"))
#
#   if(nrow(pipelineRegistryJoin) > 0){
#     for(rowNo in 1:nrow(pipelineRegistryJoin)){
#
#       ## Check outAsIn and engine conversion accordingly
#       if(pipelineRegistryJoin[['outAsIn']][rowNo] == T && rowNo > 1){
#           inputToExecute <- object@output[[rowNo-1]]
#       }else{
#           inputToExecute <- object@input
#       }
#       object@output[[rowNo]] <- do.call(pipelineRegistryJoin[['operation']][[rowNo]],
#                                         append(list(inputToExecute),
#                                                pipelineRegistryJoin[['parameters']][[rowNo]]))
#     }
#   }else{
#     stop("No functions have been added to the pipeline")
#   }
#
#   return(object)
# }



