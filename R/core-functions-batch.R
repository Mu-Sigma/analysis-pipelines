##################################################################################################
# Title: Reusable pipelines for batch/one-time analyses
# Author: Naren Srinivasan
# Created on: July 12, 2018
# Description: An R package version - Currently supports R and Spark
##################################################################################################

#' @importFrom magrittr %>%
NULL

#' @name AnalysisPipeline-class
#' @rdname AnalysisPipeline-class
#' @title Class for constructing Analysis Pipelines for batch/ one-time analyeses
#' @details Inherits the base class \link{BaseAnalysisPipeline} class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself
#' @details Additionally, this class is meant to be used for batch/ one-time processing. Contains additional slots to
#' hold the data frame to be used for the pipeline and associated schema
#' @slot input The input dataset on which analysis is to be performed
#' @slot originalSchemaDf Empty data frame representing the schema of the input
#' @family Package core functions for batch/one-time analyses
#' @include core-functions.R
#' @exportClass AnalysisPipeline
#' @export AnalysisPipeline

AnalysisPipeline <- setClass("AnalysisPipeline",
                             slots = c(
                               input = "data.frame",
                               originalSchemaDf = "data.frame"
                             ), contains = "BaseAnalysisPipeline")

#' AnalysisPipeline constructor
#' @docType methods
#' @rdname initialize-methods
#' @title Constructor for the \link{AnalysisPipeline} class
#' @include core-functions.R
#' @family Package core functions for batch/one-time analyses
#' @keywords internal

setMethod(
  f = "initialize",
  signature = "AnalysisPipeline",
  definition = function(.Object, ...,input = data.frame(), filePath = "")
  {
    tryCatch({
      ##Check input class

      .Object@input <- initDfBasedOnType(input, filePath)
      .Object@originalSchemaDf <- .Object@input[0,]


      ## Calling the parent constructor
      .Object <- methods::callNextMethod(.Object, ...)

      return(.Object)

    },error = function(e){
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    })

  }
)


.checkSchemaMatch = function(object, newData){
  tryCatch({
    schemaCheck <- checkSchema(object@originalSchemaDf, newData)
    return(schemaCheck)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}

#' @rdname checkSchemaMatch
setMethod(
  f = "checkSchemaMatch",
  signature = "AnalysisPipeline",
  definition = .checkSchemaMatch
)


#' @name checkSchema
#' @title Compare the schemas of two dataframes
#' @details
#'       Compares the schemas of two dataframes, providing information on added and removed columns in the new dataframe
#'       as compared to the old
#' @param dfOld Old dataframe
#' @param dfNew New dataframe
#' @return Returns a list with details on added columns, removed columns, comparison between column classes, and a logical
#'         whether the schema has remained the same from the old dataframe to the new one
#' @family Package core functions for batch/one-time analyses
#' @keywords internal

checkSchema <- function(dfOld, dfNew){

  schemaCheck <- list(addedColumns = list(),
                      removedColumns = list(),
                      colComparison = list(),
                      isSchemaSame = logical())

  colNamesOld <- colnames(dfOld)
  colNamesNew <- colnames(dfNew)

  addedColumns <- setdiff(colNamesNew, colNamesOld)
  removedColumns <- setdiff(colNamesOld, colNamesNew)
  isSchemaSame <- F

  oldColComparison <- purrr::map(colNamesOld, function(x, dfOld, dfNew, colNamesNew, addedColumns, removedColumns){
    colComparison <- list(colName = character(),
                          oldClass = character(),
                          newClass = character(),
                          colType = character(),
                          hasClassChanged = logical()
    )

    if(!((x %in% addedColumns) || (x %in% removedColumns))){
      newColIndex <- match(x, colNamesNew)
      colName <- x
      oldClass <- class(dfOld[[colName]])
      newClass <- class(dfNew[[colName]])
      colType <- "common"
      hasClassChanged <- ifelse(oldClass == newClass, F, T)
    }else if(x %in% removedColumns){
      colName <- x
      oldClass <- class(dfOld[[colName]])
      newClass <- NULL
      colType <- "removed"
      hasClassChanged <- NULL
    }

    colComparison <- list(colName = colName,
                          oldClass = oldClass,
                          newClass = newClass,
                          colType = colType,
                          hasClassChanged = hasClassChanged
    )
    return(colComparison)

  }, dfOld, dfNew, colNamesNew, addedColumns, removedColumns)

  addedCols <- purrr::map(addedColumns, function(x, dfNew){
    colComparison <- list(colName = character(),
                          oldClass = character(),
                          newClass = character(),
                          colType = character(),
                          hasClassChanged = logical()
    )

    colName <- x
    oldClass <- NULL
    newClass <- class(dfNew[[colName]])
    colType <- "added"
    hasClassChanged <- NULL

    colComparison <- list(colName = colName,
                          oldClass = oldClass,
                          newClass = newClass,
                          colType = colType,
                          hasClassChanged = hasClassChanged
    )

    return(colComparison)

  }, dfNew)

  colComparison <- append(oldColComparison, addedCols)

  if(length(addedColumns)  == 0 && length(removedColumns) == 0){
    if(all(unlist(purrr::map(colComparison, function(x){
      ##Returning true if class hasn't changed
      return(!x$hasClassChanged)
    })))){
      isSchemaSame <- T
    }
  }

  schemaCheck <- list(addedColumns = addedColumns,
                      removedColumns = removedColumns,
                      colComparison = colComparison,
                      isSchemaSame = isSchemaSame
  )
  return(schemaCheck)
}

.generateOutput = function(object)
{
  tryCatch({

    object %>>% initializeLoggers

    inputToExecute <- object@input

    if(all(dim(inputToExecute) == c(0,0))){
      m <- "This pipeline has not been initialized with a dataframe. Please use the setInput() function to do so."
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

    object %>>% .executeByBatch -> object

    return(object)

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}


.executeByBatch <- function(object){

  tryCatch({


    startPipelineExecution <- Sys.time()
    futile.logger::flog.info("||  Pipeline Execution STARTED  ||" , name='logger.execution')

    maxEngineName <- "r"
    outputCache <- .getCache()

    topOrder <- object@pipelineExecutor$topologicalOrdering
    dplyr::left_join(object@pipeline, getRegistry(), by = c("operation" = "functionName")) %>>%
      dplyr::left_join(object@pipelineExecutor$topologicalOrdering, by = c("id" = "id")) -> pipelineRegistryOrderingJoin

    # Set Input data and set type to engine with max. number of operations

    pipelineRegistryOrderingJoin %>>% dplyr::group_by(.data$engine) %>>% dplyr::summarise(numOp = dplyr::n()) -> engineCount

    engineCount %>>% dplyr::filter(.data$numOp == max(.data$numOp)) -> maxEngine


    if(nrow(maxEngine) == 1){
      maxEngineName <- maxEngine$engine
    }else{
      maxEngineName <- maxEngine$engine[1]
    }

    inputToExecute <- object@input

    if(maxEngineName == "spark"){

      startTypeConv <- Sys.time()

      inputToExecute <- SparkR::as.DataFrame(object@input)

      endTypeConv <- Sys.time()
      typeConvTime <- endTypeConv - startTypeConv
      futile.logger::flog.info(paste("||  Initial Type conversion from R dataframe to SparkDataFrame,",
                                "as maximum number of operations are on the Spark engine.",
                                "Time taked : %s seconds  ||"),
                                 typeConvTime,
                                 name='logger.func')

    }else if(maxEngineName == "python"){
      startTypeConv <- Sys.time()

      inputToExecute <- reticulate::r_to_py(object@input)

      endTypeConv <- Sys.time()
      typeConvTime <- endTypeConv - startTypeConv
      futile.logger::flog.info(paste("||  Initial Type conversion from R dataframe to Pandas DataFrame,",
                                     "as maximum number of operations are on the Python engine.",
                                     "Time taked : %s seconds  ||"),
                               typeConvTime,
                               name='logger.func')
    }

    batches <- unique(pipelineRegistryOrderingJoin$level)
    numBatches <- max(as.numeric(batches))

    # Iterate across batches
    lapply(batches, function(x, object, pipelineRegistryOrderingJoin, outputCache){

      startBatch <- Sys.time()
      pipelineRegistryOrderingJoin %>>% dplyr::filter(.data$level == x) -> functionsInBatch
      futile.logger::flog.info("||  Executing Batch Number : %s/%s containing functions '%s' ||",
                               x, numBatches, paste(functionsInBatch$operation, collapse = ", "),
                               name='logger.batch')

      # Garbage cleaning in the cache - Previous batch outputs

      prevBatch <- as.character(as.numeric(x) - 1)
      pipelineRegistryOrderingJoin %>>% dplyr::filter(.data$level == prevBatch) -> funcInPrevBatch

      if(nrow(funcInPrevBatch)>0){
        possiblePrevCacheOutputNames <- paste0("f", funcInPrevBatch$id, ".out")
        previousCachedOutputNames <- intersect(possiblePrevCacheOutputNames, ls(outputCache))
        pipelineRegistryOrderingJoin %>>% dplyr::filter(.data$storeOutput == TRUE) -> requiredOutputs
        requiredOutputs <-  paste0("f", requiredOutputs$id, ".out")

        unrequiredCachedOutputNames <- setdiff(possiblePrevCacheOutputNames,
                                               unique(c(previousCachedOutputNames, requiredOutputs)))

        lapply(unrequiredCachedOutputNames, function(n){
          rm(list = n, envir = outputCache)
          return(NULL)
        })

        futile.logger::flog.info("||  Cleared intermediate outputs which are not required  ||", name = "logger.pipeline")

      }


      ## Function execution in a batch
      lapply(functionsInBatch$id, function(y, object, functionsInBatch, outputCache){

        ## Check atleast one function to execute
        ## Replace formula parameters with actual outputs
        startFunc <- Sys.time()

        functionsInBatch %>>% dplyr::filter(.data$id == y) %>>% as.list -> funcDetails

        futile.logger::flog.info("||  Function ID '%s' named '%s' STARTED on the '%s' engine ||",
                                 funcDetails$id, funcDetails$operation, funcDetails$engine,
                                 name='logger.func')

        # Set parameters

        params <- unlist(funcDetails$parameters, recursive = F)
        dep <- unique(unlist(funcDetails$dependencies, recursive = F))
        depTerms <- paste0("f", dep)

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

        if(funcDetails$isDataFunction){
          # Not passed as a formula
          if(any(class(params[[1]]) == "rlang_fake_data_pronoun")){
            # Checking for outAsIn
            if(funcDetails$outAsIn && funcDetails$id  != "1"){
              dataOpFn <- paste0("f", as.numeric(funcDetails$id) - 1)
              actualDataObjectName <- paste0(dataOpFn, ".out")
              inputToExecute <-  get(actualDataObjectName, envir = outputCache)
            }
          }else if(any(class(params[[1]]) %in% c("pandas.core.frame.DataFrame", "data.frame","SparkDataFrame"))){
            inputToExecute <- params[[1]]
          }
        }



          currEngine <- funcDetails$engine

          prevEngine <- ifelse(any(class(inputToExecute) == "SparkDataFrame"), 'spark',
                               ifelse(any(class(inputToExecute) == "data.frame") ||
                                        any(class(inputToExecute) == "tibble"),
                                      'r', ifelse(any(class(inputToExecute) == "pandas.core.frame.DataFrame"),
                                                  'python',
                                                  'r')))
          #Check engine
          if(prevEngine != currEngine){
            if(prevEngine == 'spark'){

              if(currEngine == 'r'){
                startTypeConv <- Sys.time()

                inputToExecute <- SparkR::as.data.frame(inputToExecute)

                endTypeConv <- Sys.time()
                typeConvTime <- endTypeConv - startTypeConv
                futile.logger::flog.info("||  Type conversion from Spark DataFrame to R dataframe took %s seconds  ||",
                                         typeConvTime,
                                         name='logger.func')
              }else if(currEngine == 'python'){
                startTypeConv <- Sys.time()

                inputToExecute <- SparkR::as.data.frame(inputToExecute) %>>% reticulate::r_to_py()

                endTypeConv <- Sys.time()
                typeConvTime <- endTypeConv - startTypeConv
                futile.logger::flog.info("||  Type conversion from Spark DataFrame to Pandas DataFrame took %s seconds  ||",
                                         typeConvTime,
                                         name='logger.func')
              }

            }else if(prevEngine == 'r'){
              if(currEngine == "spark"){
                startTypeConv <- Sys.time()

                inputToExecute <- SparkR::as.DataFrame(inputToExecute)

                endTypeConv <- Sys.time()
                typeConvTime <- endTypeConv - startTypeConv
                futile.logger::flog.info("||  Type conversion from R dataframe to Spark DataFrame took %s seconds  ||",
                                         typeConvTime,
                                         name='logger.func')
              }else if(currEngine == 'python'){
                startTypeConv <- Sys.time()

                inputToExecute <- reticulate::r_to_py(inputToExecute)

                endTypeConv <- Sys.time()
                typeConvTime <- endTypeConv - startTypeConv
                futile.logger::flog.info("||  Type conversion from Pandas DataFrame to R dataframe took %s seconds  ||",
                                         typeConvTime,
                                         name='logger.func')
              }
            }else if(prevEngine == 'python'){
              if(currEngine == "spark"){
                startTypeConv <- Sys.time()

                inputToExecute <- reticulate::py_to_r(inputToExecute) %>>% SparkR::as.DataFrame()

                endTypeConv <- Sys.time()
                typeConvTime <- endTypeConv - startTypeConv
                futile.logger::flog.info("||  Type conversion from Pandas DataFrame to Spark DataFrame took %s seconds  ||",
                                         typeConvTime,
                                         name='logger.func')
              }else if(currEngine == 'r'){
                startTypeConv <- Sys.time()

                inputToExecute <- reticulate::py_to_r(inputToExecute)

                endTypeConv <- Sys.time()
                typeConvTime <- endTypeConv - startTypeConv
                futile.logger::flog.info("||  Type conversion from Pandas DataFrame to R dataframe took %s seconds  ||",
                                         typeConvTime,
                                         name='logger.func')
              }
            }
          }


        #Setting converted dataframe for first parameter of data function
        if(funcDetails$isDataFunction){
            inputToExecute -> params[[1]]
        }

        #Call
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

        endFunc <- Sys.time()
        funcExecTime <- endFunc - startFunc

        futile.logger::flog.info("||  Function ID '%s' named '%s' COMPLETED. Time taken : %s seconds  ||", funcDetails$id, funcDetails$operation, funcExecTime,
                                 name='logger.func')

      }, object, functionsInBatch, outputCache)

      # parallel::parLapply(X = functionsInBatch$id, cl = cl, function(y){s })
      endBatch <- Sys.time()
      batchExecTime <- endBatch - startBatch

      futile.logger::flog.info("||  Batch Number %s/%s COMPLETE. Time taken : %s seconds  ||", x, numBatches, batchExecTime, name='logger.batch')

    }, object,
    pipelineRegistryOrderingJoin, outputCache)


    futile.logger::flog.info("||  Performing final garbage cleaning and collection of outputs  ||",
                             name='logger.pipeline')

    #Final garbage cleaning
    pipelineRegistryOrderingJoin %>>% dplyr::filter(.data$storeOutput == TRUE) -> requiredOutputs
    requiredOutputs <-  paste0("f", requiredOutputs$id, ".out")

    unrequiredCachedOutputNames <- setdiff(ls(outputCache), requiredOutputs)

    lapply(unrequiredCachedOutputNames, function(n){
      rm(list = n, envir = outputCache)
      return(NULL)
    })


    object@output <- mget(ls(outputCache), envir = outputCache)
    rm(list = ls(outputCache), envir = outputCache)

    endPipelineExecution <- Sys.time()
    executionTime <- endPipelineExecution - startPipelineExecution

    futile.logger::flog.info("||  Pipeline Execution COMPLETE. Time taken : %s seconds||", executionTime, name='logger.execution')
    return(object)



  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })


}

#' @rdname generateOutput
setMethod(
  f = "generateOutput",
  signature = "AnalysisPipeline",
  definition = .generateOutput
)


#' @name generateReport
#' @rdname generateReport
#' @title Generate a HTML report from an \code{AnalysisPipeline} object
#' @details
#'       The sequence of operations stored in the \code{AnalysisPipeline} object are run, outputs generated,
#'       and a HTML report is generated with outputs in the same sequence as the pipeline created by the user
#' @param object object that contains input, pipeline, registry and output
#' @param path path on the file system, where the generated html report should be stored
#' @return Updated \code{AnalysisPipeline} object
#' @family Package core functions for batch/one-time analyses
#' @export

setGeneric(
  name = "generateReport",
  def = function(object,path)
  {
    standardGeneric("generateReport")
  }
)

#' @rdname generateReport
setMethod(
  f = "generateReport",
  signature = c("AnalysisPipeline", "character"),
  definition = function(object, path = ".")
  {
    tryCatch({

      object %>>% setLoggerDetails(target = "file") -> object

      if(length(object@output) == 0){
        object <- generateOutput(object)
      }
      # object <- updateObject(object, "emptyRow", "emptyRow",list("emptyRow"),F)

      opEngineDetails <- dplyr::left_join(object@pipeline %>>% dplyr::filter(.data$storeOutput == T),
                                          getRegistry(), by = c("operation" = "functionName"))
      if(!all(unique(opEngineDetails$engine) == 'r')){
        futile.logger::flog.warn(paste("||  Pipeline contains engines other than R.",
                              "Will attempt coercing of outputs for rendinring through 'rmarkdown'.",
                              "This may throw errors  ||"), name='logger.execution')

      }

      rmarkdown::render(
        system.file("report.Rmd", package = "analysisPipelines"),
        params = list(
          # input = object@input,
          # pipelineDetails = object@pipeline,
          # output = object@output,
          obj = object
        ),
        rmarkdown::html_document(
          css = system.file("styles.css", package = "analysisPipelines"),
          toc = T,
          toc_float = T
        ),

        output_dir = path ,
        output_file = paste('analysisPipelineReport_',Sys.time(),'.html', sep = '')
      )

    },error = function(e){
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    })
  }
)
