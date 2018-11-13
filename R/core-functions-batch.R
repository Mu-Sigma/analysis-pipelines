##################################################################################################
# Title: Reusable pipelines for batch/one-time analyses
# Version: 18.09.01
# Created on: July 12, 2018
# Description: An R package version - Currently supports R and Spark
##################################################################################################

#' @name AnalysisPipeline
#' @title Class for constructing Analysis Pipelines for batch/ one-time analyeses
#' @details Inherits the base class \link{BaseAnalysisPipeline} class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself
#' @details Additionally, this class is meant to be used for batch/ one-time processing. Contains additional slots to
#' hold the data frame to be used for the pipeline and associated schema
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initializeAnalysisPipeline}
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

#' @name initializeAnalysisPipeline
#' @title Constructor for the \link{AnalysisPipeline} class
#' @param .Object The \code{AnalysisPipeline} object
#' @param input The data frame on which operations need to be performed
#' @param filePath File path for a .csv file to directly read in the dataset from
#' @details
#'      Either one of \code{input} or \code{filePath} need to be provided i.e. either the
#'      data frame or the file path to a csv file
#' @return an object of class \code{AnalysisPipeline}, initialized with the input data frame provided
#' @include core-functions.R
#' @family Package core functions for batch/one-time analyses
#' @export

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

      for(rowNo in 1:nrow(batchPredefFunctions)){
        .Object %>>% registerFunction(functionName = batchPredefFunctions[['functionName']][[rowNo]],
                                      heading =  batchPredefFunctions[['heading']][[rowNo]],
                                      # batchPredefFunctions[['outAsIn']][[rowNo]],
                                      engine = batchPredefFunctions[['engine']][[rowNo]],
                                      exceptionFunction = batchPredefFunctions[['exceptionHandlingFunction']][[rowNo]],
                                      userDefined = F, loadPipeline = F ) -> .Object
      }
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

    outputCache <- new.env()

    topOrder <- object@pipelineExecutor$topologicalOrdering
    dplyr::left_join(object@pipeline, object@registry, by = c("operation" = "functionName")) %>>%
      dplyr::left_join(object@pipelineExecutor$topologicalOrdering, by = c("id" = "id")) -> pipelineRegistryOrderingJoin

    # Set Input data and set type to engine with max. number of operations

    pipelineRegistryOrderingJoin %>>% dplyr::group_by(engine) %>>% dplyr::summarise(numOp = dplyr::n()) -> engineCount

    engineCount %>>% dplyr::filter(numOp == max(numOp)) -> maxEngine


    maxEngineName <- "r"
    if(nrow(maxEngine) == 1){
      maxEngineName <- maxEngine$engine
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
      # TODO: convert to Python data frame
    }

    batches <- unique(pipelineRegistryOrderingJoin$level)
    numBatches <- max(as.numeric(batches))

    #   no_cores <- parallel::detectCores() - 1
    #   cl <- parallel::makeCluster(no_cores)
    #   parallel::clusterExport(cl = cl, envir = .GlobalEnv, varlist = ls())
    # .executeBatch <- function(y, functionsInBatch){
    #
    #   return(NULL)
    # }



    # Iterate across batches
    lapply(batches, function(x, object, pipelineRegistryOrderingJoin, outputCache){

      startBatch <- Sys.time()
      pipelineRegistryOrderingJoin %>>% dplyr::filter(level == x) -> functionsInBatch
      futile.logger::flog.info("||  Executing Batch Number : %s/%s containing functions '%s' ||",
                               x, numBatches, paste(functionsInBatch$operation, collapse = ", "),
                               name='logger.batch')

      # Garbage cleaning in the cache - Previous batch outputs

      prevBatch <- as.character(as.numeric(x) - 1)
      pipelineRegistryOrderingJoin %>>% dplyr::filter(level == prevBatch) -> funcInPrevBatch

      if(nrow(funcInPrevBatch)>0){
        possiblePrevCacheOutputNames <- paste0("f", funcInPrevBatch$id, ".out")
        previousCachedOutputNames <- intersect(possiblePrevCacheOutputNames, ls(outputCache))
        pipelineRegistryOrderingJoin %>>% dplyr::filter(storeOutput == TRUE) -> requiredOutputs
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

        functionsInBatch %>>% dplyr::filter(id == y) %>>% as.list -> funcDetails

        futile.logger::flog.info("||  Function ID '%s' named '%s' STARTED on the '%s' engine ||",
                                 funcDetails$id, funcDetails$operation, funcDetails$engine,
                                 name='logger.func')


        if(funcDetails$isDataFunction){
          if(funcDetails$outAsIn && funcDetails$id  != "1"){
            dataOpFn <- paste0("f", as.numeric(funcDetails$id) - 1)
            actualDataObjectName <- paste0(dataOpFn, ".out")
            inputToExecute <-  get(actualDataObjectName, envir = outputCache)

          }

          #Check engine
          ###TODO: Python to be added

          currEngine <- funcDetails$engine

          prevEngine <- ifelse(class(inputToExecute) == "SparkDataFrame", 'spark',
                               ifelse(class(inputToExecute) == "data.frame" || class(inputToExecute) == "tibble",
                                      'r', 'python'))
          #Check engine
          ###TODO: Python to be added

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
                #TODO: python
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
              }else if(prevEngine == 'python'){
                # TODO: python
              }
            }else if(prevEngine == 'python'){
              #TODO: python
            }
          }
        }


        # Set parameters

        params <- unlist(funcDetails$parameters, recursive = F)
        dep <- unlist(funcDetails$dependencies, recursive = F)
        depTerms <- paste0("f", dep)

        params <- lapply(params, function(p, depTerms, outputCache){
          if(class(p) == "formula"){
            formulaTerm <- attr(terms(p), "term.label")
            if(length(formulaTerm) == 1 && formulaTerm %in% depTerms){

              ## Formula of previous function in pipeline
              actualParamObjectName <- paste0(formulaTerm, ".out")
              p <-  get(actualParamObjectName, envir = outputCache)
            }
          }

          return(p)
        }, depTerms, outputCache)


        #Call

        #Assign as named parameters
        args <- params
        if(funcDetails$isDataFunction){
          args <- append(list(inputToExecute), params)
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

        ##outAsIn
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

        endFunc <- Sys.time()
        funcExecTime <- endFunc - startFunc

        futile.logger::flog.info("||  Function ID '%s' named '%s' COMPLETED. Time taken : %s seconds  ||", funcDetails$id, funcDetails$operation, funcExecTime,
                                 name='logger.func')

      }, object, functionsInBatch, outputCache)

      # parallel::parLapply(X = functionsInBatch$id, cl = cl, function(y){s })
      endBatch <- Sys.time()
      batchExecTime <- endBatch - startBatch

      futile.logger::flog.info("||  Batch Number %s/%s COMPLETE. Time taken : %s seconds  ||", x, numBatches, batchExecTime, name='logger.batch')

    }, object, pipelineRegistryOrderingJoin, outputCache)


    futile.logger::flog.info("||  Performing final garbage cleaning and collection of outputs  ||",
                             name='logger.pipeline')

    #Final garbage cleaning
    pipelineRegistryOrderingJoin %>>% dplyr::filter(storeOutput == TRUE) -> requiredOutputs
    requiredOutputs <-  paste0("f", requiredOutputs$id, ".out")

    unrequiredCachedOutputNames <- setdiff(ls(outputCache), requiredOutputs)

    lapply(unrequiredCachedOutputNames, function(n){
      rm(list = n, envir = outputCache)
      return(NULL)
    })


    object@output <- mget(ls(outputCache), envir = outputCache)
    rm(list = ls(outputCache), envir = outputCache)

    # stopCluster(cl)
    # object@output <- object@pipelineExecutor$cache
    #
    # #Clear cache
    # object@pipelineExecutor$cache <- NULL

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

      opEngineDetails <- object@pipeline %>>% dplyr::filter(storeOutput == T)
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
