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
    ##Check input class

    .Object@input <- initDfBasedOnType(input, filePath)
    .Object@originalSchemaDf <- .Object@input[0,]


    ## Calling the parent constructor
    .Object <- methods::callNextMethod(.Object, ...)

    for(rowNo in 1:nrow(batchPredefFunctions)){
      .Object %>>% registerFunction(batchPredefFunctions[['functionName']][[rowNo]],
                                    batchPredefFunctions[['heading']][[rowNo]],
                                    batchPredefFunctions[['outAsIn']][[rowNo]],
                                    batchPredefFunctions[['engine']][[rowNo]],
                                    batchPredefFunctions[['exceptionHandlingFunction']][[rowNo]],
                                    userDefined = F) -> .Object
    }
  return(.Object)
  }
)

#' @name .checkSchemaMatch
#' @title Function to check schema of input of an \code{AnalysisPipeline} object with a new dataset
#' @keywords internal
.checkSchemaMatch = function(object, newData){
  schemaCheck <- checkSchema(object@originalSchemaDf, newData)
  return(schemaCheck)
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

#' @name .generateOutput
#' @title Implementation of output generation for \code{AnalysisPipeline} objects
#' @keywords internal
.generateOutput = function(object)
{
  inputToExecute <- object@input

  if(all(dim(inputToExecute) == c(0,0))){
    stop("This pipeline has not been initialized with a dataframe. Please use the setInput() function to do so.")
  }

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

      #Check engine

      if(rowNo > 1){
        prevEngine <- pipelineRegistryJoin[["engine"]][rowNo - 1]
        currEngine <- pipelineRegistryJoin[["engine"]][rowNo]
      }else{
        prevEngine <- currEngine <-  pipelineRegistryJoin[["engine"]][rowNo]
      }


      ## Check outAsIn and engine conversion accordingly
      if(pipelineRegistryJoin[['outAsIn.x']][rowNo] == T && rowNo > 1){
        #object@workingInput <- do.call(object@pipeline[['operation']][[rowNo]], append(list(input), object@pipeline[['parameters']][[rowNo]]))
        if(prevEngine != currEngine){

          if(prevEngine == 'spark'){

            if(currEngine == 'r'){
              inputToExecute <- SparkR::as.data.frame(object@output[[rowNo-1]])
            }

          }else if(prevEngine == 'r'){
            if(currEngine == "spark"){
              inputToExecute <- SparkR::as.DataFrame(object@output[[rowNo-1]])
            }

          }
        }else{
          inputToExecute <- object@output[[rowNo-1]]
        }
      }else{

        if(currEngine == 'r'){
          inputToExecute <- object@input
        }else if(currEngine == 'spark'){
          inputToExecute <- SparkR::as.DataFrame(object@input)
        }
      }
      object@output[[rowNo]] <- tryCatch({do.call(pipelineRegistryJoin[['operation']][[rowNo]],
                                                  append(list(inputToExecute),
                                                         pipelineRegistryJoin[['parameters']][[rowNo]]))},
                                         error = function(e){
                                           do.call(pipelineRegistryJoin[['exceptionHandlingFunction']][[rowNo]],
                                                   list(error = e))
                                         })
    }
  }else{
    stop("No functions have been added to the pipeline")
  }

  return(object)
}

#' @rdname generateOutput
setMethod(
  f = "generateOutput",
  signature = "AnalysisPipeline",
  definition = .generateOutput
)


setMethod(
  f = "generateOutput",
  signature = "tbl",
  definition = function(object)
  {
    input <- input
    outList <- list()
    for(rowNo in 1:nrow(object)){
      if(object[['outAsIn']][rowNo] == T){
        input <- do.call(object[['operation']][[rowNo]], append(list(input), object[['parameters']][[rowNo]]))
      }
      outList[[rowNo]] <- do.call(object[['operation']][[rowNo]], append(list(input), object[['parameters']][[rowNo]]))
    }
    return(outList)
  }
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
  definition = function(object,path)
  {
    require(rmarkdown)
    if(length(object@output) == 0){
      object <- generateOutput(object)
    }
    object <- updateObject(object, "emptyRow", "emptyRow",list("emptyRow"),F)


    rmarkdown::render(
      system.file("report.Rmd", package = "analysisPipelines"),
      params = list(
        input = object@input,
        pipeline = object@pipeline,
        output = object@output
      ),
      html_document(
        css = system.file("styles.css", package = "analysisPipelines"),
        toc = T,
        toc_float = T
      ),

      output_dir = path ,
      output_file = paste('EDA_report_',Sys.time(),'.html', sep = '')
    )

  }
)
