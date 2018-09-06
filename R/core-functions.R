##################################################################################################
# Title: Reusable pipelines for generating analyses outputs and reports
# Version: 18.08.01
# Created on: July 12, 2018
# Description: An R package version which works both on R data frames, and a Spark environment i.e.
#              Spark DataFrames including Structured Streaming
##################################################################################################

#' @name readInput
#' @title Function to initialize \code{AnalysisPipeline} class with the input data frame
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initialize}
#' @slot input The input dataset on which analysis is to be performed
#' @slot workingInput Internal slot for having a working version of the input
#' @slot filePath Path of the input dataset to be uploaded
#' @slot pipeline A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions
#' @export
readInput <- setClass("AnalysisPipeline",
                       slots = c(
                         input = "data.frame",
                         filePath = "character",
                         workingInput = "data.frame",
                         pipeline = "tbl",
                         registry = "tbl",
                         output = "list"
                       ))

#' @name initialize
#' @title Constructor for the \code{AnalysisPipeline} object
#' @param .Object The \code{AnalysisPipeline} object
#' @param input The data frame on which operations need to be performed
#' @param filePath File path for a .csv file to directly read in the dataset from
#' @details
#'      Either one of \code{input} or \code{filePath} need to be provided i.e. either the
#'      data frame or the file path to a csv file
#' @return an object of class "\code{AnalysisPipeline}", initialized with the input data frame provided
#' @family Package core functions
setMethod(
  f = "initialize",
  signature = "AnalysisPipeline",
  definition = function(.Object, input = data.frame(), filePath = "", workingInput = data.frame())
  {
    if(filePath == ""){
      .Object@input <- input
    }
    else{
      .Object@input <- read.csv(filePath)
    }
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
    .Object@output <- list()

    for(rowNo in 1:nrow(dfPredefFunctions)){
      .Object %>>% registerFunction(dfPredefFunctions[['functionName']][[rowNo]],
                                    dfPredefFunctions[['heading']][[rowNo]],
                                    dfPredefFunctions[['outAsIn']][[rowNo]],
                                    userDefined = F) -> .Object
    }
    return(.Object)
  }
)

#' @name updateObject
#' @title Update the \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object by adding an operation to the pipeline
#' @details
#'       The specified operation along with the heading and parameters is updated in the pipeline slot
#'       of the  \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object, where the sequence of operations
#'      to be performed is stored
#' @param object object that contains input, pipeline, registry and output
#' @param operation function name to be updated in tibble
#' @param heading heading of that section in report
#' @param parameters parameters passed to that function
#' @param outAsIn whether to use original input or output from previous function
#' @return Updated \code{AnalysisPipeline} object
#' @family Package core functions
#' @export
setGeneric(
  name = "updateObject",
  def = function(object,
                 operation,
                 heading = "",
                 parameters,
                 outAsIn = F)
  {
    standardGeneric("updateObject")
  }
)

.updateObject = function(object, operation, heading="", parameters, outAsIn = F)
{
  if(nrow(object@pipeline) == 0){
    order = 1
  }else{
    order = max(object@pipeline$order) + 1
  }
  object@pipeline %>>% add_row(order = order,
                             operation = operation,
                             heading = heading,
                             parameters = list(parameters),
                             outAsIn = outAsIn) -> object@pipeline
  return(object)
}

setMethod(
  f = "updateObject",
  signature = "AnalysisPipeline",
  definition = .updateObject
)

setMethod(
  f = "updateObject",
  signature = "SparkAnalysisPipeline",
  definition = .updateObject
)

#' @name registerFunction
#' @title Register a user-defined function to be used with \code{AnalysisPipeline} objects
#' @details
#'       The specified operation along with the heading and parameters is updated in the pipeline slot
#'       of the AnalysisPipeline object, where the sequence of operations to be performed is stored
#' @param object object that contains input, pipeline, registry and output
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param outAsIn whether to use original input or output from previous function
#' @param loadPipeline logical parameter to see if function is being used in loadPipeline or not
#' @param session to load shiny session in the function
#' @return Updated \code{AnalysisPipeline} object
#' @family Package core functions
#' @export

setGeneric(
  name = "registerFunction",
  def = function(object, functionName,  heading ="", outAsIn=F, loadPipeline=F,
                 userDefined = T, session=session)
  {
    standardGeneric("registerFunction")
  }
)

setMethod(
  f = "registerFunction",
  signature = "AnalysisPipeline",
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
                              signature = \"AnalysisPipeline\",
                              definition = function(object",parametersName,")
                              {
                              parametersList <- unlist(strsplit(\"",sub(", ", "", parametersName),"\",\",\"))
                              parametersPassed <- lapply(parametersList,function(x){eval(parse(text = x))})

                              return(updateObject(object, \"",functionName,"\", \"",heading,"\", parametersPassed ,",outAsIn,"))
                              }
                              )
                              setMethod(
                              f = \"",functionName,"\",
                              signature = \"data.frame\",
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


#' @name generateReport
#' @title Generate a HTML report from an \code{AnalysisPipeline} object
#' @details
#'       The sequence of operations stored in the \code{AnalysisPipeline} object are run, outputs generated,
#'       and a HTML report is generated with outputs in the same sequence as the pipeline created by the user
#' @param object object that contains input, pipeline, registry and output
#' @param path path on the file system, where the generated html report should be stored
#' @return Updated \code{AnalysisPipeline} object
#' @family Package core functions
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

#' @name generateOutput
#' @title Generate a list of outputs from an \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object
#' @details
#'       The sequence of operations stored in the\code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object
#'       are run and outputs generated, stored in a list
#' @param object object that contains input, pipeline, registry and output
#' @return A list of the outputs in the sequence in which the pipeline was created
#' @family Package core functions
#' @export

setGeneric(
  name = "generateOutput",
  def = function(object)
  {
    standardGeneric("generateOutput")
  }
)

.generateOutput = function(object)
{
  input <- object@input
  for(rowNo in 1:nrow(object@pipeline)){
    if(object@pipeline[['outAsIn']][rowNo] == T && rowNo > 1){
      #object@workingInput <- do.call(object@pipeline[['operation']][[rowNo]], append(list(input), object@pipeline[['parameters']][[rowNo]]))
      object@workingInput <- object@output[[rowNo-1]]
    }else{
      object@workingInput <- input
    }
    object@output[[rowNo]] <- do.call(object@pipeline[['operation']][[rowNo]], append(list(object@workingInput), object@pipeline[['parameters']][[rowNo]]))
  }
  return(object)
}

setMethod(
  f = "generateOutput",
  signature = "AnalysisPipeline",
  definition = .generateOutput
)

setMethod(
  f = "generateOutput",
  signature = "SparkAnalysisPipeline",
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


#' @name savePipeline
#' @title Saves the \code{AnalysisPipeline} or \code{SparkAnalysisPipeline}  object to the file system without outputs
#' @details
#'       The \code{AnalysisPipeline} object is saved to the file system in the paths specified
#' @param object object that contains input, pipeline, registry and output
#' @param RDSPath the path at which the .RDS file containing the pipeline should be stored
#' @return Does not return a value
#' @family Package core functions
#' @export

setGeneric(
  name = "savePipeline",
  def = function(object, RDSPath)
  {
    standardGeneric("savePipeline")
  }
)

.savePipeline = function(object, RDSPath){
  object@output <- list()
  object@input <- data.frame()
  saveRDS(object,RDSPath)
}

setMethod(
  f = "savePipeline",
  signature = "AnalysisPipeline",
  definition = .savePipeline
)

setMethod(
  f = "savePipeline",
  signature = "SparkAnalysisPipeline",
  definition = .savePipeline
)


#' @name getPipeline
#' @title Obtain the pipeline
#' @param object The \code{AnalysisPipeline} or \code{SparkAnalysisPipeline}  object
#' @details
#'      Obtains the pipeline from the \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object as a tibble
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
  signature = "AnalysisPipeline",
  definition = .getPipeline
)

setMethod(
  f = "getPipeline",
  signature = "SparkAnalysisPipeline",
  definition = .getPipeline
)


#' @name getRegistry
#' @title Obtains the function registry
#' @param object The \code{AnalysisPipeline} or \code{SparkAnalysisPipeline}  object
#' @details
#'      Obtains the function registry from the \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object as a tibble,
#'      including both predefined and user defined functions
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
  signature = "AnalysisPipeline",
  definition = .getRegistry
)

setMethod(
  f = "getRegistry",
  signature = "SparkAnalysisPipeline",
  definition = .getRegistry
)

#' @name getInput
#' @title Obtains the initializedInput
#' @param object The \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object
#' @details
#'      Obtains the input from the \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object
#' @return Dataframe for an \code{AnalysisPipeline} & SparkDataFrame for a \code{SparkAnalysisPipeline}
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
  signature = "AnalysisPipeline",
  definition = .getInput
)

setMethod(
  f = "getInput",
  signature = "SparkAnalysisPipeline",
  definition = .getInput
)

#' @name getOuputByOrderId
#' @title Obtains a specific output
#' @param object The \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object
#' @param position The position of the function for which the output is desired in the sequence of operations in the pipeline.
#' @param includeCall Logical which defines whether the call used to generate the output should be returned. By, default this is false
#' @details
#'      Obtains a specific output from the \code{AnalysisPipeline} or \code{SparkAnalysisPipeline} object by passing the position
#'      of the function for which the output is desired, in the sequence of operations in the pipeline. This can be obtained by passing the number
#'      under the 'order' column in the pipeline table corresponding to the required function
#' @return If includeCall = F, the output object generated by the function is returned
#' @return If includeCall = T, it is a list containing to elements
#'         - call: tibble with 1 row containing the function call for the output desired
#'         - output: output generated
#' @family Package core functions
#' @export

setGeneric(
  name = "getOuputByOrderId",
  def = function(object, position, includeCall = F)
  {
    standardGeneric("getOuputByOrderId")
  }
)

.getOuputByOrderId = function(object, position, includeCall = F){
  op <- list(call = data.frame(),
             output = list())
  object@pipeline %>% dplyr::filter(order == position) -> call
  object@output[[position]] -> output

  if(includeCall){
    op <- list(call = call,
               output = output)
    return(op)
  }else{
    return(output)
  }

}

setMethod(
  f = "getOuputByOrderId",
  signature = "AnalysisPipeline",
  definition = .getOuputByOrderId
)

setMethod(
  f = "getOuputByOrderId",
  signature = "SparkAnalysisPipeline",
  definition = .getOuputByOrderId
)



#' @name loadPipeline
#' @title Loads the \code{AnalysisPipeline} object from the file system
#' @details
#'       The \code{AnalysisPipeline} object is loaded into the file system from the file system
#'       based on the path specified.
#' @details Optionally, the \code{input} parameter can be provided to
#'       initialize the \code{AnalysisPipeline} object with a data frame present in the R session.
#'       Another provided option, is to specify a filePath where the input dataset is present (in a .CSV format)
#'       and the object will be initialized with this data frame. The \code{filePath} parameter takes precedence over
#'       \code{input} parameter
#' @param RDSPath the path at which the .RDS file containing the pipeline is located
#' @param input (optional) data frame with which the pipeline object should be initialized
#' @param filePath (optional) path where a dataset in .CSV format is present which is to be loaded
#' @return An \code{AnalysisPipeline} object, optinally initialized with the data frame provided
#' @family Package core functions
#' @export

loadPipeline <- function(RDSPath, input=data.frame(), filePath=""){
  object <- readRDS(RDSPath)
  if(filePath == ""){
    object@input <- input
  }
  else{
    object@input <- read.csv(filePath)
  }
  registeredFunctions <- object@registry
  for(rowNo in 1:nrow(registeredFunctions)){
    object %>>% registerFunction(registeredFunctions[['functionName']][[rowNo]],
                                 registeredFunctions[['heading']][[rowNo]],
                                 registeredFunctions[['outAsIn']][[rowNo]],
                                 userDefined = registeredFunctions[['userDefined']][[rowNo]],
                                 loadPipeline = T) -> object
  }
  return(object)
}


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

#' @name explainFunction
#' @title Explain the parameters, and outputs of a specific predefined package function
#' @details
#' @param
#' @family Package core functions
#' @export


