#################################################
# Title: Reusable recipes for generating analysis reports
# Version: 18.07.01
# Created on: July 12, 2018
# Description: An R package version
#################################################

#' @name readInput
#' @title Function to initialize \code{AnalysisRecipe} class with the input data frame
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the recipe is to be applied, as well as the recipe itself
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initialize}
#' @slot input The input dataset on which analysis is to be performed
#' @slot filePath Path of the input dataset to be uploaded
#' @slot recipe A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions
#' @export
readInput <- setClass("AnalysisRecipe",
                       slots = c(
                         input = "data.frame",
                         filePath = "character",
                         recipe = "tbl",
                         registry = "tbl",
                         output = "list"
                       ))

#' @name initialize
#' @title Constructor for the \code{AnalysisRecipe} object
#' @param .Object The \code{AnalysisRecipe} object
#' @param input The data frame on which operations need to be performed
#' @param filePath File path for a .csv file to directly read in the dataset from
#' @details
#'      Either one of \code{input} or \code{filePath} need to be provided i.e. either the
#'      data frame or the file path to a csv file
#' @return an object of class "\code{AnalysisRecipe}", initialized with the input data frame provided
#' @family Package core functions
setMethod(
  f = "initialize",
  signature = "AnalysisRecipe",
  definition = function(.Object, input = data.frame(), filePath = "")
  {
    if(filePath == ""){
      .Object@input <- input
    }
    else{
      .Object@input <- read.csv(filePath)
    }
    .Object@recipe <- tibble(
      operation = character(),
      heading = character(),
      parameters = list(),
      outAsIn = logical()

    )
    .Object@registry <- tibble(
      functionName = character(),
      heading = character(),
      outAsIn = logical()

    )
    .Object@output <- list()
    #brickFunctions <- readRDS('support/predefFunctions.RDS')
    for(rowNo in 1:nrow(brickFunctions)){
      .Object %>>% registerFunction(brickFunctions[['functionName']][[rowNo]],brickFunctions[['heading']][[rowNo]],brickFunctions[['outAsIn']][[rowNo]]) -> .Object
    }
    return(.Object)
  }
)

#' @name updateObject
#' @title Update the \code{AnalysisRecipe} object by adding an operation to the recipe
#' @details
#'       The specified operation along with the heading and parameters is updated in the recipe slot
#'       of the AnalysisRecipe object, where the sequence of operations to be performed is stored
#' @param object object that contains input, recipe, registry and output
#' @param operation function name to be updated in tibble
#' @param heading heading of that section in report
#' @param parameters parameters passed to that function
#' @param outAsIn whether to use output of this function as input to next
#' @return Updated \code{AnalysisRecipe} object
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
  object@recipe %>>% add_row(operation = operation,
                             heading = heading,
                             parameters = list(parameters),
                             outAsIn = outAsIn) -> object@recipe
  return(object)
}

setMethod(
  f = "updateObject",
  signature = "AnalysisRecipe",
  definition = .updateObject
)

setMethod(
  f = "updateObject",
  signature = "SparkAnalysisRecipe",
  definition = .updateObject
)

#' @name registerFunction
#' @title Register a user-defined function to be used with \code{AnalysisRecipe} objects
#' @details
#'       The specified operation along with the heading and parameters is updated in the recipe slot
#'       of the AnalysisRecipe object, where the sequence of operations to be performed is stored
#' @param object object that contains input, recipe, registry and output
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param outAsIn whether to use output of this function as input to next
#' @param loadRecipe logical parameter to see if function is being used in loadRecipe or not
#' @param session to load shiny session in the function
#' @return Updated \code{AnalysisRecipe} object
#' @family Package core functions
#' @export

setGeneric(
  name = "registerFunction",
  def = function(object, functionName,  heading ="", outAsIn=F, loadRecipe=F, session=session)
  {
    standardGeneric("registerFunction")
  }
)

setMethod(
  f = "registerFunction",
  signature = "AnalysisRecipe",
  definition = function(object, functionName,  heading ="", outAsIn=F, loadRecipe=F, session=session)
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
                              signature = \"AnalysisRecipe\",
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
    if(loadRecipe==F){
      object@registry %>>% add_row(functionName = paste0(functionName),
                                   heading = heading,
                                   outAsIn = outAsIn) -> object@registry
    }
    return(object)
                              }
                              )


#' @name generateReport
#' @title Generate a HTML report from an \code{AnalysisRecipe} object
#' @details
#'       The sequence of operations stored in the \code{AnalysisRecipe} object are run, outputs generated,
#'       and a HTML report is generated with outputs in the same sequence as the pipeline created by the user
#' @param object object that contains input, recipe, registry and output
#' @param path path on the file system, where the generated html report should be stored
#' @return Updated \code{AnalysisRecipe} object
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
  signature = c("AnalysisRecipe", "SparkAnalysisRecipe"),
  definition = function(object,path)
  {
    require(rmarkdown)
    if(length(object@output) == 0){
      object <- generateOutput(object)
    }
    object <- updateObject(object, "emptyRow", "emptyRow",list("emptyRow"),F)


    rmarkdown::render(
      'support/report.Rmd',
      params = list(
        input = object@input,
        recipe = object@recipe,
        output = object@output
      ),
      html_document(
        css = "styles.css",
        toc = T,
        toc_float = T
      ),

      output_dir = path ,
      output_file = paste('EDA_report_',Sys.time(),'.html', sep = '')
    )

  }
)

#' @name generateOutput
#' @title Generate a list of outputs from an \code{AnalysisRecipe} object
#' @details
#'       The sequence of operations stored in the \code{AnalysisRecipe} object are run and outputs generated,
#'       stored in a list
#' @param object object that contains input, recipe, registry and output
#' @return A list of the outputs in the sequence in which the recipe was created
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
  for(rowNo in 1:nrow(object@recipe)){
    if(object@recipe[['outAsIn']][rowNo] == T){
      input <- do.call(object@recipe[['operation']][[rowNo]], append(list(input), object@recipe[['parameters']][[rowNo]]))
    }
    object@output[[rowNo]] <- do.call(object@recipe[['operation']][[rowNo]], append(list(input), object@recipe[['parameters']][[rowNo]]))
  }
  return(object)
}

setMethod(
  f = "generateOutput",
  signature = "AnalysisRecipe",
  definition = .generateOutput
)

setMethod(
  f = "generateOutput",
  signature = "SparkAnalysisRecipe",
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


#' @name saveRecipe
#' @title Saves the \code{AnalysisRecipe} object to the file system
#' @details
#'       The \code{AnalysisRecipe} object is saved to the file system in the paths specified
#' @param object object that contains input, recipe, registry and output
#' @param RDSPath the path at which the .RDS file containing the recipe should be stored
#' @return Does not return a value
#' @family Package core functions
#' @export

setGeneric(
  name = "saveRecipe",
  def = function(object, RDSPath)
  {
    standardGeneric("saveRecipe")
  }
)

.saveRecipe = function(object,RDSPath){
  object@output <- list()
  object@input <- data.frame()
  saveRDS(object,RDSPath)
}

setMethod(
  f = "saveRecipe",
  signature = "AnalysisRecipe",
  definition = .saveRecipe
)

setMethod(
  f = "saveRecipe",
  signature = "SparkAnalysisRecipe",
  definition = .saveRecipe
)


#' @name loadRecipe
#' @title Loads the \code{AnalysisRecipe} object from the file system
#' @details
#'       The \code{AnalysisRecipe} object is loaded into the file system from the file system
#'       based on the path specified.
#' @details Optionally, the \code{input} parameter can be provided to
#'       initialize the \code{AnalysisRecipe} object with a data frame present in the R session.
#'       Another provided option, is to specify a filePath where the input dataset is present (in a .CSV format)
#'       and the object will be initialized with this data frame. The \code{filePath} parameter takes precedence over
#'       \code{input} parameter
#' @param RDSPath the path at which the .RDS file containing the recipe is located
#' @param input (optional) data frame with which the recipe object should be initialized
#' @param filePath (optional) path where a dataset in .CSV format is present which is to be loaded
#' @return An \code{AnalysisRecipe} object, optinally initialized with the data frame provided
#' @family Package core functions
#' @export

loadRecipe <- function(RDSPath, input=data.frame(), filePath=""){
  object <- readRDS(RDSPath)
  if(filePath == ""){
    object@input <- input
  }
  else{
    object@input <- read.csv(filePath)
  }
  registeredFunctions <- object@registry
  for(rowNo in 1:nrow(registeredFunctions)){
    object %>>% registerFunction(registeredFunctions[['functionName']][[rowNo]],registeredFunctions[['heading']][[rowNo]],registeredFunctions[['outAsIn']][[rowNo]],loadRecipe=T) -> object
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
#' @return An \code{AnalysisRecipe} object, optinally initialized with the data frame provided
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


