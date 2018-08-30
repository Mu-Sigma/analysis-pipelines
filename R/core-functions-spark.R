##################################################################################################
# Title: Reusable recipes for generating analysis reports
# Version: 18.08.01
# Created on: July 12, 2018
# Description: An R package version
##################################################################################################

#' @name readInputSpark
#' @title Function to initialize \code{SparkAnalysisRecipe} class with the input Spark DataFrame
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the recipe is to be applied, as well as the recipe itself
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initialize}
#' @slot input The input Spark DataFrame on which analysis is to be performed
#' @slot workingInput Internal slot for having a working version of the input
#' @slot recipe A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions for Spark
#' @export
readInputSpark <- setClass("SparkAnalysisRecipe",
                           slots = c(
                             input = "SparkDataFrame",
                             workingInput = "SparkDataFrame",
                             recipe = "tbl",
                             registry = "tbl",
                             output = "list"
                           ))

#' @name initialize
#' @title Constructor for the \code{SparkAnalysisRecipe} object
#' @param .Object The \code{SparkAnalysisRecipe} object
#' @param input The Spark DataFrame on which operations need to be performed
#' @details
#'      \code{input} needs to be provded and the argument needs to be of class \code{SparkDataFrame}, which is
#'      generally created through operations using SparkR
#' @return an object of class "\code{SparkAnalysisRecipe}", initialized with the input Spark DataFrame provided
#' @family Package core functions for Spark
#' @export
setMethod(
  f = "initialize",
  signature = "SparkAnalysisRecipe",
  definition = function(.Object, input)
  {
    .Object@input <- input
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

    for(rowNo in 1:nrow(sparkPredefFunctions)){
      .Object %>>% registerFunctionSpark(sparkPredefFunctions[['functionName']][[rowNo]],sparkPredefFunctions[['heading']][[rowNo]],sparkPredefFunctions[['outAsIn']][[rowNo]]) -> .Object
    }
    return(.Object)
  }
)


#' @name registerFunctionSpark
#' @title Register a user-defined function to be used with \code{SparkAnalysisRecipe} objects specifically for Spark
#'        DataFrames
#' @details
#'       The specified operation along with the heading and parameters is updated in the recipe slot
#'       of the \code{SparkAnalysisRecipe} object, where the sequence of operations to be performed is stored. This function
#'       is used to register functions which are designed to operate on Spark DataFrames
#' @param object object that contains input, recipe, registry and output
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param outAsIn whether to use original input or output from previous function
#' @param loadRecipe logical parameter to see if function is being used in loadRecipe or not
#' @param session to load shiny session in the function (Currently not implemented)
#' @return Updated \code{SparkAnalysisRecipe} object
#' @family Package core functions for Spark
#' @export

setGeneric(
  name = "registerFunctionSpark",
  def = function(object, functionName,  heading ="", outAsIn=F, loadRecipe=F, session=session)
  {
    standardGeneric("registerFunctionSpark")
  }
)

setMethod(
  f = "registerFunctionSpark",
  signature = "SparkAnalysisRecipe",
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
                              signature = \"SparkAnalysisRecipe\",
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
    if(loadRecipe==F){
      object@registry %>>% add_row(functionName = paste0(functionName),
                                   heading = heading,
                                   outAsIn = outAsIn) -> object@registry
    }
    return(object)
  }
)


#'### TODO
#'  @name loadRecipeSpark
#' @title Loads the \code{SparkAnalysisRecipe} object from the file system
#' @details
#'       The \code{SparkAnalysisRecipe} object is loaded into the file system from the file system
#'       based on the path specified.
#' @param RDSPath the path at which the .RDS file containing the recipe is located
#' @param input Spark DataFrame with which the recipe object should be initialized
#' @return An \code{SparkAnalysisRecipe} object, optinally initialized with the data frame provided
#' @family Package core functions for Spark
#' @export
loadRecipeSpark <- function(RDSPath, input){
  object <- readRDS(RDSPath)
    object@input <- input
  registeredFunctions <- object@registry
  for(rowNo in 1:nrow(registeredFunctions)){
    object %>>% registerFunctionSpark(registeredFunctions[['functionName']][[rowNo]],registeredFunctions[['heading']][[rowNo]],registeredFunctions[['outAsIn']][[rowNo]],loadRecipe=T) -> object
  }
  return(object)
}
