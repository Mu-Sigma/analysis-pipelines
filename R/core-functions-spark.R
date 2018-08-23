#' @import SparkR

#' @name sparkRSessionCreateIfNotPresent
#' @title
#' @details
#' @details
#' @slot input The input dataset on which analysis is to be performed
#' @slot
#' @family Package core functions
#' @export

sparkRSessionCreateIfNotPresent <- function(...){

  if(Sys.getenv("SPARK_HOME") == "" && sparkHome == ""){
    stop("SPARK_HOME environment variable is not set on the system, and sparkHome argument is empty")
  }

  if(!("SparkR" %in% installed.packages())){
    stop("SparkR package not installed. Please install from the $SPARK_HOME folder")
  }

  if(sparkHome == ""){
    .libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
    sparkHome <- Sys.getenv("SPARK_HOME")
  }else{
    .libPaths(c(file.path(sparkHome, "R", "lib"), .libPaths()))
  }

  library(SparkR)
  sparkR.session(...)
}

#' @name readInput
#' @title Function to initialize \code{AnalysisRecipe} class with the input data frame
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the recipe is to be applied, as well as the recipe itself. Overloads the initialization function
#' for Spark DataFrames
#' @details More details of how an object of this class should be initialized is provided in the
#' constructor - \link{initialize}
#' @slot input The input dataset on which analysis is to be performed
#' @slot filePath Path of the input dataset to be uploaded
#' @slot recipe A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions
#' @export
readInputSpark <- setClass("AnalysisRecipe",
                           slots = c(
                             input = "SparkDataFrame",
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
    return(.Object)
  }
)


#' @name registerFunctionSpark
#' @title Register a user-defined function to be used with \code{AnalysisRecipe} objects specifically for Spark
#'        DataFrames
#' @details
#'       The specified operation along with the heading and parameters is updated in the recipe slot
#'       of the AnalysisRecipe object, where the sequence of operations to be performed is stored. This function
#'       is used to register functions which are designed to operate on Spark DataFrames
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
  name = "registerFunctionSpark",
  def = function(object, functionName,  heading ="", outAsIn=F, loadRecipe=F, session=session)
  {
    standardGeneric("registerFunctionSpark")
  }
)

setMethod(
  f = "registerFunctionSpark",
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
                              parametersList <- unlist(strsplit(\"",parametersName,"\",\",\"))
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
