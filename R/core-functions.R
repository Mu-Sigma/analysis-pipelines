#################################################
# Title: Reusable recipes for generating analysis reports
# Version: 18.07.01
# Created on: July 12, 2018
# Description: An R package version


#' @name read_input
#' @title Function to initialize \code{AnalysisRecipe} class with the input data frame
#' @details The class which holds the metadata including the recipe of available functions,
#' the data on which the recipe is to be applied, as well as the recipe itself
#' @details More details of how the object of this class should be initialized is provided in the
#' constructor - \link{initialize}
#' @slot input The input dataset on which analysis is to be performed
#' @slot filePath Path of the input dataset to be uploaded
#' @slot recipe A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @family Package core functions
#' @export
read_input <- setClass("AnalysisRecipe",
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
    brickFunctions <- readRDS('support/predefFunctions.RDS')
    for(rowNo in 1:nrow(brickFunctions)){
      .Object %>>% registerFunction(brickFunctions[['functionName']][[rowNo]],brickFunctions[['heading']][[rowNo]],brickFunctions[['outAsIn']][[rowNo]]) -> .Object
    }
    return(.Object)
  }
)

##### Object Update Function

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
setMethod(
  f = "updateObject",
  signature = "AnalysisRecipe",
  definition = function(object, operation, heading="", parameters, outAsIn = F)
  {
    object@recipe %>>% add_row(operation = operation,
                               heading = heading,
                               parameters = list(parameters),
                               outAsIn = outAsIn) -> object@recipe
    return(object)
  }
)


##### Register Function

#' @description
#' @param object object that contains input, recipe, registry and output
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param outAsIn whether to use output of this function as input to next
#' @param loadRecipe logical parameter to see if function is being used in loadRecipe or not
#' @param session to load shiny session in the function
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
    methodBody <- paste0(capture.output(body(eval(parse(text=functionName)))),collapse="\n")
    firstArg <- names(as.list(args(eval(parse(text=functionName)))))[1]
    methodBody <- paste0("{",firstArg,"=object",substring(methodBody,2))
    methodArg <- paste0(capture.output(args(eval(parse(text=functionName)))),collapse="")
    methodArg <- strsplit(strsplit(methodArg,firstArg)[[1]][2],"NULL")[[1]][1]
    registerFunText <- paste0("setGeneric(
                              name = \"",functionName,"\",
                              def = function(object, ",parametersName,")
                              {
                              standardGeneric(\"",functionName,"\")
                              }
                              )

                              setMethod(
                              f = \"",functionName,"\",
                              signature = \"AnalysisRecipe\",
                              definition = function(object, ",parametersName,")
                              {
                              parametersList <- unlist(strsplit(\"",parametersName,"\",\",\"))
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



###### Generate Report

#' @description
#' @param object object that contains input, recipe, registry and output
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
  signature = "AnalysisRecipe",
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



###### Generate Output

#' @description
#' @param object object that contains input, recipe, registry and output
#' @export
setGeneric(
  name = "generateOutput",
  def = function(object)
  {
    standardGeneric("generateOutput")
  }
)

setMethod(
  f = "generateOutput",
  signature = "AnalysisRecipe",
  definition = function(object)
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



###### Save Recipe

#' @description
#' @param object object that contains input, recipe, registry and output
#' @param RDSPath path for saving file
#' @export
setGeneric(
  name = "saveRecipe",
  def = function(object, RDSPath)
  {
    standardGeneric("saveRecipe")
  }
)

setMethod(
  f = "saveRecipe",
  signature = "AnalysisRecipe",
  definition = function(object,RDSPath)
  {
    object@output <- list()
    object@input <- data.frame()
    saveRDS(object,RDSPath)
  }
)



###### Load Recipe

#' @description
#' @param RDSPath file path for object to be loaded
#' @param input The input dataset on which analysis is to be performed
#' @param filePath Path of the input dataset to be uploaded
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

##################
# MISC FUNCTIONS #
##################

# This function can assist developers to register new functions that they want to add to the package
updatePackageRegistry <- function(functionName, functionHeader, flag){
  #
  # 'functionName' is the name of the function
  # 'functionHeader' is the header caption that will feature in the report for this function's output
  # 'flag' is a boolean which dictates if the 'functionName' function returns a data.frame to be used an input
  # example usage: updatePackageRegistry('ignoreCols', '', TRUE)
  #
  tryCatch({
    functionsDefined <- readRDS("support/predefFunctions.RDS")
    invisible(source("EDA.R"))
    functionList <- ls(envir = .GlobalEnv)
    if(functionName %in% functionList){
      functionsDefined <- add_row(functionsDefined, functionName = functionName, heading = functionHeader, outAsIn = flag)
      print(functionsDefined)
      saveRDS(functionsDefined, "support/predefFunctions.RDS")
      print("Successfully Registered function into package!")
    }else
      print(paste0("Failed to register function into package. Could not find function '", functionName, "' in the environment."))
  }, error = function(e){
    stop(e)
  }, warning = function(e){
    warning(e)
  })
}




########################
## PREPARING PACKAGES ##
########################

# # Expands the package list into a dependency tree of its base packages
# packageDependencyList <- function(packageList){
#   if(class(packageList) != "character"){
#     packageList <- as.character(packageList)
#   }
#   if(length(packageList) == 0){
#     stop('input parameter is empty')
#   }
#   # Create a graph object to containing package dependency hierarchy
#   dependency.check <- miniCRAN::makeDepGraph(packageList, suggests = FALSE, includeBasePkgs = FALSE)
#
#   # Iterates over each element of the object packageList and gives out the individual package dependencies for each of these
#   # Elements of packageList
#   result <- list()
#   for(i in 1:length(packageList)){
#     result[[i]] <- (igraph::topo_sort(dependency.check))}
#
#   # For each element in the list, numbers are replaced with package names, remove NAs and reverse the order to get the correct order
#   result <- lapply(result, function(x){
#     x <- igraph::as_ids(x)
#   })
#   result <- Reduce(c, result) #append all the lists into one list
#   result <- result[!duplicated(result)] #remove duplicates
#
#   return(result)
# }
#
#
# # Function to install packages if they don't exist and return a list of packages that failed to install
# installPackages <- function(packageList){
#   lapply(packageList, function(x){
#     if(!(x %in% installed.packages()[,1]))
#       install.packages(x, dependencies = T, repos = "http://cloud.r-project.org/")
#   })
#   failedList <- NULL
#   lapply(packageList, function(x){
#     if(!(x %in% installed.packages()[,1]))
#       failedList <<- c(failedList, x)
#   })
#   return(failedList)
# }
#
# # Function that installs the basic packages required to run the package
# installBaseDependencies <- function(){
#   packageList <- c("tibble", "pipeR", "data.table", "magrittr")
#   packageListBig <- packageDependencyList(packageList)
#   failedList <- installPackages(packageListBig)
#   failedList <- NULL
#   if(!is.null(failedList))
#     stop(paste0("The following packages failed to install: ", paste0(failedList, collapse = ", ")))
#   print("Success!")
# }
# # Calling the function at source to ready the use of the package
# installBaseDependencies()
#
# # Function that maintains a list of packages required per function and installs them when called
# installRequiredPackages <- function(){
#   packageList <- c('univarCatDistPlots' = c('ggplot2', 'plotly', 'dplyr'),
#                    'outlierPlot' = c('ggplot2', 'plotly'),
#                    'multiVarOutlierPlot' = c('ggplot2', 'plotly')
#   )
#   packageList <- unique(unlist(packageList))
#   failedList <- installPackages(packageList)
#   failedList <- NULL
#   if(!is.null(failedList))
#     stop(paste0("The following packages failed to install: ", paste0(failedList, collapse = ", ")))
#   print("Success!")
# }

##################################################
#
# library(tibble)
# library(pipeR)
# library(data.table)
# library(magrittr)
