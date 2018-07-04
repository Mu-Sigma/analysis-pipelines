#################################################
# Title: Sample R package
# Version: 18.06.01
# Created on: June 14, 2018
# Description: A PoC for EDA Package
#################################################

library(tibble)
library(pipeR)
library(data.table)
library(magrittr)

source('EDAUtils.R')




##### Create Object

#' @decription 
#' @slot input The input dataset on which analysis is to be performed
#' @slot filePath Path of the input dataset to be uploaded
#' @slot recipe A tibble which holds functions to be called
#' @slot registry A tibble which holds all the registered functions
#' @slot output A list which holds all the functions output
#' @export
read_input <- setClass("brickObject",
                       slots = c(
                         input = "data.frame",
                         filePath = "character",
                         recipe = "tbl",
                         registry = "tbl",
                         output = "list"
                       ))

#### Constructor
setMethod(
  f = "initialize",
  signature = "brickObject",
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
    brickFunctions <- readRDS('predefFunctions.RDS')
    for(rowNo in 1:nrow(brickFunctions)){
      .Object %>>% registerFunction(brickFunctions[['functionName']][[rowNo]],brickFunctions[['heading']][[rowNo]],brickFunctions[['outAsIn']][[rowNo]]) -> .Object
    }
    return(.Object)
  }
)




##### Object Update Function

#' @decription 
#' @param object object that contains input, recipe, registry and output
#' @param operation function name to be updated in tibble
#' @param heading heading of that section in report
#' @param parameters parameters passed to that function
#' @param outAsIn whether to use output of this function as input to next
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
  signature = "brickObject",
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
  signature = "brickObject",
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
                              signature = \"brickObject\",
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
  signature = "brickObject",
  definition = function(object,path)
  {
    require(rmarkdown)
    if(length(object@output) == 0){
      object <- generateOutput(object)
    }
    object <- updateObject(object, "emptyRow", "emptyRow",list("emptyRow"),F)
    
    
    rmarkdown::render(
      'report.Rmd',
      params = list(
        input = object@input,
        recipe = object@recipe,
        output = object@output
      ),
      html_document(
        css = "styles.css" ,
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
  signature = "brickObject",
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
  def = function(object,RDSPath)
  {
    standardGeneric("saveRecipe")
  }
)

setMethod(
  f = "saveRecipe",
  signature = "brickObject",
  definition = function(object,RDSPath)
  {
    object@output <- list()  
    saveRDS(object,RDSPath)
  }
)



###### Load Recipe

#' @description 
#' @param RDSPath file path for object to be loaded
#' @param input The input dataset on which analysis is to be performed
#' @param filePath Path of the input dataset to be uploaded
#' @export
loadRecipe <- function(RDSPath,input=data.frame(),filePath=""){
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




