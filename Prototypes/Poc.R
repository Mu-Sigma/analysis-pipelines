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
      outAsIn = character()
    )
    .Object@registry <- readRDS('predefFunctions.RDS')
    .Object@output <- list()
    .Object = createFunctions(.Object)
    return(.Object)
  }
)


##### Object Update Function

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




##### Create Function

setGeneric(
  name = "createFunctions",
  def = function(object)
  {
    standardGeneric("createFunctions")
  }
)

setMethod(
  f = "createFunctions",
  signature = "brickObject",
  definition = function(object)
  {
    for(rowNo in 1:nrow(object@registry)){
        functionName = object@registry[["functionName"]][[rowNo]]
        heading = object@registry[["heading"]][[rowNo]]
        outAsIn = object@registry[["outAsIn"]][[rowNo]]
        parametersName <- names(as.list(args(functionName)))
        parametersName <- paste0(parametersName[c(-1,-length(parametersName))],collapse=",")

        
        registerFunText <- paste0("setGeneric(
          name = \"eda_",functionName,"\",
          def = function(object, ",parametersName,")
          {
            standardGeneric(\"eda_",functionName,"\")
          }
        )
        
        setMethod(
          f = \"eda_",functionName,"\",
          signature = \"brickObject\",
          definition = function(object, ",parametersName,")
          {
            parametersList <- unlist(strsplit(\"",parametersName,"\",\",\"))
            parametersPassed <- lapply(parametersList,function(x){eval(parse(text = x))})
            
            return(updateObject(object, \"",functionName,"\", \"",heading,"\", parametersPassed ,",outAsIn,"))
          }
        )")
        
        eval(parse(text = registerFunText))
  }
    return(object)
  }
)


##### Register Function

setGeneric(
  name = "registerFunction",
  def = function(object, functionName,  heading ="", outAsIn=F)
  {
    standardGeneric("registerFunction")
  }
)

setMethod(
  f = "registerFunction",
  signature = "brickObject",
  definition = function(object, functionName,  heading ="", outAsIn=F)
  {
    parametersName <- names(as.list(args(functionName)))
    parametersName <- paste0(parametersName[c(-1,-length(parametersName))],collapse=",")

    
    registerFunText <- paste0("setGeneric(
      name = \"udf_",functionName,"\",
      def = function(object, ",parametersName,")
      {
        standardGeneric(\"udf_",functionName,"\")
      }
    )
    
    setMethod(
      f = \"udf_",functionName,"\",
      signature = \"brickObject\",
      definition = function(object, ",parametersName,")
      {
        parametersList <- unlist(strsplit(\"",parametersName,"\",\",\"))
        parametersPassed <- lapply(parametersList,function(x){eval(parse(text = x))})
        
        return(updateObject(object, \"",functionName,"\", \"",heading,"\", parametersPassed ,",outAsIn,"))
      }
    )")
    
    eval(parse(text = registerFunText))
    object@registry %>>% add_row(functionName = functionName,
                                heading = heading,
                                outAsIn = outAsIn) -> object@registry
    return(object)
  }
)




###### Generate Report

setGeneric(
  name = "generateReport",
  def = function(object)
  {
    standardGeneric("generateReport")
  }
)

setMethod(
  f = "generateReport",
  signature = "brickObject",
  definition = function(object)
  {
    require(rmarkdown)
    if(length(object@output) == 0){
      object <- generateOutput(object)
    }
    object <- updateObject(object, "emptyRow", "emptyRow",list("emptyRow"),F)
    
    fileName <- "ss"
    
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
        
      output_dir = "." ,
      output_file = paste(fileName,'.html', sep = '')
    )
    
  }
)





###### Generate Output

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






