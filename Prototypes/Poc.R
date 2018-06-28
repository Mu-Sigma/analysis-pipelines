#################################################
# Title: Sample R package
# Version: 18.06.01
# Created on: June 14, 2018
# Description: A PoC for EDA Package
#################################################

library(tibble)
library(pipeR)
library(magrittr)
library(data.table)

##### Create Object

read_input <- setClass("brickObject",
                slots = c(
                  input = "data.frame",
                  filePath = "character",
                  output = "tbl",
                  wrapper = "character"
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
    .Object@output <- tibble(
      operation = character(),
      heading = character(),
      functionName = character(),
      parameters = list(),
      outAsIn = character()
    )
    .Object@wrapper <- character()
    
    return(.Object)
  }
)


##### Object Update FUnction

setGeneric(
  name = "updateObject",
  def = function(object,
                 operation,
                 heading = "",
                 funName,
                 parameters,
                 outAsIn = F)
  {
    standardGeneric("updateObject")
  }
)

setMethod(
  f = "updateObject",
  signature = "brickObject",
  definition = function(object, operation, heading="", funName, parameters, outAsIn = F)
  {
    object@output %>>% add_row(operation = operation,
                               heading = heading,
                               functionName = funName,
                               parameters = list(parameters),
                               outAsIn = outAsIn) -> object@output
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
    parametersList <- capture.output(args(functionName))[1]
    secondArg <- substring(capture.output(as.list(args(functionName)))[4],2)
    parametersListNames <- paste0(secondArg,strsplit(parametersList,secondArg)[[1]][2])
    
    registerFunText <- paste0("setGeneric(
      name = \"udf_",functionName,"\",
      def = function(object, ",parametersListNames,"
      {
        standardGeneric(\"udf_",functionName,"\")
      }
    )
    
    setMethod(
      f = \"udf_",functionName,"\",
      signature = \"brickObject\",
      definition = function(object, ",parametersListNames,"
      {
        parametersList <- unlist(strsplit(\"",parametersName,"\",\",\"))
        parametersPassed <- lapply(parametersList,function(x){eval(parse(text = x))})
        
        return(updateObject(object, \"",functionName,"\", \"",heading,"\", paste0(\"",functionName,"\"), parametersPassed ,",outAsIn,"))
      }
    )")
    
    eval(parse(text = registerFunText))
    object@wrapper <- c(object@wrapper,registerFunText)
    return(object)
  }
)




##### Quick Peek

setGeneric(
  name = "quickPeek",
  def = function(object)
  {
    standardGeneric("quickPeek")
  }
)

setMethod(
  f = "quickPeek",
  signature = "brickObject",
  definition = function(object)
  {
    return(updateObject(object, "quickPeek", "Quick Peek", "ignoreColumns",list("colName"),F))
  }
)


##### Ignore Columns

setGeneric(
  name = "ignoreColumns",
  def = function(object, colName)
  {
    standardGeneric("ignoreColumns")
  }
)

setMethod(
  f = "ignoreColumns",
  signature = "data.frame",
  definition = function(object, colName)
  {
    return(object %>>% dplyr::select(-dplyr::one_of(colName)))
  }
)
 
setMethod(
  f = "ignoreColumns",
  signature = "brickObject",
  definition = function(object, colName)
  {
    return(updateObject(object, "ignoreCol", "", "ignoreColumns",list(colName),T))
  }
)








###### Bivariate Plot

setGeneric(
  name = "bivarPlots",
  def = function(object,
                 var1,
                 var2,
                 priColor = "blue",
                 secColor = "black")
  {
    standardGeneric("bivarPlots")
  }
)

setMethod(
  f = "bivarPlots",
  signature = "data.frame",
  definition = function(object,
                        var1,
                        var2,
                        priColor = "blue",
                        secColor = "black")
  {
    dataset <- object
    bivarPlot <-
      ggplot2::ggplot(dataset, ggplot2::aes(dataset[, var1], dataset[, var2])) +
      ggplot2::geom_point(color = priColor) +
      ggplot2::geom_smooth(method = lm, color = secColor)
    
    return(bivarPlot)
  }
)

setMethod(
  f = "bivarPlots",
  signature = "brickObject",
  definition = function(object,
                        var1,
                        var2,
                        priColor = "blue",
                        secColor = "black")
  {
    return(updateObject(object, "bivarPlots", "Bivariate Plots", "bivarPlots", list(var1,var2,priColor,secColor),F))
    
  }
)




###### Generate Report

setGeneric(
  name = "genReport",
  def = function(object)
  {
    standardGeneric("genReport")
  }
)

setMethod(
  f = "genReport",
  signature = "brickObject",
  definition = function(object)
  {
    require(rmarkdown)
    object <- quickPeek(object)
    fileName <- "ss"
    rmarkdown::render(
      'report.Rmd',
      params = list(
        input = object@input,
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


viewOutput <- function(edaObject,rowNo){
  parameters <- edaObject@output['parameters'][[1]][[rowNo]]
  parameters <- append(list(edaObject@input),parameters)
  do.call(edaObject@output['functionName'][[1]][rowNo],parameters)                     
}







