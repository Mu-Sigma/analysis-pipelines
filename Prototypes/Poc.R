#################################################
# Title: Sample R package
# Version: 18.06.01
# Created on: June 14, 2018
# Description: A PoC for EDA Package
#################################################

library(tibble)
library(pipeR)

##### Create Object

eda <- setClass("eda",
                slots = c(
                  input = "data.frame",
                  output = "tbl",
                  wrapper = "character"
                ))


#### Constructor
setMethod(
  f = "initialize",
  signature = "eda",
  definition = function(.Object, input)
  {
    .Object@input <- input
    .Object@output <- tibble(
      operation = character(),
      functionName = character(),
      parameters = list()
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
                 funName,
                 parameters)
  {
    standardGeneric("updateObject")
  }
)

setMethod(
  f = "updateObject",
  signature = "eda",
  definition = function(object, operation, funName, parameters)
  {
    object@output %>>% add_row(operation = operation,
                               functionName = list(funName),
                               parameters = list(parameters)) -> object@output
    return(object)
  }
)


##### Register Function

setGeneric(
  name = "registerFunction",
  def = function(object, functionName, parametersList)
  {
    standardGeneric("registerFunction")
  }
)

setMethod(
  f = "registerFunction",
  signature = "eda",
  definition = function(object, functionName, parametersList)
  {
    
    parametersListNames <- paste0(parametersList,collapse = ",")
    
    registerFunText <- paste0("setGeneric(
      name = \"gen_",functionName,"\",
      def = function(object, ",parametersListNames,")
      {
        standardGeneric(\"gen_",functionName,"\")
      }
    )
    
    setMethod(
      f = \"gen_",functionName,"\",
      signature = \"eda\",
      definition = function(object, ",parametersListNames,")
      {
        parametersList <- unlist(strsplit(parametersListNames,\",\"))
        parametersPassed <- lapply(parametersList,function(x){eval(parse(text = x))})
        
        return(updateObject(object, \"",functionName,"\", paste0(\"",functionName,"\"), parametersPassed))
      }
    )")
    
    eval(parse(text = registerFunText))
    object@wrapper <- c(object@wrapper,registerFunText)
    return(object)
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
  signature = "eda",
  definition = function(object, colName)
  {
    return(updateObject(object, "ignoreCol", "ignoreColumns",list(object@input,colName)))
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
  signature = "eda",
  definition = function(object,
                        var1,
                        var2,
                        priColor = "blue",
                        secColor = "black")
  {
    return(updateObject(object, "bivarPlots", "bivarPlots", list(var1,var2,priColor,secColor)))
    
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
  signature = "eda",
  definition = function(object)
  {
    require(rmarkdown)
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
  parameters <- edaObject@output[3][[1]][[rowNo]]
  parameters <- append(list(edaObject@input),parameters)
  do.call(edaObject@output[2][[1]][[rowNo]],parameters)
}







