#################################################
# Title: Sample R package
# Version: 18.06.01
# Created on: June 14, 2018
# Description: A PoC for EDA Package
#################################################

library(tibble)
library(pipeR)
library(magrittr)

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
      heading = character(),
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
                 heading,
                 funName,
                 parameters)
  {
    standardGeneric("updateObject")
  }
)

setMethod(
  f = "updateObject",
  signature = "eda",
  definition = function(object, operation, heading="", funName, parameters)
  {
    object@output %>>% add_row(operation = operation,
                               heading = heading,
                               functionName = funName,
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
  signature = "eda",
  definition = function(object)
  {
    return(updateObject(object, "quickPeek", "Quick Peek", "ignoreColumns",list("colName")))
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
    return(updateObject(object, "ignoreCol", "", "ignoreColumns",list(colName)))
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
    return(updateObject(object, "bivarPlots", "Bivariate Plots", "bivarPlots", list(var1,var2,priColor,secColor)))
    
  }
)


###### Univariate Distribution Categorical

setGeneric(
  name = "uniCatPlot",
  def = function(object,
                 uniCol,
                 priColor = "blue")
  {
    standardGeneric("uniCatPlot")
  }
)

setMethod(
  f = "uniCatPlot",
  signature = "data.frame",
  definition = function(object,
                        uniCol,
                        priColor = "blue")
  {
    dataset <- object
    levels(dataset[[uniCol]]) <- c(levels(dataset[[uniCol]]), "NA")
    dataset[[uniCol]][is.na(dataset[[uniCol]])] <- "NA"
    dataset <- dataset %>% dplyr::group_by_(.dots = c(uniCol)) %>% dplyr::summarise(count = n())
    y <- dataset[[uniCol]]
    catPlot <- plotly::plot_ly(y = y, x=dataset[["count"]],type="bar",orientation='h',color = I(priColor)) %>%
      plotly::layout(title=paste0("Frequency Histogram for ",uniCol),
                     xaxis=list(title = "Frequency"),
                     yaxis=list(title = uniCol))
      
      
    return(catPlot)

  }
)

setMethod(
  f = "uniCatPlot",
  signature = "eda",
  definition = function(object,
                        uniCol,
                        priColor = "blue")
  {
    return(updateObject(object, "uniCatPlot", "Univariate Distribution Categorical", "uniCatPlot", list(uniCol,priColor)))
    
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
  do.call(edaObject@output['functionName'][[rowNo]],parameters)                     
}







