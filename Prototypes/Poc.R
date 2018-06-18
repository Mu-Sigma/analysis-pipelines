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
      functionName = character()
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
                 val)
  {
    standardGeneric("updateObject")
  }
)

setMethod(
  f = "updateObject",
  signature = "eda",
  definition = function(object, operation, val)
  {
    object@output %>>% add_row(operation = operation,
                               functionName = list(val)) -> object@output
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
    wrapperGenericText <- paste0("setGeneric(name = \"",functionName,"\",def = function(object, parametersList=c(\"\")){standardGeneric(\"",functionName,"\")})")
    wrapperMethodText <- paste0("setMethod(f = \"",functionName,"\",signature = \"eda\",definition = function(object, parametersList=c(\"\")){object@wrapper <- ssas})")
    eval(parse(text=wrapperGenericText))
    eval(parse(text=wrapperMethodText))
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
    return(updateObject(object, "ignoreCol", ignoreColumns))
  }
)





##### Univariate Categorical Plot

setGeneric(
  name = "uniCatPlots",
  def = function(object, uniCol, priColor = "blue")
  {
    standardGeneric("uniCatPlots")
  }
)

setMethod(
  f = "uniCatPlots",
  signature = "data.frame",
  definition = function(object, uniCol, priColor = "blue")
  {
    data <- object
    levels(data[[uniCol]]) <- c(levels(data[[uniCol]]), "NA")
    data[[uniCol]][is.na(data[[uniCol]])] <- "NA"
    data <-
      data %>>% dplyr::group_by_(.dots = c(uniCol)) %>>% dplyr::summarise(count = n())
    y = data[[uniCol]]
    catPlot <-
      plotly::plot_ly(
        y = y,
        x = data[["count"]],
        type = "bar",
        orientation = 'h',
        color = I(priColor)
      ) %>>%
      plotly::layout(
        title = paste0("Frequency Histogram for ", uniCol),
        xaxis = list(title = "Frequency"),
        yaxis = list(title = uniCol)
      )
    
    return(catPlot)
    
  }
)

setMethod(
  f = "uniCatPlots",
  signature = "eda",
  definition = function(object, uniCol, priColor = "blue")
  {
    return(updateObject(object, "uniCatPlots", uniCatPlots))
    
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
    return(updateObject(object, "bivarPlots", bivarPlots))
    
  }
)
