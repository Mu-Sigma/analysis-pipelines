######################################################################
# Title: Analysis Journey - Approach 1 - S4 Class + Pipeline operator
# Author: Naren Srinivasan
# Version: 18.06.01
# Created on: June 13, 2018
# Description: An approach for pipelining independent operations
#              used in data science to create reports
#######################################################################


# Packages
#install.packages("tidyverse")
library(tidyverse)
library(future)
library(promises)

#' @decription This class forms the prototype of any report,
#'              storing all operations performed on input data and the corresponding putput
#' @slot input The input dataset on which analysis is to be performed
#' @slot output A tibble which holds the name of the operations and the corresponding result

.AnalysisReport <- setClass("AnalysisReport", 
                            slots = c( env = "environment",
                                       #input = "data.frame",
                                       output = "tbl")
                            )
# The constructor for the "AnalysisReport" class
setMethod(
  f = "initialize",
  signature = "AnalysisReport",
  definition = function(.Object, dataset)
  {
    .Object@env$dataset  <- dataset
    .Object@output   <- tibble(analysis = character(), 
                               promise = list(),
                               result = list(),
                               resultClass = character())
    return(.Object)
  }
)

setGeneric(name = "getSum", function(object){
  standardGeneric("getSum")
})

setMethod("getSum",
          signature = "AnalysisReport",
          function(object){
  
            getSummary <- function(dataset){
              sum <- as.tibble(summary(dataset))
              return(sum)
            }
            
          #dataset <- analysisReportObject@input
          object@output %>% add_row(analysis = "generateSummary",
                                          promise = list(future(get("getSummary")(object@input), 
                                                                lazy = T)),
                                          result = list("unfulfilled"),
                                          resultClass = tableClass ) -> object@output
          return(object)
          })
####### Reconstruct promises ###################################

# Either need to reconstruct promises (OR)
# Define the methods as method in the class -> so wrapper methods should be auto generated


#### Collect outputs #############################################
#' @decription This function generates outputs based on promises
#' @param analysisReportObject An initialized object of the class 'AnalysisReport' with dataset
#'                             on results are to be collected
#' @return The outputs as a list
collectOutput <- function(analysisReportObject)
  {
    res <- list()
    evaluatePromise <- function(x){
      x %>% then(.)
      return(list(x))
    }
   analysisReportObject@output %>% rowwise() %>% mutate(promise = evaluatePromise(promise)) -> analysisReportObject@output
   res <- lapply(analysisReportObject@output$promise, function(x) return(x$result$value))
   names(res) <- analysisReportObject@output$analysis
  return(res)
}

################# Report generation #######################################################
# Result classes

tableClass <- "table"
ggplotClass <- "ggplot"

#' @decription This function generates a report based on a 'AnalysisReport' object
#' @param analysisReportObject An initialized object of the class 'AnalysisReport' with dataset
#'                             on which analysis is to be performed
#' @return The report in html

generateReport <- function(analysisReportObject){
  
  

}

######### Annotation function ############################################################

#' @decription This function is to add annotations to a specific part of the report
#' @param analysisReportObject An initialized object of the class 'AnalysisReport' with dataset
#'                             on which analysis is to be performed
#' @return The input object updated with annotations added

annotateOutput <- function(analysisReportObject){
  
}


########## Example analysis functions######################################################

#' @decription This function generates a summary of the dataset
#' @param analysisReportObject An initialized object of the class 'AnalysisReport' with dataset
#'                             on which analysis is to be performed
#' @return The input object updated with the Summary operation performed and the result stored 

generateSummary <- function(analysisReportObject){
  
  getSummary <- function(dataset){
    sum <- as.tibble(summary(dataset))
    return(sum)
  }
  
  #dataset <- analysisReportObject@input
  analysisReportObject@output %>% add_row(analysis = "generateSummary",
                                           promise = list(future(get("getSummary")(analysisReportObject@input), 
                                                                 lazy = T)),
                                           result = list("unfulfilled"),
                                           resultClass = tableClass ) -> analysisReportObject@output
  return(analysisReportObject)
}

#' @decription This function generates a bivariate plots of 2 numeric variables in the dataset
#' @param analysisReportObject An initialized object of the class 'AnalysisReport' with dataset
#'                             on which analysis is to be performed
#' @param x Numeric variable to be plotted on the x-axis
#' @param y Numeric variable to be plotted on the y-axis
#' @return The input object updated with the Summary operation performed and the result stored 

plotBivariate <- function(analysisReportObject, x, y){
  getBivariatePlot <- function(dataset, x,y){
    p <- ggplot(data=dataset, 
                aes_string(x=x, y=y, group=1)) +
      geom_line()
    return(p)
  }
  
  #dataset <- analysisReportObject@input
  analysisReportObject@output %>% add_row(analysis = "plotBivariate",
                                          promise = list(future(get("getBivariatePlot")(analysisReportObject@input, x, y),
                                                                lazy = T)),
                                          result = list("unfulfilled"),
                                          resultClass = ggplotClass) -> analysisReportObject@output
  return(analysisReportObject)
}
