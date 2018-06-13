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

#' @decription This class forms the prototype of any report,
#'              storing all operations performed on input data and the corresponding putput
#' @slot input The input dataset on which analysis is to be performed
#' @slot output A tibble which holds the name of the operations and the corresponding result

.AnalysisReport <- setClass("AnalysisReport", 
                            slots = c(input = "data.frame",
                                       output = "tbl")
                            )
# The constructor for the "AnalysisReport" class
setMethod(
  f = "initialize",
  signature = "AnalysisReport",
  definition = function(.Object, input)
  {
    .Object@input  <- input
    .Object@output   <- tibble(analysis = character(), 
                               result = list(),
                               resultClass = list())
    return(.Object)
  }
)

# Result classes

tableClass <- "table"
ggplotClass <- "ggplot"

#' @decription This function generates a report based on a 'AnalysisReport' object
#' @param analysisReportObject An initialized object of the class 'AnalysisReport' with dataset
#'                             on which analysis is to be performed
#' @return The report in html

generateReport <- function(analysisReportObject){
  
  

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
  
  dataset <- analysisReportObject@input
  analysisReportObject@output %>% add_row(analysis = "generateSummary",
                       result = list(get("getSummary")(dataset)),
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
  
  dataset <- analysisReportObject@input
  analysisReportObject@output %>% add_row(analysis = "plotBivariate",
                              result = list(get("getBivariatePlot")(dataset, x, y)),
                              resultClass = ggplotClass) -> analysisReportObject@output
  return(analysisReportObject)
}
