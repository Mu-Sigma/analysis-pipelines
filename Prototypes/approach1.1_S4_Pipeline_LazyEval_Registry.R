######################################################################
# Title: Analysis Journey - Approach 1 - S4 Class + Pipeline operator
#                                         + Lazy evaluation + Registry
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
#' @slot input The environment containing the input dataset on which analysis is to be performed
#' @slot registry A tibble that is a function registry where various analysis operations can be registered
#' @slot recipe A tibble which holds the list of operations to be performed

functionPrefix <- "ar_"

.AnalysisRecipe <- setClass("AnalysisRecipe", 
                            slots = c( input = "environment",
                                       registry = "tbl",
                                       #input = "data.frame",
                                       recipe = "tbl")
                            )
# The constructor for the "AnalysisReport" class
setMethod(
  f = "initialize",
  signature = "AnalysisRecipe",
  definition = function(.Object, dataset)
  {
    .Object@input$dataset  <- dataset
    .Object@registry <- tibble(id = character(),
                               analysisOperation = character(),
                              params = list())
    #                            additionalParams = character(),
    #                            dependencies = character())
    .Object@recipe   <- tibble( registryId = character(),
                                analysisOperation = character(), 
                                paramValues = character())
                                #promise = list(),
                                #resultClass = character())
    return(.Object)
  }
)

createAnalysisRecipeMethod <- function(opId, opName, opAdditionalParams = ""){
   defineGenericCall <- paste0('setGeneric(name = \"', functionPrefix, opName, '\", function(object, opAdditionalParams =  list("")){',
    'standardGeneric(\"','ar_', opName, '\")})')
   
   ### Check against registry is to be added
   
   defineRecipeCall <- paste('setMethod(f = \"', functionPrefix, opName,
                             '\", signature = \"AnalysisRecipe\", definition = function(object, opAdditionalParams = list("")){',
                             'object@recipe %>% add_row(registryId = ', opId, ', analysisOperation = \"', opName, 
                             '\", paramValues = list(opAdditionalParams)) -> object@recipe; return(object);})', sep = ""
                          )
   
  
   eval(parse(text = defineGenericCall))
   eval(parse(text = defineRecipeCall))
}

setGeneric(name = "registerAnalysisOperation", function(object, opId, 
                                                        opName, opAdditionalParams = "",
                                                        opDependencies = ""){
  standardGeneric("registerAnalysisOperation")
})

setMethod("registerAnalysisOperation",
          signature = "AnalysisRecipe",
          function(object, opId, opName, opAdditionalParams = "", opDependencies = ""){
            
            createAnalysisRecipeMethod(opId, opName, opAdditionalParams)
            #dataset <- analysisReportObject@input
            object@registry %>% add_row(id = opId,
                                        analysisOperation = opName,
                                        params = list(opAdditionalParams)) -> object@registry
                                        #additionalParams = opAdditionalParams,
                                        #dependencies = opDependencies) -> object@registry
            return(object)
          })

#### Collect outputs #############################################

setGeneric(name = "collectOutput", function(object){
  standardGeneric("collectOutput")
})

#' @decription This function generates outputs based on promises
#' @param analysisReportObject An initialized object of the class 'AnalysisReport' with dataset
#'                             on results are to be collected
#' @return The outputs as a list
#' 
setMethod("collectOutput",
          signature = "AnalysisRecipe", function(object)
  {
    getOutput <- function(name, adParams){
      op <-list()
      if(adParams != ""){
        params <- append(list(dataset = quote(object@input$dataset)), unlist(adParams, recursive = F))
        op <- list(do.call(name, params))
      }else{
        params <- list(dataset = quote(object@input$dataset))
        op <- list(do.call(name, params))
      }
      return(op)
    }

      object@recipe %>% rowwise(.) %>% mutate(output = getOutput(analysisOperation, paramValues)) -> object@recipe
  return(object)

})

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


