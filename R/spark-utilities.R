######################################################################################################
# Title: Utility functions for working with Spark through R
# Version: 18.08.01
# Created on: August 24, 2018
# Description: Functions to work with Spark, incuding Structured Streaming
######################################################################################################

#' @import SparkR

#' @name sparkRSessionCreateIfNotPresent
#' @title
#' @details
#' @details
#' @slot input The input dataset on which analysis is to be performed
#' @slot
#' @family Package core functions
#' @export

sparkRSessionCreateIfNotPresent <- function(...){

  if(Sys.getenv("SPARK_HOME") == "" && sparkHome == ""){
    stop("SPARK_HOME environment variable is not set on the system, and sparkHome argument is empty")
  }

  if(!("SparkR" %in% installed.packages())){
    stop("SparkR package not installed. Please install from the $SPARK_HOME folder")
  }

  if(sparkHome == ""){
    .libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
    sparkHome <- Sys.getenv("SPARK_HOME")
  }else{
    .libPaths(c(file.path(sparkHome, "R", "lib"), .libPaths()))
  }

  library(SparkR)
  sparkR.session(...)
}


castKafkaStreamAsString <- function(streamObj){
  streamObj <- selectExpr(streamObj, "CAST(key AS STRING)", "CAST(value AS STRING)","topic","timestamp")
  return(streamObj)
}

convertKafkaValueFromJson <- function(streamObj, schema){
  streamObj <- select(streamObj, from_json(streamObj$value,
                                           schema = schema))
  return(streamObj)
}

