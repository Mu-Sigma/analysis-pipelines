######################################################################################################
# Title: Utility functions for working with Spark through R
# Author: Naren Srinivasan, Anoop S
# Created on: August 24, 2018
# Description: Functions to work with Spark, incuding Structured Streaming
######################################################################################################


#' @name sparkRSessionCreateIfNotPresent
#' @title Connect to a Spark session
#' @details Loads the SparkR package and intializes a Spark session from R
#' @param ... Arguments to sparkR.session
#' @family Spark utilities
#' @examples
#' \dontrun{
#' sparkHome <- "/Users/naren/softwares/spark-2.3.1-bin-hadoop2.7/"
#' sparkMaster <- "local[1]"
#' sparkPackages <- c("org.apache.spark:spark-sql-kafka-0-10_2.11:2.3.1")
#' sparkRSessionCreateIfNotPresent(master = sparkMaster,
#'  sparkPackages = sparkPackages)
#' }
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

  SparkR::sparkR.session(...)
}

#' @name castKafkaStreamAsString
#' @title Connect to a Spark session
#' @details Takes in a Structured Stream from Kafka created from \code{read.stream(source = 'kafka', ...)} and returns
#' a Structured Streaming DataFrame where the \code{key} and \code{value} from the Kafka stream are cast to string
#' @param streamObj Spark Structured Streaming DataFrame returned by \code{read.stream} function with \code{source = 'kafka'}
#' @return Updated Spark Structured Streaming DataFrame with key, value, topic and timestamp from the Kafka stream
#' @family Spark utilities
#' @export

castKafkaStreamAsString <- function(streamObj){
  streamObj <- SparkR::selectExpr(streamObj, "CAST(key AS STRING)", "CAST(value AS STRING)","topic","timestamp")
  return(streamObj)
}

#' @name convertKafkaValueFromJson
#' @title Connect to a Spark session
#' @details Takes in a Structured Stream from Kafka created from \code{read.stream(source = 'kafka', ...)} and returns
#' a Structured Streaming DataFrame where the \code{key} and \code{value} from the Kafka stream are cast to string
#' @param streamObj Spark Structured Streaming DataFrame which is returned by the \code{castKafkaStreamAsString} function
#' @param schema A structType object created from SparkR specifying the schema of the json data present in the \code{value}
#' attribute of the incoming Kafka stream
#' @return Spark Structured Streaming DataFrame with the json data in the \code{value} attribute of the Kafka stream parsed
#' into a DataFrame format
#' @family Spark utilities
#' @export

convertKafkaValueFromJson <- function(streamObj, schema){
  streamObj <- SparkR::select(streamObj, SparkR::from_json(streamObj$value,
                                           schema = schema))
  return(streamObj)
}

