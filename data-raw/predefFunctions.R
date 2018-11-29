##################################################################################################
# Title: Predefined functions as part of package
# Version: 18.08.01
# Created on: August 23, 2018
# Description: Reproducible code to generate list of predefined functions
##################################################################################################

##################################################################################################
# Working with batch pipelines - data frames in R, Spark or Python
##################################################################################################

##################################################################################################
# EDA
##################################################################################################
.batchPredefFunctions <- data.frame(functionName = c("univarCatDistPlots"),
                                heading = c("Univariate Distribution Categorical"),
                                engine = c("r"),
                                exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                isDataFunction = T,
                                firstArgClass = "",
                                stringsAsFactors = F)

.batchPredefFunctions %>>% dplyr::add_row(functionName = "outlierPlot",
                                   heading = "Univariate Outlier",
                                   # outAsIn = FALSE,
                                   engine = "r",
                                   exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                   isDataFunction = T,
                                   firstArgClass = "")           -> .batchPredefFunctions
.batchPredefFunctions %>>% dplyr::add_row(functionName = "multiVarOutlierPlot",
                                   heading = "Multivariate Outlier",
                                   engine = "r",
                                   exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                   isDataFunction = T,
                                   firstArgClass = "")           -> .batchPredefFunctions
.batchPredefFunctions %>>% dplyr::add_row(functionName = "ignoreCols",
                                   heading = "Ignore Columns",
                                   engine = "r",
                                   exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                   isDataFunction = T,
                                   firstArgClass = "")            -> .batchPredefFunctions
.batchPredefFunctions %>>% dplyr::add_row(functionName = "getFeaturesForPyClassification",
                                          heading = "",
                                          engine = "r",
                                          exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                          isDataFunction = T,
                                          firstArgClass = "")           -> .batchPredefFunctions
.batchPredefFunctions %>>% dplyr::add_row(functionName = "getTargetForPyClassification",
                                          heading = "",
                                          engine = "r",
                                          exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                          isDataFunction = T,
                                          firstArgClass = "")           -> .batchPredefFunctions

##################################################################################################

##################################################################################################
# Working with Streaming pipelines - Currently supports Apache Spark Structured Streaming
##################################################################################################

##################################################################################################
# Kafka Streams as input
##################################################################################################

.streamingPredefFunctions <- data.frame(functionName = c("castKafkaStreamAsString"),
                                        heading = c("Cast Kafka stream to a string"),
                                        engine = c("spark-structured-streaming"),
                                       exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                       isDataFunction = T,
                                       firstArgClass = "",
                                       stringsAsFactors = F)

.streamingPredefFunctions %>>% dplyr::add_row(functionName = "convertKafkaValueFromJson",
                                       heading = "Convert Kafka Value from JSON",
                                       engine = c("spark-structured-streaming"),
                                       exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                       isDataFunction = T,
                                       firstArgClass = ""
                                       )           -> .streamingPredefFunctions


devtools::use_data(.batchPredefFunctions, .streamingPredefFunctions, internal = TRUE, overwrite = T)
