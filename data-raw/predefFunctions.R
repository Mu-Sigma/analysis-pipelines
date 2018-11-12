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
batchPredefFunctions <- data.frame(functionName = c("univarCatDistPlots"),
                                heading = c("Univariate Distribution Categorical"),
                                # outAsIn = c(FALSE),
                                engine = c("r"),
                                exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                stringsAsFactors = F)

batchPredefFunctions %>>% dplyr::add_row(functionName = "outlierPlot",
                                   heading = "Univariate Outlier",
                                   # outAsIn = FALSE,
                                   engine = "r",
                                   exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))))           -> batchPredefFunctions
batchPredefFunctions %>>% dplyr::add_row(functionName = "multiVarOutlierPlot",
                                   heading = "Multivariate Outlier",
                                   # outAsIn = FALSE,
                                   engine = "r",
                                   exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))))           -> batchPredefFunctions
batchPredefFunctions %>>% dplyr::add_row(functionName = "ignoreCols",
                                   heading = "",
                                   # outAsIn = TRUE,
                                   engine = "r",
                                   exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))))            -> batchPredefFunctions

##################################################################################################

##################################################################################################
# Working with Streaming pipelines - Currently supports Apache Spark Structured Streaming
##################################################################################################

##################################################################################################
# Kafka Streams as input
##################################################################################################

streamingPredefFunctions <- data.frame(functionName = c("castKafkaStreamAsString"),
                                        heading = c(""),
                                        engine = c("spark-structured-streaming"),
                                       exceptionHandlingFunction = c(as.character(substitute(genericPipelineException))),
                                        # outAsIn = c(TRUE),
                                       stringsAsFactors = F)

streamingPredefFunctions %>>% dplyr::add_row(functionName = "convertKafkaValueFromJson",
                                       heading = "",
                                       engine = c("spark-structured-streaming"),
                                       exceptionHandlingFunction = c(as.character(substitute(genericPipelineException)))
                                       # outAsIn = TRUE
                                       )           -> streamingPredefFunctions


devtools::use_data(batchPredefFunctions, streamingPredefFunctions, internal = TRUE, overwrite = T)
