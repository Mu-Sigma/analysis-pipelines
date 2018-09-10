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
                                outAsIn = c(FALSE),
                                engine = "r",
                                stringsAsFactors = F)

batchPredefFunctions %>>% dplyr::add_row(functionName = "outlierPlot",
                                   heading = "Univariate Outlier",
                                   outAsIn = FALSE,
                                   engine = "r")           -> batchPredefFunctions
batchPredefFunctions %>>% dplyr::add_row(functionName = "multiVarOutlierPlot",
                                   heading = "Multivariate Outlier",
                                   outAsIn = FALSE,
                                   engine = "r")           -> batchPredefFunctions
batchPredefFunctions %>>% dplyr::add_row(functionName = "ignoreCols",
                                   heading = "",
                                   outAsIn = TRUE,
                                   engine = "r")            -> batchPredefFunctions

##################################################################################################

##################################################################################################
# Working with Spark Structured Streaming
##################################################################################################

##################################################################################################
# Kafka Streams as input
##################################################################################################

structuredStreamingPredefFunctions <- data.frame(functionName = c("castKafkaStreamAsString"),
                                        heading = c(""),
                                        outAsIn = c(TRUE), stringsAsFactors = F)

structuredStreamingPredefFunctions %>>% dplyr::add_row(functionName = "convertKafkaValueFromJson",
                                       heading = "",
                                       outAsIn = TRUE)           -> structuredStreamingPredefFunctions

# sparkPredefFunctions %>>% dplyr::add_row(functionName = "printStream",
#                                        heading = "",
#                                        outAsIn = TRUE)           -> sparkPredefFunctions


devtools::use_data(batchPredefFunctions, structuredStreamingPredefFunctions, internal = TRUE, overwrite = T)
