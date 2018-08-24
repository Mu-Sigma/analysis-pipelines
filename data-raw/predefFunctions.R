##################################################################################################
# Title: Predefined functions as part of package
# Version: 18.07.01
# Created on: August 23, 2018
# Description: Reproducible code to generate list of predefined functions
##################################################################################################

##################################################################################################
# Working with data frames
##################################################################################################

##################################################################################################
# EDA
##################################################################################################
dfPredefFunctions <- data.frame(functionName = c("univarCatDistPlots"),
                                heading = c("Univariate Distribution Categorical"),
                                outAsIn = c(FALSE), stringsAsFactors = F)

dfPredefFunctions %>>% dplyr::add_row(functionName = "outlierPlot",
                                   heading = "Univariate Outlier",
                                   outAsIn = FALSE)           -> dfPredefFunctions
dfPredefFunctions %>>% dplyr::add_row(functionName = "multiVarOutlierPlot",
                                   heading = "Multivariate Outlier",
                                   outAsIn = FALSE)           -> dfPredefFunctions
dfPredefFunctions %>>% dplyr::add_row(functionName = "ignoreCols",
                                   heading = "",
                                   outAsIn = TRUE)            -> dfPredefFunctions

##################################################################################################

##################################################################################################
# Working with Spark DataFrames, and Spark Structured Streaming
##################################################################################################

##################################################################################################
# Kafka Streams as input
##################################################################################################

sparkPredefFunctions <- data.frame(functionName = c("castKafkaStreamAsString"),
                                        heading = c(""),
                                        outAsIn = c(TRUE), stringsAsFactors = F)

sparkPredefFunctions %>>% dplyr::add_row(functionName = "convertKafkaValueFromJson",
                                       heading = "",
                                       outAsIn = TRUE)           -> sparkPredefFunctions

# sparkPredefFunctions %>>% dplyr::add_row(functionName = "printStream",
#                                        heading = "",
#                                        outAsIn = TRUE)           -> sparkPredefFunctions


devtools::use_data(dfPredefFunctions, sparkPredefFunctions, internal = TRUE, overwrite = T)
