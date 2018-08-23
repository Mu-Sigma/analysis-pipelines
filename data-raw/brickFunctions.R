##################################################################################################
# Title: Predefined functions as part of package
# Version: 18.07.01
# Created on: August 23, 2018
# Description: Reproducible code to generate list of predefined functions
##################################################################################################

brickFunctions <- data.frame(functionName = c("univarCatDistPlots"), heading = c("Univariate Distribution Categorical"), outAsIn = c(FALSE), stringsAsFactors = F)

brickFunctions %>>% dplyr::add_row(functionName = "outlierPlot",
                                   heading = "Univariate Outlier",
                                   outAsIn = FALSE)           -> brickFunctions
brickFunctions %>>% dplyr::add_row(functionName = "multiVarOutlierPlot",
                                   heading = "Multivariate Outlier",
                                   outAsIn = FALSE)           -> brickFunctions
brickFunctions %>>% dplyr::add_row(functionName = "ignoreCols",
                                   heading = "",
                                   outAsIn = TRUE)            -> brickFunctions

devtools::use_data(brickFunctions, internal = TRUE)


