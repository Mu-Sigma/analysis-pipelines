source("EDAUtils.R")

loadGlobals()

lapply(packages, FUN = function(p){
  loadPackage(p)
})

library(testthat)
library(futile.logger)


allResults <- list()
allResultsSummary <- list()

dataset <- read.csv("../JenkinsTestData/hotel_new.csv")
metafile <- list(datapath = "../JenkinsTestData/hotel_metaFile_date.csv")
catCols <- c("STRUC_DESC","LOC_DESC","Class")
numericCols <- colnames(dataset)[!colnames(dataset) %in% catCols]
singularCols <- c("PercentTransientNights", "PercentLeisure", "web_nts_totsty")
plotlyGraphs <- 1

#### Test loadGlobals ####
#### Test loadGlobals ####
testloadGlobals <- function(){
  failedTestResults <- list()
  result <- tryCatch({
  test_that("Global variables are loaded properly",{
    loadGlobals()
    expect_equal(87, projectID)
  })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Load globals test passed")
  } else {
    return(failedTestResults)
  }
}

testloadGlobalsResults <- testloadGlobals()
if(testloadGlobalsResults != 'Load globals test passed') {
  allResults <- c(allResults,'LoadGlobalsTest', testloadGlobalsResults)
}


#### Test replaceSpaces ####
testreplaceSpaces <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    test_that("Replace spaces is correct",{
      output <- replaceSpaces(dataset)
      output <- as.character(output[1, 24])
    expect_equivalent("SMALL_TOWN", output)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Replace spaces test passed")
  } else {
    return(failedTestResults)
  }
}

testreplaceSpacesResults <- testreplaceSpaces()
if(testreplaceSpacesResults != 'Replace spaces test passed') {
  allResults <- c(allResults,'ReplaceSpacesTest', testreplaceSpacesResults)
}

#### Test Dummy Variable creation ####

testDummyVarCreation <- function(){
  failedTestResults <- list()
  data <- data.frame(cbind("Name" = c("Monika", "B", "C", "D", "E", "F", "A"), "Employment_type" = c("Salaried", "Self-Employed", "Business", "Unemployed", "Investor", "Salaried", "Business")))
  referenceLevel = "Investor"
  expected_output <- data.frame(rbind(c(0,1,0,0), c(0,0,1,0),c(1,0,0,0),c(0,0,0,1),c(0,0,0,0),c(0,1,0,0),c(1,0,0,0)))
  colnames(expected_output) <- paste(colnames(data["Employment_type"]),
                                     unique(data['Employment_type'])[unique(data['Employment_type']) != referenceLevel],sep='_')
  
  result <- tryCatch({
    output <- DummyVarCreation(data["Employment_type"])
    test_that("Column dummification is correct",{
      expect_equivalent(output[[1]], expected_output)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Dummy Variable creation test passed")
  } else {
    return(failedTestResults)
  }
}

testDummyVarCreationResults <- testDummyVarCreation()
if(testDummyVarCreationResults != 'Dummy Variable creation test passed') {
  allResults <- c(allResults,'DummyVarCreationtest', testDummyVarCreationResults)
}


#### Test Check Column Type ####
testCheckColumnType <- function(){
  failedTestResults <- list()
  numeric_data <-  c(10,11,12,14,15,17,22,25,50,101,150,56,32,33,41)
  result <- tryCatch({
    test_that("Numeric Class returned correctly",{
      expect_equal(CheckColumnType(numeric_data), "numeric")
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  failedTestResults <- list()
  factor_data <-  c(10,10,10,10,10,10,10,10,150,56,32,33,41)
  result <- tryCatch({
    test_that("Numeric Class for less number of unique values returned correctly",{
      expect_equal(CheckColumnType(factor_data), "numeric")
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  
  character_data <-  c("A","B","C","D","E")
  result <- tryCatch({
    test_that("Character Class returned correctly",{
      expect_equal(CheckColumnType(character_data), "character")
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Check column type test passed")
  } else {
    return(failedTestResults)
  }
}

testCheckColumnTypeResults <- testCheckColumnType()
if(testCheckColumnTypeResults != 'Check column type test passed') {
  allResults <- c(allResults,'CheckColumnTypetest', testCheckColumnTypeResults)
}



#### Test loadMetafile ####
testloadMetafile <- function(){
  failedTestResults <- list()
  expectedOutput <- as.data.frame(matrix(c("Occupancy","numeric",
                                            "RMS_AVAIL_QTY","numeric",
                                           "FAC_ID","factor",
                                           "AvgDailyRate","numeric",
                                           "STRUC_DESC","character",
                                           "LOC_DESC","character",
                                           "Class","character",
                                           "Compet_Occupancy","numeric",
                                           "Compet_AvgDailyRate","numeric",
                                           "PercentGovtNights","numeric",
                                           "PercentGroupNights","numeric",
                                           "PercentTransientNights","numeric",
                                           "PercentBusiness","numeric",
                                           "PercentLeisure","numeric",
                                           "hotelcount1mile","numeric",
                                           "hotelcount5mile","numeric",
                                           "rooms1mile","numeric",
                                           "rooms5mile","numeric",
                                           "club_contribution","numeric",
                                           "cro_nts_totsty","numeric",
                                           "gds_nts_totsty","numeric",
                                           "ids_nts_totsty","numeric",
                                           "inn_nts_totsty","numeric",
                                           "slf_nts_totsty","numeric",
                                           "web_nts_totsty","numeric"),ncol = 2, nrow = 25, byrow= T))
  colnames(expectedOutput) <- c("Column","Column.Type")
  result <- tryCatch({
    test_that("Load Metafile is correct",{
      output <- loadMetafile(metafile, dataset)
      output <- output$typeData[,c("Column","Column.Type")]
      expect_equivalent(output$Column.Type, as.character(expectedOutput$Column.Type))
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Load metafile test passed")
  } else {
    return(failedTestResults)
  } 
}

testloadMetafileResults <- testloadMetafile()
if(testloadMetafileResults != 'Load metafile test passed') {
  allResults <- c(allResults,'LoadMetafileTest', testloadMetafileResults)
}


#### Test useMetaFile ####
testuseMetaFile <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    expectedNumCols <- c("Occupancy","RMS_AVAIL_QTY","AvgDailyRate","Compet_Occupancy","Compet_AvgDailyRate",
                         "PercentGovtNights","PercentGroupNights","PercentTransientNights","PercentBusiness",
                         "PercentLeisure","hotelcount1mile","hotelcount5mile","rooms1mile","rooms5mile",
                         "club_contribution","cro_nts_totsty","gds_nts_totsty","ids_nts_totsty", "inn_nts_totsty",
                         "slf_nts_totsty","web_nts_totsty")
    test_that("Use metadata is correct for Numeric Columns",{
    output <- useMetaFile(dataset, loadMetafile(metafile, dataset)$typeData)
    expect_equivalent(output$numCols, expectedNumCols)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  result <- tryCatch({
    expectedCatCols <- c("FAC_ID","STRUC_DESC","LOC_DESC","Class")
    test_that("Use metadata is correct for Categoric Columns",{
      output <- useMetaFile(dataset, loadMetafile(metafile, dataset)$typeData)
      expect_equivalent(output$numCols, expectedNumCols)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Use Metafile test passed")
  } else {
    return(failedTestResults)
  }
}

testuseMetaFileResults <- testuseMetaFile()
if(testuseMetaFileResults != 'Use Metafile test passed') {
  allResults <- c(allResults,'UseMetafileTest', testuseMetaFileResults)
}


#### Test numericStats ####
testNumericStats <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    expectedOutput <- as.data.frame(matrix(c(
      "FAC_ID",858,858,0,1.000,858.000,429.500,429.500,247.828,0.000,1.800,
      "RMS_AVAIL_QTY",858,270,0,54.000,777.000,192.421,173.000,86.350,1.912,8.903,
      "Occupancy",858,856,0,0.000,0.983,0.477,0.457,0.158,0.633,3.551,
      "AvgDailyRate",858,858,0,0.000,259.793,79.053,74.468,22.353,2.628,16.608,
      "Compet_Occupancy",858,848,0,0.000,0.964,0.482,0.473,0.129,0.081,5.410,
      "Compet_AvgDailyRate",858,848,0,0.000,298.969,78.875,75.525,23.673,2.172,19.001,
      "PercentGovtNights",858,844,0,0.000,0.710,0.050,0.031,0.063,3.906,26.601,
      "PercentGroupNights",858,781,0,0.000,0.772,0.151,0.103,0.154,1.407,4.753,
      "PercentTransientNights",858,852,0,0.202,1.000,0.800,0.846,0.157,-1.151,3.964,
      "PercentBusiness",858,858,0,0.002,0.647,0.275,0.271,0.124,0.163,2.634,
      "PercentLeisure",858,858,0,0.353,0.998,0.725,0.729,0.124,-0.163,2.634,
      "hotelcount1mile",858,59,0,0.000,158.000,7.914,4.000,13.779,5.507,44.399,
      "hotelcount5mile",858,133,0,0.000,274.000,32.277,20.000,39.407,2.867,13.219,
      "rooms1mile",858,594,0,0.000,36616.000,1169.845,411.000,3047.156,6.573,55.795,
      "rooms5mile",858,758,0,0.000,69156.000,4547.645,1888.000,8365.118,4.223,25.352,
      "club_contribution",858,856,0,0.000,0.924,0.347,0.331,0.141,0.730,4.131,
      "cro_nts_totsty",858,856,0,0.000,0.446,0.096,0.079,0.063,1.889,8.343,
      "gds_nts_totsty",858,853,0,0.000,0.500,0.071,0.059,0.055,2.448,14.482,
      "ids_nts_totsty",858,845,0,0.000,0.450,0.030,0.014,0.046,4.061,26.358,
      "inn_nts_totsty",858,544,0,0.000,0.340,0.009,0.001,0.035,5.026,30.564,
      "slf_nts_totsty",858,854,0,0.000,1.000,0.708,0.730,0.143,-1.169,6.184,
      "web_nts_totsty",858,854,0,0.000,0.379,0.086,0.080,0.045,1.674,9.226), 
      ncol = 11, nrow = 22, byrow= T))
    colnames(expectedOutput) <- c("ColumnName","N","Unique","Missing","Min","Max","Mean","Median","SD","Skewness","Kurtosis")

    test_that("Numeric Stats calculation is correct",{
      data <- dataset[,numericCols]
      output <- numericStats(data)
      output <- lapply(output, function(x) as.factor(x))
      expect_equivalent(output, expectedOutput)
    })
    }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Numeric Statistics test passed")
  } else {
    return(failedTestResults)
  }
}

testNumericStatsResults <- testNumericStats()
if(testNumericStatsResults != 'Numeric Statistics test passed') {
  allResults <- c(allResults,'NumericStatsTest', testNumericStatsResults)
}


#### Test categoricStats ####
testCategoricStats <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    expectedOutput <- as.data.frame(matrix(c(
      "STRUC_DESC",858,8,0,"2 TO 6 STORY INTERIOR CORRIDORS",
      "LOC_DESC",858,9,0,"SUBURBAN",
      "Class",858,2,0,"bad"
      ), 
      ncol = 5, nrow = 3, byrow= T))
    colnames(expectedOutput) <- c("ColumnName","N","Unique","Missing","Mode")
    
    test_that("Categoric Stats calculation is correct",{
      data <- dataset[,catCols]
      output <- categoricStats(data)
      output <- lapply(output, function(x) as.factor(x))
      expect_equivalent(output, expectedOutput)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Categorical Statistics test passed")
  } else {
    return(failedTestResults)
  }
}

testcategoricStatsResults <- testCategoricStats()
if(testcategoricStatsResults != 'Categorical Statistics test passed') {
  allResults <- c(allResults,'CategoricStatsTest', testcategoricStatsResults)
}


#### Test Observing Categoric Levels ####

testObserveCatLevels <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    expectedOutput <- as.data.frame(matrix(c(
      "STRUC_DESC","2 TO 6 STORY INTERIOR CORRIDORS",259,30.186,
      "STRUC_DESC","" ,252,29.371,
      "STRUC_DESC","2 STORY EXTERIOR CORRIDORS",117,13.636,
      "STRUC_DESC","7 TO 12 STORY HIGH RISE",116,13.520,
      "STRUC_DESC","UNKNOWN",76,8.858,
      "STRUC_DESC","13 OR MORE STORIES",25,2.914,
      "STRUC_DESC","3 TO 6 STORY EXTERIOR CORRIDORS",12,1.399,
      "STRUC_DESC","U-2",1,0.117,
      "LOC_DESC","SUBURBAN",356,41.492,
      "LOC_DESC","SMALL TOWN",152,17.716,
      "LOC_DESC","URBAN",120,13.986,
      "LOC_DESC","HIGHWAY",115,13.403,
      "LOC_DESC","AIRPORT",58,6.760,
      "LOC_DESC","RESORT",50,5.828,
      "LOC_DESC","DOWNTOWN",5,0.583,
      "LOC_DESC","NO LOCATION",1,0.117,
      "LOC_DESC","OTHER COMMUNITY",1,0.117,
      "Class","bad",531,61.888,
      "Class","good",327,38.112
    ), 
    ncol = 4, nrow = 19, byrow= T))
    colnames(expectedOutput) <- c("Column Name","Level","Frequency","Frequency %")
    
    test_that("Observing Categoric Levels is correct",{
      data <- dataset[,catCols]
      output <- observeCatLevels(data)
      output <- lapply(output, function(x) as.factor(x))
      expect_equivalent(output, expectedOutput)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Observe Categoric Levels test passed")
  } else {
    return(failedTestResults)
  }
}

testObserveCategoricLevelsResults <- testObserveCatLevels()
if(testObserveCategoricLevelsResults != 'Observe Categoric Levels test passed') {
  allResults <- c(allResults,'ObserveCategoricLevelsTest', testObserveCategoricLevelsResults)
}



#### Test missingValue ####

testMissingValue <- function(){
  failedTestResults <- list()
  
  result <- tryCatch({
    expected_output <- data.frame(c("Name", "State"), c(86.666666667, 73.333333333))
    colnames(expected_output) <- c("Column Name", "% of Missing Values")
    test_that("Missing value calculation is correct",{
      data <- data.frame("SN" = 1:15, "Age" = c(21:35), "Name" = c("John","Dora", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), "Salary" = seq(35000, 105000, by=5000), State = c("AP", "UP", "MP", "Gujarath", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
      output <- missingValue(data = data, missingValues = c("NA","blank"), lower = 50, upper = 100)
      columns <- output[,1]
      missing_value_percent <- output[,2]
      expect_equivalent(output, expected_output)
    })
  },error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Missing values test passed")
  } else {
    return(failedTestResults)
  }
}

testMissingValueResults <- testMissingValue()
if(testMissingValueResults != 'Missing values test passed') {
  allResults <- c(allResults,'MissingValueTest', testMissingValueResults)
}


#### Test univarNumDistPlots ####
testunivarNumDistPlots <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    test_that("Univariate Numeric Plot is correct",{
      expect_equal(class(univarNumDistPlots(data = dataset, "RMS_AVAIL_QTY", "blue", "grey50", plotlyGraphs)[[1]]), c("plotly","htmlwidget"))
    })
  },error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Univariate Numeric Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testunivarNumDistPlotsResults <- testunivarNumDistPlots()
if(testunivarNumDistPlotsResults != 'Univariate Numeric Plot test passed') {
  allResults <- c(allResults,'Univariate Numeric Plot Test', testunivarNumDistPlotsResults)
}

#### Test univarCatDistPlots ####
testunivarCatDistPlots <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    test_that("Univariate Categoric Plot is correct",{
      expect_equal(class(univarCatDistPlots(data = dataset, "LOC_DESC", "blue", plotlyGraphs)), c("plotly","htmlwidget"))
    })
  },error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Univariate Categoric Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testunivarcatDistPlotsResults <- testunivarCatDistPlots()
if(testunivarcatDistPlotsResults != 'Univariate Categoric Plot test passed') {
  allResults <- c(allResults,'Univariate Categoric Plot Test', testunivarcatDistPlotsResults)
}


#### Test Outlier Statistics ####

testOutlierstatistics <- function(method){
  failedTestResults <- list()
  
  result <- tryCatch({
    if(method == "percentile"){
      expected_output <- as.data.frame(rbind(c("Salary", 35000, 35700, 38500, 42000, 52500, 70000, 87500, 98000, 101500, 104300, 105000), c("Age", 21, 21.14, 21.7, 22.4, 24.5, 28, 31.5, 33.6, 34.3, 34.86, 35)))
      colnames(expected_output) <- c("Column Name","P_00","P_01","P_05","P_10", "P_25","P_50","P_75","P_90","P_95","P_99","P_100")
    }
    
    if(method == "iqr"){
      expected_output <- as.data.frame(rbind(c("Salary", 52500, 87500, 35000, 0, 140000, -52500, 192500), c("Age", 24.5, 31.5, 7, 14, 42, 3.5, 52.5)))
      colnames(expected_output) <- c("Column Name","25%","75%","IQR","-1.5*IQR", "+1.5*IQR","-3*IQR","+3*IQR")
    }
    
    if(method == "z_score"){
      expected_output <- as.data.frame(rbind(c("Salary", 70000, 22360.680, 92360.680, 114721.360, 137082.040, 47639.320, 25278.640, 2917.960), c("Age", 28, 4.472, 32.472, 36.944, 41.416, 23.528, 19.056, 14.584)))
      colnames(expected_output) <- c("Column Name","Mean","SD","+1 SD","+2 SD", "+3 SD","-1 SD","-2 SD","-3 SD")
    }
    
    test_that("Outlier statistics calculation is correct",{
      data <- data.frame("Salary" = seq(35000, 105000, by=5000), "Age" = c(21:35))
      output <- outlierStatistic(data, method)
      output <- as.data.frame(sapply( output, as.factor ))
      expect_equivalent(output, expected_output)
      
    })
  },error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  result <- tryCatch({
    if(method == "percentile"){
      expected_output <- as.data.frame(rbind(c("Salary", 35000, 35700, 38500, 42000, 52500, 70000, 87500, 98000, 101500, 104300, 105000), c("Age", 21, 21.12, 21.6, 22.2, 24, 27, 30, 33.4, 34.4, 34.88, 35)))
      
      colnames(expected_output) <- c("Column Name","P_00","P_01","P_05","P_10", "P_25","P_50","P_75","P_90","P_95","P_99","P_100")
    }
    
    if(method == "iqr"){
      expected_output <- as.data.frame(rbind(c("Salary", 52500, 87500, 35000, 0, 140000, -52500, 192500), c("Age", 24, 30, 6, 15, 39, 6, 48)))
      colnames(expected_output) <- c("Column Name","25%","75%","IQR","-1.5*IQR", "+1.5*IQR","-3*IQR","+3*IQR")
    }
    
    if(method == "z_score"){
      expected_output <- as.data.frame(rbind(c("Salary", 70000, 22360.680, 92360.680, 114721.360, 137082.040, 47639.320, 25278.640, 2917.960), c("Age", 27.308, 4.404, 31.712, 36.116, 40.52, 22.904, 18.50, 14.096)))
      colnames(expected_output) <- c("Column Name","Mean","SD","+1 SD","+2 SD", "+3 SD","-1 SD","-2 SD","-3 SD")
    }
    
    test_that("Outlier statistics ignores NA values in the data",{
      data <- data.frame("Salary" = seq(35000, 105000, by=5000), "Age" = c(21,22,23,24,25,26,NA,27,28,29,30,NA,31,34,35))
      output <- outlierStatistic(data, method)
      output <- as.data.frame(sapply( output, as.factor ))
      expect_equivalent(output, expected_output)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Outlier statistics test passed")
  } else {
    return(failedTestResults)
  }
}

testOutlierStatisticsResults <- testOutlierstatistics("percentile")
if(testOutlierStatisticsResults != 'Outlier statistics test passed') {
  allResults <- c(allResults,'OutlierStatisticstest', testOutlierStatisticsResults)
}

testOutlierStatisticsResults <- testOutlierstatistics("iqr")
if(testOutlierStatisticsResults != 'Outlier statistics test passed') {
  allResults <- c(allResults,'OutlierStatisticstest', testOutlierStatisticsResults)
}

testOutlierStatisticsResults <- testOutlierstatistics("z_score")
if(testOutlierStatisticsResults != 'Outlier statistics test passed') {
  allResults <- c(allResults,'OutlierStatisticstest', testOutlierStatisticsResults)
}

#### Test Outlier detection ####

testOutlierdetection <- function(method){
  failedTestResults <- list()
  
  result <- tryCatch({
    if(method == "percentile"){
      expected_output <- c(1,2,3,10,11,12)
    }
    
    if(method == "iqr"){
      expected_output <- c(10,11)
    }
    
    if(method == "z_score"){
      expected_output <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    }
    
    test_that("Outlier detection is correct",{
      data <- data.frame("Salary" = seq(35000, 105000, by=5000), "Age" = c(10,11,12,14,15,17,22,25,50,101,150,56,32,33,41))
      output <- outlierDetection(data, method, "Age", 0.2 )
      output_vals <- which(output$Outlier == TRUE)
      expect_equal(output_vals, expected_output)
    })
  },error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  result <- tryCatch({
    test_that("Outlier detection removes NA values in the data",{
      data <- data.frame("Salary" = seq(35000, 105000, by=5000), "Age" = c(21,22,23,24,25,26,NA,27,28,29,30,NA,31,34,35))
      output <- outlierDetection(data, method, "Age", 0.3)
      output_NA <- which(output$Outlier == TRUE)
      data <- data.frame("Salary" = seq(35000, 95000, by=5000), "Age" = c(21,22,23,24,25,26,27,28,29,30,31,34,35))
      output <- outlierDetection(data, method, "Age", 0.3)
      expect_equal(nrow(output), nrow(data)-sum(is.na(data$Age)))
      output_WONA <- which(output$Outlier == TRUE)
      expect_equal(output_NA, output_WONA)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Outlier detection test passed")
  } else {
    return(failedTestResults)
  }
}

testOutlierDetectionResults <- testOutlierdetection("percentile")
if(testOutlierDetectionResults != 'Outlier detection test passed') {
  allResults <- c(allResults,'OutlierDetectiontest', testOutlierDetectionResults)
}

testOutlierDetectionResults <- testOutlierdetection("iqr")
if(testOutlierDetectionResults != 'Outlier detection test passed') {
  allResults <- c(allResults,'OutlierDetectiontest', testOutlierDetectionResults)
}


testOutlierDetectionResults <- testOutlierdetection("z_score")
if(testOutlierDetectionResults != 'Outlier detection test passed') {
  allResults <- c(allResults,'OutlierDetectiontest', testOutlierDetectionResults)
}


#### Test Outlier Plot function ####

testOutlierPlot <- function(method){
  failedTestResults <- list()
  data <- data.frame("Salary" = seq(35000, 105000, by=5000), "Age" = c(10,11,12,14,15,17,22,25,50,101,150,56,32,33,41),"Outlier"=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE))
  result <- tryCatch({
    test_that("Correct class for plots",{
      expect_equal(class(outlierPlot(data, method, "Age", 0.9, "grey50", plotlyGraphs)), c("plotly","htmlwidget"))
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Outlier Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testOutlierPlotResults <- testOutlierPlot("percentile")
if(testOutlierPlotResults != 'Outlier Plot test passed') {
  allResults <- c(allResults,'OutlierPlottest', testOutlierPlotResults)
}

testOutlierPlotResults <- testOutlierPlot("iqr")
if(testOutlierPlotResults != 'Outlier Plot test passed') {
  allResults <- c(allResults,'OutlierPlottest', testOutlierPlotResults)
}

testOutlierPlotResults <- testOutlierPlot("z_score")
if(testOutlierPlotResults != 'Outlier Plot test passed') {
  allResults <- c(allResults,'OutlierPlottest', testOutlierPlotResults)
}

#### Test Multivariate outlier detection #####
set.seed(1)
testMultiVarOutlier <- function(){
  failedTestResults <- list()
  expected_output <- data.frame(rbind(c(-12.0409, 0, 0, 24, 22, "Dora", 40000, "UP", "Outlier")), stringsAsFactors = FALSE, row.names = NULL)
  colnames(expected_output) <- c("Studentized Residuals","P-Value", "P-Value(Bonferroni Correction)", "SN", "Age", "Name", "Salary", "State", "Outlier")
  
  data <- data.frame("SN" = 23:37, "Age" = c(21:35), "Name" = c("John","Dora", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), "Salary" = seq(35000, 105000, by=5000), State = c("AP", "UP", "MP", "Gujarath", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  result <- tryCatch({
    test_that("Multivariate outliers identified correctly",{
      output <- multiVarOutlier(data, "Salary", c("Age"), 0.0000001)[[2]]
      expect_equal(round(as.numeric(output["Studentized Residuals"]),3), round(as.numeric(expected_output["Studentized Residuals"]),3), tolerance = 1e-6)
      expect_equal(as.numeric(output["P-Value"]), as.numeric(expected_output["P-Value"]), tolerance = 1e-7)
      expect_equal(as.numeric(output["P-Value(Bonferroni Correction)"]), as.numeric(expected_output["P-Value(Bonferroni Correction)"]), tolerance = 1e-7)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Multi Variate outlier test passed")
  } else {
    return(failedTestResults)
  }
}

testMultiVarOutlierResults <- testMultiVarOutlier()
if(testMultiVarOutlierResults != 'Multi Variate outlier test passed') {
  allResults <- c(allResults,'MultiVarOutliertest', testMultiVarOutlierResults)
}

#### Test Multi-Var Outlier Plot ####
testMultiVarOutlierPlot <- function(){
  failedTestResults <- list()
  data <- data.frame("Salary" = seq(35000, 105000, by=5000), "Age" = c(10,11,12,14,15,17,22,25,50,101,150,56,32,33,41),"Outlier"=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE))
  result <- tryCatch({
    test_that("Correct class for plots",{
      expect_equal(class(multiVarOutlierPlot(data, "Salary", "Age", "Age", "grey50",plotlyGraphs)), c("plotly","htmlwidget"))
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Multi Var Outlier Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testMultiVarOutlierPlotResults <- testMultiVarOutlierPlot()
if(testMultiVarOutlierPlotResults != 'Multi Var Outlier Plot test passed') {
  allResults <- c(allResults,'MultiVarOutlierPlottest', testMultiVarOutlierPlotResults)
}


#### Test getCorrMat ####
testgetCorrMat <- function(){
  failedTestResults <- list()
  expectedOutput <- matrix( c(1.000, 1.000, 0.055, 1.000, 1.000, 0.055, 0.055, 0.055, 1.000), nrow=3, ncol=3) 
  rownames(expectedOutput) <- c("SN", "Age", "col")
  colnames(expectedOutput) <- c("SN", "Age", "col")
  result <- tryCatch({
    data <- data.frame("SN" = 23:37, "Age" = c(21:35), "col" = c(1,10,0,1,4,7,4,8,1,3,9,1,3,4,5))
    test_that("Correlation Matrix is correct",{
      expect_equivalent(getCorrMat(data), expectedOutput)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Correlation Matrix test passed")
  } else {
    return(failedTestResults)
  }
}

testgetCorrMatResults <- testgetCorrMat()
if(testgetCorrMatResults != 'Correlation Matrix test passed') {
  allResults <- c(allResults,'CheckCorMatTest', testgetCorrMatResults)
}

#### Test Correlation Matrix Plot ####
testCorMatPlot <- function(){
  failedTestResults <- list()
  data <- cor(dataset[,numericCols])
  result <- tryCatch({
    test_that("Correct class for Correlation Matrix plots",{
      expect_equal(class(correlationMatPlot(data[,numericCols])), c("matrix"))
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Correlation Matrix Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testCorMatPlotResults <- testCorMatPlot()
if(testCorMatPlotResults != 'Correlation Matrix Plot test passed') {
  allResults <- c(allResults,'CheckCorMatPlotTest', testCorMatPlotResults)
}

#### Test Bivariate Plot ####
testBivarPlots <- function(var1, var2, type1, type2){
  failedTestResults <- list()
  result <- tryCatch({
    test_that("Correct class for plots",{
      expect_equal(class(bivarPlot(dataset, var1, var2, type1, type2, "blue", "grey50")), c("gg","ggplot"))
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Bivariate Plot test passed")
  } else {
    return(failedTestResults)
  }
}


testBivariatePlotResults <- testBivarPlots(numericCols[1], numericCols[2], "numeric", "numeric")
if(testBivariatePlotResults != 'Bivariate Plot test passed') {
  allResults <- c(allResults,'BivariatePlottest', testBivariatePlotResults)
}

testBivariatePlotResults <- testBivarPlots(catCols[1], catCols[2],"character", "character")
if(testBivariatePlotResults != 'Bivariate Plot test passed') {
  allResults <- c(allResults,'BivariatePlottest', testBivariatePlotResults)
}

testBivariatePlotResults <- testBivarPlots(numericCols[1], catCols[1],"numeric", "character")
if(testBivariatePlotResults != 'Bivariate Plot test passed') {
  allResults <- c(allResults,'BivariatePlottest', testBivariatePlotResults)
}

#### Test freqFun ####
testfreqFun <- function(){
  failedTestResults <- list()
  data <- data.frame(cbind("Name" = c("Monika", "Moni", "Monikaa"), 
                           "Employment_type" = c("Salaried", "Self-Employed", "Business"),
                           "Country" = c("India", "US", "India")))
  referenceLevel = "Investor"
  expected_output <- as.data.frame(matrix(c("Moni", "Business"   , "India"   ,    0,       0, 0.0,
                                               "Monika", "Business"   , "India"   ,    0,       0, 0.0,
                                               "Monikaa", "Business"   , "India"   ,    1,     100, 0.5,
                                               "Moni", "Business"   ,    "US",    0,       0, 1.0,
                                               "Monika", "Business"   ,    "US",    0,       0, 1.0,
                                               "Monikaa", "Business"   ,    "US",    0,       0, 1.0,
                                               "Moni", "Salaried"   , "India"   ,    0,       0, 0.0,
                                               "Monika", "Salaried"   , "India"   ,    1,     100, 0.5,
                                               "Monikaa", "Salaried"   , "India"   ,    0,       0, 1.0,
                                               "Moni",      "Salaried"   ,    "US",    0,       0, 1.0,
                                               "Monika",    "Salaried"   ,    "US",    0,       0, 1.0,
                                               "Monikaa",    "Salaried"   ,    "US",    0,       0, 1.0,
                                               "Moni", "Self-Employed"   , "India"   ,    0,       0, 0.0,
                                               "Monika", "Self-Employed"   , "India"   ,    0,       0, 0.0,
                                               "Monikaa", "Self-Employed"   , "India"   ,    0,       0, 0.0,
                                               "Moni", "Self-Employed"   ,    "US",    1,     100, 0.5,
                                               "Monika", "Self-Employed"   ,    "US",    0,       0, 1.0,
                                               "Monikaa", "Self-Employed"   ,    "US",    0,       0, 1.0), 
                                          nrow = 18, ncol = 6, byrow = T))
  colnames(expected_output) <- c("x", "y", "z", "Freq", "percent", "pos")
  expected_output[, 4:6] <- sapply(expected_output[,4:6], as.character)
  expected_output[, 4:6] <- sapply(expected_output[,4:6], as.numeric)

  result <- tryCatch({
    output <- freq.fun(data$Name, data$Employment_type, data$Country)
    test_that("Frequency calculation is correct",{
      expect_equivalent(output[1:6], expected_output)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  if(length(failedTestResults) == 0){
    return("Frequency function test passed")
  } else {
    return(failedTestResults)
  }
}

testfreqFunResults <- testfreqFun()
if(testfreqFunResults != 'Frequency function test passed') {
  allResults <- c(allResults,'FrequencyFunctiontest', testfreqFunResults)
}


testgetCorrMatResults <- testgetCorrMat()
if(testgetCorrMatResults != 'Correlation Matrix test passed') {
  allResults <- c(allResults,'CheckCorMatTest', testgetCorrMatResults)
}

#### test corNetPlot ####

testcorNetPlot <- function(){
  failedTestResults <- list()
  expected_output <- as.data.frame(matrix(c( "RMS_AVAIL_QTY", "RMS_AVAIL_QTY", 1.00000000,
                                            "Occupancy", "RMS_AVAIL_QTY", 0.07328674,
                                            "AvgDailyRate", "RMS_AVAIL_QTY", 0.14524093,
                                            "RMS_AVAIL_QTY",     "Occupancy", 0.07328674,
                                            "Occupancy",     "Occupancy", 1.00000000,
                                            "AvgDailyRate",     "Occupancy", 0.27224522,
                                            "RMS_AVAIL_QTY",  "AvgDailyRate", 0.14524093,
                                            "Occupancy", "AvgDailyRate", 0.27224522,
                                            "AvgDailyRate",  "AvgDailyRate", 1.00000000
  ), nrow = 9, ncol = 3, byrow = T))
  colnames(expected_output) <- c("X1", "X2", "value")
  expected_output$value <- as.numeric(as.character(expected_output$value))
  data <- data.frame("SN" = 23:37, "Age" = c(21:35), "col" = c(1,10,0,1,4,7,4,8,1,3,9,1,3,4,5))
  remove_missing_values_corrnet <- list(flag = TRUE)
  results <- corNetPlot(dataset, c("RMS_AVAIL_QTY", "Occupancy", "AvgDailyRate"), "Numeric", c(), remove_missing_values_corrnet, 0.2)
  result1 <- tryCatch({
    test_that(" Class of correlation network plot is correct",{
      expect_equal(class(results$correlationNetworkData), "qgraph")
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result1)){
    failedTestResults <- c(failedTestResults, as.character(result1))
  }
  
  result2 <- tryCatch({
    test_that("Correlation Network is correct",{
      expect_equal(expected_output$value, results$correlationNetworkDataDownload$value)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result2)){
    failedTestResults <- c(failedTestResults, as.character(result2))
  }
  
  if(length(failedTestResults) == 0){
    return("Correlation Network test passed")
  } else {
    return(failedTestResults)
  }
}


testcorNetPlotResults <- testcorNetPlot()
if(testcorNetPlotResults != 'Correlation Network test passed') {
  allResults <- c(allResults,'CorNettest', testcorNetPlotResults)
}

#### Test Factor Plot ####
# testfactorPlot <- function(var1, var2, type1, type2){
#   failedTestResults <- list()
#   result <- tryCatch({
#     test_that("Correct class for plots",{
#       expect_equal(class(factorPlot(dataset,c("FAC_ID"),catCols,"FAC_ID","FAC_ID","#000000","#000000",plotlyGraphs)), c("plotly","htmlwidget"))
#     })
#   }, error = function(e){return(e)})
#   if(!isTRUE(result)){
#     failedTestResults <- c(failedTestResults, as.character(result))
#   }
#   
#   if(length(failedTestResults) == 0){
#     return("factorPlot test passed")
#   } else {
#     return(failedTestResults)
#   }
# }
# 
# 
# testfactorPlotResults <- testfactorPlot()
# if(testfactorPlotResults != 'factorPlot test passed') {
#   allResults <- c(allResults,'factorPlot', testfactorPlotResults)
# }


#### Test preProcessing ####
testpreprocessing <- function(var1, var2, type1, type2){
  failedTestResults <- list()
  typedata <- read.csv(metafile$datapath)
  results <- preprocessing(dataset, typedata, c("STRUC_DESC"))
  result <- tryCatch({
    test_that("Preprocessing is correct",{
      expect_equal(colnames(results$dataset_dummified), c("STRUC_DESC_", "STRUC_DESC_13.OR.MORE.STORIES",
                                                                               "STRUC_DESC_2.STORY.EXTERIOR.CORRIDORS"     
                                                                               ,"STRUC_DESC_2.TO.6.STORY.INTERIOR.CORRIDORS"
                                                                               ,"STRUC_DESC_3.TO.6.STORY.EXTERIOR.CORRIDORS"
                                                                               ,"STRUC_DESC_7.TO.12.STORY.HIGH.RISE"        
                                                                               ,"STRUC_DESC_U.2"                            
                                                                               ,"STRUC_DESC_UNKNOWN"))
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  result1 <- tryCatch({
    expected_output <- as.data.frame(matrix(c(  "“UNKNOWN”",
                                                "“UNKNOWN”",
                                                "“UNKNOWN”",
                                                "“UNKNOWN”",
                                                "“UNKNOWN”",
                                                "“UNKNOWN”",
                                                "“UNKNOWN”",
                                                "“UNKNOWN”"), nrow = 8))
    colnames(expected_output) <- c("Reference")
    colnames(results$reference_level) <- c("Reference")
    test_that("Reference Levels are correct",{
      expect_equal(results$reference_level, expected_output)
    })
  }, error = function(e){return(e)})
  if(!isTRUE(result1)){
    failedTestResults <- c(failedTestResults, as.character(result1))
  }
  
  if(length(failedTestResults) == 0){
    return("preprocessing test passed")
  } else {
    return(failedTestResults)
  }
}


testpreprocessingResults <- testpreprocessing()
if(testpreprocessingResults != 'preprocessing test passed') {
  allResults <- c(allResults,'preprocessing', testpreprocessingResults)
}

#### Test runFactorAnalysis ####

testrunFactAnalysis <- function(){
  failedTestResults <- list()
  result <- tryCatch({
    singularCols <- c("PercentTransientNights", "PercentLeisure", "web_nts_totsty")
    output <- runFactorAnalysis(dataset[1:20,1:15],singularCols,catCols,c("Occupancy"),"FAC_ID",3,c("1","2","3"),0.005,0.01)
    
    expectedOutputfac_data <- as.data.frame(matrix(c(
      0.908126255388036,-1.00413822020679,1.93773389856586,-0.482174663859887,-0.566021198819783,0.422857120970744,0.0832520780274117,-0.0728509613514634,-0.850268178190133,
      1.04392083563298,-0.427796888138405,0.374826299634923,0.463488349414165,0.772761086177153,0.973705556598872,-0.833209467510197,-1.52987018838073,-0.850268178190133,
      -0.891151932857418,0.377124989481671,-0.432172892665865,0.833529336679611,-0.887637453711012,-0.932612061391544,-0.0178210607219817,-0.0728509613514634,-0.850268178190133,
      1.00997219057174,2.50156241591027,0.518413901870799,0.778672904385841,0.895825958579549,1.31700896255005,-0.15344755545593,-0.0728509613514634,-0.0772971071081939,
      -0.449819547061364,-0.255637829529643,-0.905963557261606,-1.01803031718692,-0.831428435276621,0.369533531975245,1.62379521088415,-0.0728509613514634,-0.850268178190133,
      -1.53617618902088,-0.38232403694238,-0.883627747850339,-0.781932653024975,0.883239465648681,-0.795965975202151,-1.04221548501725,1.3841682656778,-0.0772971071081939,
      -0.212179031632719,-1.89897112377512,-0.908278254134915,-1.32212219844928,1.08155468442564,2.35618521105904,-1.04203452608888,-0.0728509613514634,-0.850268178190133,
      -0.517716837183833,-0.133821112673022,-0.722707867790148,0.0929509612148879,0.947027570845561,-0.759361658361252,-0.764235296093142,1.3841682656778,-0.0772971071081939,
      0.738383030081861,-0.217793578650062,-0.581694514061755,-0.0409162510207812,-0.93331871893227,0.540090123220805,-0.21600105566501,-1.52987018838073,-0.0772971071081939,
      2.13027747759249,-0.570184884217613,-0.222761232406927,0.932382046306968,-1.448749994917,-0.518341838732528,-0.208111022991722,-1.52987018838073,0.695673963973745,
      1.00997219057174,0.646638595411482,-0.263949834984117,1.10048596971999,-0.26214507308892,0.524027356262502,1.14318383980931,-0.0728509613514634,-0.850268178190133,
      -0.246127676693954,-0.294339987990608,-0.690404485814435,-0.375997856600243,-1.36607874333645,-0.727625343720975,0.18891277079242,-0.0728509613514634,-0.0772971071081939,
      0.908126255388036,-0.686710694439343,1.23153594599847,-0.0823203218993649,0.666605795124963,1.21019317722968,-0.0310302069327643,-0.0728509613514634,-0.850268178190133,
      0.568639804775686,2.17540157326974,0.319657069728503,2.53269840089663,-1.30667797260647,1.03147228582818,0.239292455755372,-0.0728509613514634,-0.850268178190133,
      -0.755357352612478,-0.109001388216963,-1.04331885142875,-0.190148294082868,0.0650827063186024,-0.949431752475728,1.6065603127539,-0.0728509613514634,3.01458717721956,
      -0.280076321755189,-0.912602234233591,-0.56593266199992,-1.33369230960019,1.06260886029018,-0.971933321451827,2.10361107722546,-1.52987018838073,-0.0772971071081939,
      -1.53617618902088,0.138265295284201,-0.109558640809623,-0.0749090467176055,1.66846536663632,-0.842090069603424,-0.338501757944001,1.3841682656778,0.695673963973745,
      -1.264587028531,0.10623433613697,-0.13765718480976,-1.76259201704142,0.890638360634673,-0.971933321451827,0.201996355605194,1.3841682656778,0.695673963973745,
      -0.687460062490008,0.184822781434539,0.329231956553917,0.301290698664302,-1.05334098937509,-0.351633520672033,-0.567071754334866,1.3841682656778,1.46864503505568,
      0.0594101288571612,0.763271992084683,2.75662865366568,0.429337262201138,-0.278411274617699,-0.924144462631831,-1.97692491209747,-0.0728509613514634,0.695673963973745
    ), 
    ncol = 9, nrow = 20, byrow= T))
    colnames(expectedOutputfac_data) <- c("RMS_AVAIL_QTY","AvgDailyRate","Compet_Occupancy","Compet_AvgDailyRate","PercentGovtNights","PercentGroupNights","PercentBusiness","hotelcount1mile","hotelcount5mile")
    
    test_that("Factor Analysis is correct",{
      expectedOutputfac_data <- lapply(expectedOutputfac_data, function(x) as.factor(x))
      outputfac_data <- lapply(output$fac_data, function(x) as.factor(x))
      expect_equivalent(outputfac_data, expectedOutputfac_data)
    })
    
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  result1 <- tryCatch({
    expectedOutputUniq <- as.data.frame(matrix(c(
      1,"RMS_AVAIL_QTY",0.029,0.971,
      2,"AvgDailyRate",0.461,0.539,
      3,"Compet_Occupancy",0.595,0.405,
      4,"Compet_AvgDailyRate",0.005,0.995,
      5,"PercentGovtNights",0.690,0.310,
      6,"PercentGroupNights",0.669,0.331,
      7,"PercentBusiness",0.511,0.489,
      8,"hotelcount1mile",0.316,0.684,
      9,"hotelcount5mile",0.863,0.137
    ), 
    ncol = 4, nrow = 9, byrow= T))
    colnames(expectedOutputUniq) <- c("ID","ColumnName","Uniqueness","Communality")
    
    test_that("Factor analysis uniqueness is correct",{
      
      outputUniq <- lapply(output$uniq, function(x) as.factor(x))
      expect_equivalent(outputUniq, expectedOutputUniq)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result1)){
    failedTestResults <- c(failedTestResults, as.character(result1))
  }
  
  result2 <- tryCatch({
    test_that("Class of Fact Plot is correct",{
      expect_equal(class(output$fact_plot), "qgraph")
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result2)){
    failedTestResults <- c(failedTestResults, as.character(result2))
  }
  
  result3 <- tryCatch({
    test_that("Class of Fact Model is correct",{
      expect_equal(class(output$factor_model), "factanal")
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result3)){
    failedTestResults <- c(failedTestResults, as.character(result3))
  }
  
  if(length(failedTestResults) == 0){
    return("Factor Analysis test passed")
  } else {
    return(failedTestResults,output, expectedOutputUniq)
  }
}

testrunFactAnalysisResults <- testrunFactAnalysis()
if(testrunFactAnalysisResults != 'Factor Analysis test passed') {
  allResults <- c(allResults,'RunFactorAnalysisTest', testrunFactAnalysisResults)
}



####Test singularityRemoval ####
testsingularityRemoval <- function(){
  failedTestResults <- list()
  results <- singularityRemoval("Occupancy", dataset[,numericCols], numericCols, c())
  expected_output <- c("PercentTransientNights", "PercentLeisure", "web_nts_totsty")
  result1 <- tryCatch({
    test_that("Singularity removal is correct",{
      expect_equal(results$rem_sing, expected_output)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result1)){
    failedTestResults <- c(failedTestResults, as.character(result1))
  }
  
  if(length(failedTestResults) == 0){
    return("Singularity Removal test passed")
  } else {
    return(failedTestResults)
  }
}

testsingularityRemovalResults <- testsingularityRemoval()
if(testsingularityRemovalResults != 'Singularity Removal test passed') {
  allResults <- c(allResults,'SingularityRemovaltest', testsingularityRemovalResults)
}


#### Test kMeansKneePlot ####
testkMeansKneePlot <- function(){
  failedTestResults <- list()
  results <- kMeansKneePlot("kmedoids", dataset, "euclidean", c(2,5), 5, "blue")
  result1 <- tryCatch({
    test_that("Class of Kmeans knee plot is correct",{
      expect_equal(class(results$plot), c("gg", "ggplot"))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result1)){
    failedTestResults <- c(failedTestResults, as.character(result1))
  }
  
  result2 <- tryCatch({
    test_that("Kmeans knee plot clusters is correct",{
      expect_equal(results$k, 4)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result2)){
    failedTestResults <- c(failedTestResults, as.character(result2))
  }
  
  if(length(failedTestResults) == 0){
    return("KMeans Knee plot test passed")
  } else {
    return(failedTestResults)
  }
}

testkMeansKneePlotResults <- testkMeansKneePlot()
if(testkMeansKneePlotResults != 'KMeans Knee plot test passed') {
  allResults <- c(allResults,'Kneeplottest', testkMeansKneePlotResults)
}

#### Test generateSilPlot ####
testgenerateSilPlot <- function(){
  failedTestResults <- list()
  data <- data.frame("K" = c(1,2,3,4,5), "Silhouette" = c(0.102, 0.104, 0.110, 0.105, 0.101))
  results <- generateSilPlot(data, 3, "blue")
  result <- tryCatch({
    test_that("Silhoutte Plot class is correct",{
      expect_equal(class(results), c("gg", "ggplot"))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Sil Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testgenerateSilPlotResults <- testgenerateSilPlot()
if(testgenerateSilPlotResults != 'Sil Plot test passed') {
  allResults <- c(allResults,'SilPlottest', testgenerateSilPlotResults)
}


#### Test dataDendogram ####
testdataDendogram <- function(){
  failedTestResults <- list()
  expected_output <- matrix(c( 4,7,1,117, 152,0.4777165,71.76855,0.4545057,  64.60406,0.05635708,0.0000000,  0.3719567,1,8, 23,485, 0.4708804,0.08668172,0.09977427,0.001354402,0.000451467, 0.7512415,
                               3,7,1,360, 163,0.4268293,60.15714,0.4271131,  69.46303,0.03316107,0.2553833,  0.2204996,12,22,917,  1717, 0.2981850,0.08945549,0.05229041,0.024632671,0.004753673, 0.7203976,
                               6,8,2,719, 230,0.7311360,80.64723,0.6322977,  93.74261,0.02281640,0.3128342,  0.2611111,5,61,  1511, 10430, 0.3216134,0.07406746,0.07299661,0.048902374,0.000713903, 0.6812422), 
                                          nrow = 3, byrow = T)
  colnames(expected_output) <- c("STRUC_DESC", "LOC_DESC", "Class", "FAC_ID", "RMS_AVAIL_QTY", "Occupancy", "AvgDailyRate", "Compet_Occupancy", 
                                 "Compet_AvgDailyRate", "PercentGovtNights", "PercentGroupNights", "PercentBusiness", "hotelcount1mile", 
                                 "hotelcount5mile", "rooms1mile", "rooms5mile", "club_contribution", "cro_nts_totsty", "gds_nts_totsty", 
                                 "ids_nts_totsty", "inn_nts_totsty", "slf_nts_totsty")
  dimnames(expected_output) <- list(c("117", "360", "719"))
  results <- dataDendogram(3,catCols,numericCols,singularCols,c(),dataset,"blue", plotlyGraphs)
  result1 <- tryCatch({
    test_that("Dendogram Plot class is correct",{
      expect_equal(class(results$dendoPlot), c("plotly", "htmlwidget"))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result1)){
    failedTestResults <- c(failedTestResults, as.character(result1))
  }
  
  result2 <- tryCatch({
    test_that("Agglomeration Plot class is correct",{
      expect_equal(class(results$agg_plot), c("plotly", "htmlwidget"))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result2)){
    failedTestResults <- c(failedTestResults, as.character(result2))
  }
  
  result3 <- tryCatch({
    resultcom <<- results$pam_fit$medoids
    dimnames(resultcom)[2] <- list(NULL)
    test_that("Clustering data is correct",{
    expect_equal(resultcom, expected_output, tolerance = 0.05)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result3)){
    failedTestResults <- c(failedTestResults, as.character(result3))
  }
  
  if(length(failedTestResults) == 0){
    return("Cluster Dendogram test passed")
  } else {
    return(failedTestResults)
  }
}

testdataDendogramResults <- testdataDendogram()
if(testdataDendogramResults != 'Cluster Dendogram test passed') {
  allResults <- c(allResults,'Dendogramtest', testdataDendogramResults)
}

#### Test DendPlot ####
testclusterDendoplot <- function(){
  failedTestResults <- list()
  center <- resultcom
  rownames(center) <- 1:nrow(center)
  hcluster <- hclust(cluster::daisy(center, metric = "gower"))
  dend <- as.dendrogram(hcluster)
  dend_c <- dendextend::color_branches(dend, k = 3)
  ggdend <- dendextend::as.ggdend(dend_c)
  results <- clusterDendoplot(ggdend, plotlyGraphs)
  result <- tryCatch({
    test_that("Class of dendogram plot is correct",{
      expect_equal(class(results), c("plotly", "htmlwidget"))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Dendogram Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testclusterDendoplotResults <- testclusterDendoplot()
if(testclusterDendoplotResults != 'Dendogram Plot test passed') {
  allResults <- c(allResults,'DendPlottest', testclusterDendoplotResults)
}

#### Test clusterAggPlot ####
testclusterAggPlot <- function(){
  failedTestResults <- list()
  center <- resultcom
  rownames(center) <- 1:nrow(center)
  hcluster <- hclust(cluster::daisy(center, metric = "gower"))
  aggSch <- data.frame(
    index = 1:length(hcluster$height),
    height = sort(hcluster$height,T),
    merge.left = hcluster$merge[,1],
    merge.right = hcluster$merge[,2]
  )
  results <- clusterAggPlot(aggSch,"blue",center, plotlyGraphs)
  result <- tryCatch({
    test_that("Class of agg plot is correct",{
      expect_equal(class(results), c("plotly", "htmlwidget"))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Agglomeration Plot test passed")
  } else {
    return(failedTestResults)
  }
}

testclusterAggPlotResults <- testclusterAggPlot()
if(testclusterAggPlotResults != 'Agglomeration Plot test passed') {
  allResults <- c(allResults,'AggPlottest', testclusterAggPlotResults)
}

#### Test clusterDistData ####
testclusterDistData <- function(){
  failedTestResults <- list()
  expected_output <- as.data.frame(matrix(c("FAC_ID",-0.34,1.126,-0.786,
                                            "RMS_AVAIL_QTY",-0.69,1.147,-0.457,
                                            "STRUC_DESC",-0.577,-0.577,1.155,
                                            "LOC_DESC",0.577,0.577,-1.155,
                                            "Occupancy",-0.1,1.046,-0.946,
                                            "AvgDailyRate",-1.116,0.815,0.301,
                                            "Compet_Occupancy",-0.426,1.143,-0.716,
                                            "Compet_AvgDailyRate",-0.366,1.131,-0.765,
                                            "PercentGovtNights",0.999,0.001,-1.001,
                                            "PercentGroupNights",-1.146,0.45,0.696,
                                            "PercentTransientNights",1.079,-0.896,-0.184,
                                            "PercentBusiness",0.68,-1.148,0.469,
                                            "PercentLeisure",-0.68,1.148,-0.469,
                                            "hotelcount1mile",0.577,-1.155,0.577,
                                            "hotelcount5mile",-0.498,1.151,-0.653,
                                            "rooms1mile",-0.361,1.13,-0.769,
                                            "rooms5mile",-0.468,1.148,-0.68,
                                            "club_contribution",0.116,0.937,-1.053,
                                            "cro_nts_totsty",-1.139,0.732,0.407,
                                            "gds_nts_totsty",-0.621,1.154,-0.533,
                                            "ids_nts_totsty",-0.931,-0.126,1.057,
                                            "inn_nts_totsty",-0.576,-0.578,1.155,
                                            "slf_nts_totsty",0.95,0.094,-1.044,
                                            "web_nts_totsty",-0.384,-0.751,1.135,
                                            "Class",-0.577,1.155,-0.577), nrow = 25, ncol = 4, byrow = T))
  rownames(expected_output) <- c("FAC_ID", "RMS_AVAIL_QTY", "STRUC_DESC", "LOC_DESC", "Occupancy", "AvgDailyRate", "Compet_Occupancy", 
                                 "Compet_AvgDailyRate", "PercentGovtNights", "PercentGroupNights", "PercentTransientNights", "PercentBusiness", "PercentLeisure", "hotelcount1mile", 
                                 "hotelcount5mile", "rooms1mile", "rooms5mile", "club_contribution", "cro_nts_totsty", "gds_nts_totsty", 
                                 "ids_nts_totsty", "inn_nts_totsty", "slf_nts_totsty", "web_nts_totsty", "Class")
  colnames(expected_output) <- c("ColumnName", "cluster1", "cluster2", "cluster3")
  expected_output[, 2:4] <- sapply(expected_output[,2:4], as.character)
  expected_output[, 2:4] <- sapply(expected_output[,2:4], as.numeric)
  pam_fit <- cluster::pam(dataset, diss = FALSE, k = 3, stand = TRUE)
  results <- clusterDistData(pam_fit,"blue",c("cluster1", "cluster2", "cluster3"), plotlyGraphs)
  result <- tryCatch({
    test_that("cluster distance data is correct",{
      expect_equal(results$tcenter, expected_output)
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Cluster distance test passed")
  } else {
    return(failedTestResults)
  }
}

testclusterDistDataResults <- testclusterDistData()
if(testclusterDistDataResults != 'Cluster distance test passed') {
  allResults <- c(allResults,'ClusDisttest', testclusterDistDataResults)
}

#### Test clusterMultiplePlots ####
testclusterMultiplePlots <- function(){
  failedTestResults <- list()
  pam_fit <- cluster::pam(dataset, diss = FALSE, k = 3, stand = TRUE)
  tcenter <- as.data.frame(pam_fit$medoids)
  rownames(tcenter) <- 1:nrow(tcenter)
  is.num <- sapply(tcenter, is.numeric)
  tcenter[is.num] <- lapply(tcenter[is.num], scale, TRUE, TRUE)
  tcenter[is.num] <- lapply(tcenter[is.num], round, 3)
  tcenter <- data.frame(t(tcenter))
  tcenter_num <- tcenter[,-1]
  results <- clusterMultiplePlots(tcenter_num,"blue","white", 1, "plot", plotlyGraphs)
  result <- tryCatch({
    test_that("Class of cluster multiple plots is correct",{
      expect_equal(class(results), c("plotly", "htmlwidget"))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Cluster multiple plots test passed")
  } else {
    return(failedTestResults)
  }
}

testclusterMultiplePlotsResults <- testclusterMultiplePlots()
if(testclusterMultiplePlotsResults != 'Cluster multiple plots test passed') {
  allResults <- c(allResults,'ClusMultiplePlotstest', testclusterMultiplePlotsResults)
}

#### Test clusterSammonsData ####
testclusterSammonsData <- function(){
  failedTestResults <- list()
  # expected_output <- matrix(list(1.466812, -4.584490, 3.117678),list(3.2074859, -0.6874857, -2.5200002), nrow = 3)
  # colnames(expected_output) <- c("X1", "X2")
  # expected_output <- sapply(expected_output, as.numeric(as.character))
  pam_fit <- cluster::pam(dataset, diss = FALSE, k = 3, stand = TRUE)
  results <- clusterSammonsData(dataset,singularCols,c(),catCols,1,pam_fit,c("cluster1", "cluster2", "cluster3"))
  result1 <- tryCatch({
    test_that("Sammons Data class is correct",{
      expect_equivalent(class(results$vor), "SpatialPolygonsDataFrame")
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result1)){
    failedTestResults <- c(failedTestResults, as.character(result1))
  }

  # result2 <- tryCatch({
  #   compare <- results$polygondata[,1:2]
  #   test_that("Sammons polygon data is correct",{
  #     expect_equal(compare, expected_output, tolerance = 0.05)
  #   })
  # }, error = function(e){return(e)})
  # 
  # if(!isTRUE(result2)){
  #   failedTestResults <- c(failedTestResults, as.character(result2))
  # }
  
  result3 <- tryCatch({
    test_that("Sammons cluster size is correct",{
      expect_equal(results$ksize, c(544, 275, 39))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result3)){
    failedTestResults <- c(failedTestResults, as.character(result3))
  }
  
  if(length(failedTestResults) == 0){
    return("Sammons Data test passed")
  } else {
    return(failedTestResults)
  }
}

testclusterSammonsDataResults <- testclusterSammonsData()
if(testclusterSammonsDataResults != 'Sammons Data test passed') {
  allResults <- c(allResults,'SammonsDatatest', testclusterSammonsDataResults)
}


#### Test clusterSammonsPlot ####
testclusterSammonsPlot <- function(){
  failedTestResults <- list()
  pam_fit <- cluster::pam(dataset, diss = FALSE, k = 3, stand = TRUE)
  sammonsData <- clusterSammonsData(dataset,singularCols,c(),catCols,1,pam_fit,c("cluster1", "cluster2", "cluster3"))
  results <- clusterSammonsPlot(sammonsData$scaleddata,sammonsData$kclus,sammonsData$sammonsdata,sammonsData$vor,sammonsData$ksize,sammonsData$polygondata,
                               "blue",c("AvgDailyRate"),1,c("cluster1", "cluster2", "cluster3"))
  result <- tryCatch({
    test_that("Class of Sammons plot is correct",{
      expect_equal(class(results$sammons_plot), c("gg", "ggplot"))
    })
  }, error = function(e){return(e)})

  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }

  if(length(failedTestResults) == 0){
    return("Cluster sammons plot test passed")
  } else {
    return(failedTestResults)
  }
}

testclusterSammonsPlotResults <- testclusterSammonsPlot()
if(testclusterSammonsPlotResults != 'Cluster sammons plot test passed') {
  allResults <- c(allResults,'ClusterSammonsPlottest', testclusterSammonsPlotResults)
}

#### Test downloadClusterTable ####
testdownloadClusterTable <- function(){
  failedTestResults <- list()
  dataset <- na.omit(dataset)
  pam_fit <- cluster::pam(dataset, diss = FALSE, k = 3, stand = TRUE)
  results <- downloadClusterTable(dataset, singularCols, c(), pam_fit, "")
  result <- tryCatch({
    test_that("Downloading cluster data",{
      expect_equal(c(544,275,39), c(sum(results$cluster==1), sum(results$cluster==2), sum(results$cluster==3)))
    })
  }, error = function(e){return(e)})
  
  if(!isTRUE(result)){
    failedTestResults <- c(failedTestResults, as.character(result))
  }
  
  if(length(failedTestResults) == 0){
    return("Download Cluster data test passed")
  } else {
    return(failedTestResults)
  }
}

testdownloadClusterTableResults <- testdownloadClusterTable()
if(testdownloadClusterTableResults != 'Download Cluster data test passed') {
  allResults <- c(allResults,'DownloadClusterTabletest', testdownloadClusterTableResults)
}


#### Logging test results ####
# Generating summary and logging to file
if(length(allResults) == 0) {
  print("All tests passed")
} else {
  print(allResults)
}
