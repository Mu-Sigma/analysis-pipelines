library(testthat)
library(futile.logger)
set.seed(7)


flog.threshold(INFO)
# flog.appender(appender.file("./testCaseSummary.log"))
flog.appender(appender.file("./testCases.log"))
testResults <- as.data.frame(test_file("testCases.R"))

summary <- list()
summary[['Number of tests run']] <- nrow(testResults)#196
summary[['Numeber of tests failed']] <- sum(testResults$error)


flog.info("*************************************************************************************************************")
flog.info("Summary of test cases for this build")
flog.info(knitr::kable(as.data.frame(summary)))


if(sum(testResults$error) > 0){
  # flog.info(unlist(allTestResults))
  stop()
}else{
  print("All tests passed. Pushing build...")
}


#############################################################################################################
#####                               METRIC COVERAGE REPORTING STARTS                                    #####
#############################################################################################################

######## IMPORTANT!!!! WORKING PATH NEEDS TO BE brick_name/Source/ ###############
library("devtools")

testPackageName<-"testcovr"

sourceFileLocation<-"EDAUtils.R"
testFileLocation<-"testCases.R"

# set value according to brick 
# EDA
# Data Wrangling
# Regression
# Classification
# Panel Regression
# Text Mining
# Segmentation
# Quality Check
# SQL

brickName<-"EDA"


##############
# Commented cause aarushi told complex code
##############
total_coverage<-covr::file_coverage(source_files = sourceFileLocation,test_files = testFileLocation)

total_coverage_data<-as.data.frame(total_coverage)
total_coverage_string<-capture.output(total_coverage,file = NULL,type = "message")

unit_test_coverage_percentage<-"0%"
unit_test_coverage_percentage<-strsplit(total_coverage_string[1],split = ": ")[[1]][2]
unit_test_coverage_percentage <- regmatches(unit_test_coverage_percentage, gregexpr(pattern = "(\\d{2,3}.\\d{2}%)", unit_test_coverage_percentage))[[1]]
#unit_test_coverage_percentage<-paste0(as.character(round(percent_coverage(total_coverage),2)),"%")
# unit_test_coverage<-strsplit(unit_test_coverage_percentage,split = "%")[[1]][1]

#### Deleting the test package
# unlink(testPackageName,recursive = T)

number_of_unit_test<-0
number_of_unit_test<-nrow(testResults)

#### Read Number of lines ####


openedFile <- file("EDAUtils.R",open="r")
readsizeof <- 20000
lines_of_code<-0
(while((linesread <- length(readLines(openedFile,readsizeof))) > 0 )
  lines_of_code <- lines_of_code+linesread )
close(openedFile)
lines_of_code = as.character(lines_of_code)

##### upload results to DB ######

## Use RPostgreSQL package to connect into database directly and import tables into dataframes.
library("RPostgreSQL")
## loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
## Open a l
con <- dbConnect(drv, host='172.25.1.30', port='5432', dbname="metric", user='postgres', password='postgres')


unit_coverage<-unit_test_coverage_percentage
issues<-"0"
number_of_int_tests<-"0"
## lint function is masked by devtools
detach("package:devtools", unload=TRUE)
library("lintr")
warnings_lint<-lint("EDAUtils.R")
warnings_data_frame <- as.data.frame(warnings_lint)
warnings_list <- as.list(table(warnings_data_frame$type))
issues<-as.character(warnings_list$warning[1])
## Submits a statement
dbSendQuery(con, paste0("INSERT INTO eoc_brick_metrics VALUES (\'",brickName,"\',\'",lines_of_code,"\',\'",number_of_unit_test,"\',\'",issues,"\',\'",number_of_int_tests,"\',\'",unit_test_coverage_percentage,"\');"))

## Closes the connection
dbDisconnect(con)
## Frees all resources in the driver
dbUnloadDriver(drv)

#############################################################################################################
#####                               METRIC COVERAGE REPORTING ENDS                                      #####
#############################################################################################################

