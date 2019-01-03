#' analysisPipelines
#'
#' The package aims at enabling data scientists to compose pipelines of analysis which consist of data manipulation,
#' exploratory analysis & reporting, as well as modeling steps. It also aims to enable data scientists to use tools
#' of their choice through an R interface, and compose interoperable pipelines between R, Spark, and Python.
#' Credits to Mu Sigma for supporting the development of the package.
#'
#' @note To enable pipelines involving Spark tasks, the package uses the 'SparkR' package. Using Spark as an engine requires the SparkR package to be installed.
#' SparkR is distributed natively with Apache Spark and is not distributed on CRAN. The SparkR version needs to directly map to the Spark version (hence the native distribution), and care needs to be taken to ensure that this is configured properly.
#' To install from Github, run the following command, if you know the Spark version:
#' \itemize{
#' \item devtools::install_github('apache/spark@v2.x.x', subdir='R/pkg')
#' }
#' The other option is to install SparkR by running the following terminal commands if Spark has already been installed:
#' \itemize{
#' \item $ export SPARK_HOME=/path/to/spark/directory
#' \item $ cd $SPARK_HOME/R/lib/SparkR/
#' \item $ R -e "devtools::install('.')"
#' }
#' @docType package
#' @name analysisPipelines
NULL
