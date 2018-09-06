## ---- include=FALSE------------------------------------------------------
library(ggplot2)
library(analysisPipelines)
library(SparkR)
# Set spark home variable if not present
if(Sys.getenv("SPARK_HOME") == "") {
  Sys.setenv(SPARK_HOME = "/home/anoop/software/spark-2.3.1-bin-hadoop2.7/")  
}

