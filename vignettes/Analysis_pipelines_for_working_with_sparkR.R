## ---- include=FALSE------------------------------------------------------
library(ggplot2)
library(analysisPipelines)
library(SparkR)

sparkHome <- "/Users/naren/softwares/spark-2.3.1-bin-hadoop2.7/"
sparkMaster <- "local[1]"
sparkPackages <- c("org.apache.spark:spark-sql-kafka-0-10_2.11:2.3.1")
# Set spark home variable if not present
if(Sys.getenv("SPARK_HOME") == "") {
  Sys.setenv(SPARK_HOME = sparkHome)  
}

## ------------------------------------------------------------------------
sparkRSessionCreateIfNotPresent(master = sparkMaster, sparkPackages = sparkPackages)

## ------------------------------------------------------------------------
pipelineObj <- AnalysisPipeline(filePath = system.file("hotel_new.csv", package = "analysisPipelines"))
# pipelineObj <- AnalysisPipeline(input = SparkR::as.data.frame(system.file("hotel_new.csv", package = "analysisPipelines")))

## ------------------------------------------------------------------------
# Function to calculate avg occupancy for the client as well as competitors - grouped by input parameter
occupancyAnalysis <- function(inputDataset, groupByColumn) {
  occupancySummary <- summarize(groupBy(inputDataset,inputDataset[[groupByColumn]]),
                       avgOccupancy=mean(inputDataset$Occupancy),
                       avgCompetitorOccupancy=mean(inputDataset$Compet_Occupancy))
 return(occupancySummary)
}

# Function to calculate avg booking contribution for differen channels - grouped by input parameter
bookingChannelsAnalysis <- function(inputDataset, groupByColumn) {
  bookingChannelsAnalysisSummary <- summarize(groupBy(inputDataset,inputDataset[[groupByColumn]]),
                       avgCallCenterBookingPct=avg(inputDataset$call_center_booking_pct),
                       avgAgencyBookingPct=avg(inputDataset$travel_agency_booking_pct),
                       avgThirdPartyBookingPct=avg(inputDataset$third_party_web_bookings_pct),
                       avgHotelToHotelBookingPct=avg(inputDataset$hotel_to_hotel_bookings_pct),
                       avgDirectBookingPct=avg(inputDataset$direct_call_booking_pct),
                       avgWebBookingPct=avg(inputDataset$direct_web_booking_pct))
 return(bookingChannelsAnalysisSummary)
}


## ------------------------------------------------------------------------
# Register user defined functions
pipelineObj <- pipelineObj %>>% registerFunction("occupancyAnalysis", "Occupany analysis",
                                                 engine = "spark")  %>>%
  registerFunction("bookingChannelsAnalysis", "Booking channels analysis",
                                                 engine = "spark")
# List al registered functions 
pipelineObj %>>% getRegistry

# Define pipeline from list of registered functions
pipelineObj %>% occupancyAnalysis(groupByColumn = "location_type") %>% bookingChannelsAnalysis(groupByColumn = "location_type") -> pipelineObj

pipelineObj %>>% getPipeline
pipelineObj %>>% visualizePipeline

