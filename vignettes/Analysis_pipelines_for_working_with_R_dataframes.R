## ----sourcing------------------------------------------------------------
library(analysisPipelines)

## ----creating object, warning=F------------------------------------------
obj <- AnalysisPipeline(filePath = system.file("hotel_new.csv", package = "analysisPipelines")) 
class(obj)

## ----printing object contents, warning=F---------------------------------
obj %>>% getInput %>>% str
obj %>>% getRegistry

## ----pipe demo 1, warning=F----------------------------------------------
# Running univariate categorical distribution plot on the constructed object

obj1 <- obj %>>% univarCatDistPlots(uniCol = "building_type", priColor = "blue", optionalPlots = 0, storeOutput = T)
obj1 %>>% getPipeline

## ----pipe demo 2, warning=F----------------------------------------------
# Running univariate categorical distribution plot and then 
# outlier detection on the constructed object

obj %>>% univarCatDistPlots(uniCol = "location_type", priColor = "xy", optionalPlots = 0, storeOutput = T) %>>% 
         outlierPlot(method = "iqr", columnName = "Occupancy", 
              cutoffValue = 0.01, priColor = "blue", optionalPlots = 0, storeOutput = T) -> obj2
obj2 %>>% getPipeline

## ----lazy eval 1---------------------------------------------------------
length(obj1@output)

