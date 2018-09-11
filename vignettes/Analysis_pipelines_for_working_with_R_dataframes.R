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

obj <- obj %>>% univarCatDistPlots(uniCol = "building_type", priColor = "blue", optionalPlots = 0)
obj %>>% getPipeline

## ----pipe demo 2, warning=F----------------------------------------------
# Running univariate categorical distribution plot and then 
# outlier detection on the constructed object

obj <- obj %>>% 
  univarCatDistPlots(uniCol = "location_type", priColor = "blue", optionalPlots = 0) %>>% 
  outlierPlot(method = "iqr", columnName = "Occupancy", 
              cutoffValue = 0.01, priColor = "blue", optionalPlots = 0)
obj %>>% getPipeline

## ----lazy eval 1---------------------------------------------------------
length(obj@output)

## ----lazy eval 2, warning=F----------------------------------------------
obj1 <- obj %>>% generateOutput
length(obj1@output)

## ----lazy eval 2.5, warning=F--------------------------------------------
length(obj@output)

## ----lazy eval 3, warning=F----------------------------------------------
# The index can range from 1 to length(obj@output)
obj1 %>>% getOuputByOrderId(3)

## ----current register, warning=FALSE-------------------------------------
# Currently registered functions
obj %>>% getRegistry

## ----bivariate definition------------------------------------------------
bivariatePlots <- function(object, select_var_name_1, select_var_name_2, 
                       priColor = "blue", secColor='black') {
  x=object[, select_var_name_1]
  y=object[, select_var_name_2]
  bivarPlot <- ggplot2::ggplot(object, ggplot2::aes(x,y)) +
    ggplot2::geom_point(color=priColor,alpha=0.7) +
    ggplot2::geom_smooth(method = lm,color=secColor) +
    ggplot2::xlab(select_var_name_1) +
    ggplot2::ylab(select_var_name_2) + 
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste('Bivariate plot for', select_var_name_1, 
                           'and', select_var_name_2, sep=' ')) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10), 
                   axis.text = ggplot2::element_text(size=10),
                   axis.title=ggplot2::element_text(size=10))
  return(bivarPlot)
}

## ----register function, warning=F----------------------------------------
# The first parameter provides the function name
# The second parameter allows for a header that will feature in the report
obj <- obj %>>% registerFunction('bivariatePlots', "Bivariate Plots")

# Printing the updated registry
obj %>>% getRegistry

## ----register function 2, warning=F--------------------------------------
# Chaining the user-defined function to the object's pipeline where it was registered
obj <- obj %>>% 
  bivariatePlots(select_var_name_1 = 'Occupancy', select_var_name_2 = 'max_rooms_capacity', 
                 priColor = "blue", secColor = "black")

# Printing the updated pipeline
obj %>>% getPipeline

## ----register function 3, warning=F--------------------------------------
obj2 <- obj %>>% generateOutput()
obj2 %>>% getOuputByOrderId(4)

## ----generate report and tabs, warning=F,  eval=F------------------------
#  # generateReport() needs a destination path as an argument
#  # The function writes a HTML file with a name in the format 'EDA_report_[timestamp].html'
#  obj2 %>>% generateReport('~/Desktop')

## ----generate report and tabs 2, message=FALSE, warning=FALSE, eval=FALSE----
#  obj <- obj %>>% bivariatePlots('Occupancy', 'PercentTransientNights',
#                             priColor = "blue", secColor = "black")
#  
#  obj %>>% generateReport('~/Desktop')

## ----save pipelines, message=FALSE, warning=FALSE, eval=TRUE-------------
# Saves the pipeline and registry of the EDA object
savePipeline(obj, 'pipeline.RDS')

## ----load pipelines, message=FALSE, warning=FALSE, eval=T----------------
obj2 <- loadPipeline('pipeline.RDS',filePath = system.file("hotel_new.csv", package = "analysisPipelines")) 

obj2 %>% getRegistry
obj2 %>% getPipeline

