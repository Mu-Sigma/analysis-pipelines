source('Poc.R')

library(magrittr)

uniCatPlot = function(object = data.frame(),
                      uniCol = 1,
                      priColor = "blue")
{
  dataset <- object
  levels(dataset[[uniCol]]) <- c(levels(dataset[[uniCol]]), "NA")
  dataset[[uniCol]][is.na(dataset[[uniCol]])] <- "NA"
  dataset <- dataset %>% dplyr::group_by_(.dots = c(uniCol)) %>% dplyr::summarise(count = n())
  y <- dataset[[uniCol]]
  catPlot <- plotly::plot_ly(y = y, x=dataset[["count"]],type="bar",orientation='h',color = I(priColor)) %>%
    plotly::layout(title=paste0("Frequency Histogram for ",uniCol),
                   xaxis=list(title = "Frequency"),
                   yaxis=list(title = uniCol))
  
  
  return(catPlot)
  
}



# Bivar Plot




obj <- read_input(filePath = 'hotel_new.csv') %>>% eda_outlierPlot(method = "iqr",columnName = "Occupancy",cutoffValue = 0.01,priColor = "blue",optionalPlots = 1) %>>% eda_univarCatDistPlots(uniCol = "building_type",priColor = "blue",optionalPlots = 1)  

obj1 <- obj %>>% generateOutput()

system.time(obj2 <- obj %>>% generateReport())

system.time(obj2 <- obj1 %>>% generateReport())


