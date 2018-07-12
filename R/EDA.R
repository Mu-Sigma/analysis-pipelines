###################################################
# Title: Functions for various EDA operations
# Version: 18.07.01
# Created on: June 14, 2018
# Description: Consolidated file for all functions
#                to be used for EDA out of the box
###################################################

# source("core-functions.R")

########################
# FUNCTION DEFINITIONS #
########################

ignoreCols <- function(data, columns){
  tryCatch({
    if(all(columns %in% colnames(data))){
      return(data[, setdiff(colnames(data), columns), drop = F])
    }else{
      mismatch <- colnames(data)[!all(columns %in% colnames(data))]
      stop(paste0("Columns ", paste0(mismatch, collapse = ", "), " are not present in the dataset"))
    }
  }, error = function(e){
    stop(e)
  }, warning = function(e){
    warning(e)
  })
}

# Univariate Categoric Distribution function
univarCatDistPlots <- function(data, uniCol, priColor,optionalPlots){
  levels(data[[uniCol]]) <- c(levels(data[[uniCol]]), "NA")
  data[[uniCol]][is.na(data[[uniCol]])] <- "NA"
  data <- data %>% dplyr::group_by_(.dots = c(uniCol)) %>% dplyr::summarise(count = n())
  y=data[[uniCol]]
  catPlot <- ggplot2::ggplot(data,
                             ggplot2::aes(x = reorder(y, count), y=count)) +
    ggplot2::geom_bar(stat = "identity",  fill = priColor,alpha=0.7) +
    ggplot2::xlab(uniCol) +
    ggplot2::ylab("Frequency") + ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 16),panel.grid.major.y=ggplot2::element_blank(),panel.border=ggplot2::element_rect(size=0.1)
    ) +
    ggplot2::coord_flip()
  if(optionalPlots){
    catPlot <- plotly::plot_ly(y = y, x=data[["count"]],type="bar",orientation='h',color = I(priColor)) %>%
      plotly::layout(title=paste0("Frequency Histogram for ",uniCol),
                     xaxis=list(title = "Frequency"),
                     yaxis=list(title = uniCol))


  }
  return(catPlot)
}

#Outlier Plot Function
outlierPlot <- function(data,method,columnName,cutoffValue, priColor,optionalPlots){
  if(method == "iqr"){
    outlierPlotObj <- ggplot2::ggplot(data, ggplot2::aes(x="", y = data[,columnName])) +
      ggplot2::geom_boxplot(fill = priColor,alpha=0.7) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),panel.grid.minor.x=ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank(),legend.position = "bottom") +ggplot2::ylab(columnName) + ggplot2::xlab("")

  }
  if(method == "percentile"){
    Outlier<-data$Outlier
    Value<-data[,columnName]
    outlierPlotObj <- ggplot2::ggplot(data) +
      ggplot2::geom_histogram(ggplot2::aes(x = Value, fill = Outlier),bins=30,alpha=0.7) +
      ggplot2::scale_fill_manual(values = c(priColor, "red"),breaks=c("FALSE", "TRUE"),
                                 labels=c("Normal", "Outlier"),name = "Status") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),panel.grid.minor.x=ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank(),legend.position = "bottom") +
      ggplot2::xlab(columnName)

  }
  if(method == "z_score"){
    data$zScore <- scale(data[,columnName],center = T, scale = T)
    Zscore<-as.vector(data$zScore)
    y<-data[,columnName]
    outlierPlotObj <-
      ggplot2::ggplot(data, ggplot2::aes(x = Zscore, y = y)) +
      ggplot2::geom_point(ggplot2::aes(color = Outlier),alpha=0.7)+
      ggplot2::scale_color_manual("Status", values = c("TRUE" = "red","FALSE" =priColor))+
      ggplot2::ylab(columnName)+
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),panel.grid.minor.x=ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank(),legend.position = "bottom") +
      ggplot2::xlab("Z-score")+
      ggplot2::geom_vline(xintercept = (cutoffValue),linetype = "dashed")+
      ggplot2::geom_vline(xintercept = -(cutoffValue), linetype = "dashed")

  }
  #conditionToBe
  if(optionalPlots)
  {
    outlierPlotObj <- plotly::ggplotly(outlierPlotObj)
    outlierPlotObj$x$layout$margin$l <- outlierPlotObj$x$layout$margin$l + 30
    outlierPlotObj$x$layout$margin$b <- outlierPlotObj$x$layout$margin$b + 3

  }

  return(outlierPlotObj)

}

#Mutlivariate Outlier Plot Function
multiVarOutlierPlot <- function(data,depCol,indepCol,sizeCol, priColor,optionalPlots){
  x<-data[,indepCol]
  y<-data[,depCol]
  size<-data[,sizeCol]
  outlierPlot <- ggplot2::ggplot(data,ggplot2::aes(x = x,y = y),alpha=0.6)+
    ggplot2::geom_point(ggplot2::aes(color = Outlier, size = size),alpha=0.7)+
    ggplot2::scale_color_manual("",values = c("Outlier" = "red", "Normal" = priColor))+
    ggplot2::labs(title = paste(depCol,"vs",indepCol)) +  ggplot2::theme_bw() +
    ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),panel.grid.minor.x=ggplot2::element_blank(),legend.position = "bottom") +
    ggplot2::ylab(depCol) +
    ggplot2::xlab(indepCol)
  #conditionToBe
  if(optionalPlots)
  {
    outlierPlot <- plotly::ggplotly(outlierPlot,tooltip=c("all"))
    outlierPlot$x$layout$margin$l <- outlierPlot$x$layout$margin$l + 30
    outlierPlot$x$layout$margin$b <- outlierPlot$x$layout$margin$b + 3
  }
  return(outlierPlot)
}
