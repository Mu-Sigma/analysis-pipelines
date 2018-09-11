######################################################################################################
# Title: Functions for various EDA operations on data frames
# Version: 18.08.01
# Created on: June 14, 2018
# Description: Consolidated file for all functions
#                to be used for EDA out of the box
######################################################################################################


########################
# FUNCTION DEFINITIONS #
########################
#' @name ignoreCols
#' @title Ignores the columns in the loaded dataframe object
#' @details The columns selected are removed from the object
#' @param data the dataframe object that needs to be loaded
#' @param columns the names of columns to be ignored from dataframe object
#' @return Updated dataframe object
#' @family Package EDA Utilites functions
#' @export
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
#' @name univarCatDistPlots
#' @title Univariate Categoric Distribution
#' @details A univariate distribution graph on the selected categorical columns from the dataframe
#' @param uniCol the name of column on which the plot needs to be generated
#' @param priColor the primary color for the plots
#' @param optionalPlots A Flag for optional plots
#' @return A univariate categoric distribution plot
#' @family Package EDA Utilites functions
#' @export
univarCatDistPlots <- function(data, uniCol, priColor,optionalPlots){
  levels(data[[uniCol]]) <- c(levels(data[[uniCol]]), "NA")
  data[[uniCol]][is.na(data[[uniCol]])] <- "NA"
  data <- data %>% dplyr::group_by_(.dots = c(uniCol)) %>% dplyr::summarise(count = dplyr::n())
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
#' @name outlierPlot
#' @title Outlier detection plot
#' @details Outlier are to be identified on the selected column from the dataframe
#' @param data the dataframe that needs to be loaded
#' @param method the method on which outliers are to be identified
#' @param columnName the name of column for which the outliers are identified
#' @param cutoffValue the cut off value to define the threshold for outliers
#' @param priColor the primary color for the plots
#' @param optionalPlots A Flag for optional plots
#' @return Outliers plot object
#' @family Package EDA Utilites functions
#' @export
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
#' @name multiVarOutlierPlot
#' @title Multi-Variate Outlier Plot
#' @details Multivaraite outlier plot using the selected columns from the dataframe
#' @param data the dataframe that needs to be loaded
#' @param depCol the name of column which is to be identified as dependent column
#' @param indepCol the name of an independent column
#' @param sizeCol the name of column used to define the size of point in plots
#' @param priColor the primary color for the plots
#' @param optionalPlots A Flag for optional plots
#' @return Outliers plot
#' @family Package EDA Utilites functions
#' @export
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

## Bivariate Plots
#' @name bivarPlots
#' @title Bi-Variate Plot
#' @details A bivariate distribution graph on the selected columns from the dataframe.Selected two columns are on two axis' and a plot is generated
#' @param dataset the dataframe that needs to be loaded
#' @param select_var_name_1 the name of first column on which the plot needs to be generated
#' @param select_var_name_2 the name of second column on which the plot needs to be generated
#' @param priColor the primary color for the plots
#' @param secColor A secondary color for the plots
#' @return Bivariate plot
#' @family Package EDA Utilites functions
#' @export
bivarPlots <- function(dataset, select_var_name_1, select_var_name_2, priColor = "blue", secColor= "black") {

  numeric_cols <- unlist(getDatatype(dataset)['numeric_cols'])
  cat_cols <- unlist(getDatatype(dataset)['cat_cols'])

  if (select_var_name_1 %in% numeric_cols && select_var_name_2 %in% numeric_cols) {
    x = dataset[, select_var_name_1]
    y = dataset[, select_var_name_2]
    bivarPlot <-
      ggplot2::ggplot(dataset, ggplot2::aes(x, y)) +
      ggplot2::geom_point(color = priColor, alpha = 0.7) +
      ggplot2::geom_smooth(method = lm, color = secColor) +
      ggplot2::xlab(select_var_name_1) +
      ggplot2::ylab(select_var_name_2) + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste(
        'Bivariate plot for',
        select_var_name_1,
        'and',
        select_var_name_2,
        sep = ' '
      )) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
        axis.text = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 10)
      )



  } else if (select_var_name_1 %in% cat_cols &&
             select_var_name_2 %in% cat_cols) {
    new_df <- dataset %>% dplyr::group_by_(.dots=c(select_var_name_1,select_var_name_2)) %>% dplyr::summarise(n = dplyr::n())
    colfunc <- grDevices::colorRampPalette(c(priColor, "white" , secColor))
    colorvar <- length(unique(new_df[[select_var_name_2]]))
    a=as.vector(as.character(unique(new_df[[select_var_name_1]])))
    y=new_df[[select_var_name_1]]
    label=new_df[[select_var_name_2]]
    bivarPlot <-ggplot2::ggplot(new_df, ggplot2::aes(x = y, y= n, fill = label)) +
      ggplot2::geom_bar(position = "dodge", stat = "identity",alpha=0.9) +
      ggplot2::guides(fill=ggplot2::guide_legend(title=select_var_name_2)) +
      ggplot2::coord_flip()+
      ggplot2::xlab(select_var_name_1) +
      ggplot2::ylab("count") + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste('Bivariate plot for',select_var_name_1,'and',select_var_name_2,sep=' '))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),axis.text = ggplot2::element_text(size=10),
                     axis.title=ggplot2::element_text(size=10),legend.position="bottom",axis.text.x=ggplot2::element_text(angle=45, hjust=1))+ ggplot2::scale_fill_manual(values = colfunc(colorvar))


  } else {
    cols <- c(select_var_name_1, select_var_name_2)
    cat_col <- cols[which(cols %in% cat_cols)]
    num_col <- cols[which(cols %in% numeric_cols)]
    a = as.vector(as.character(unique(dataset[[cat_col]])))
    y = dataset[[cat_col]]
    x = dataset[[num_col]]
    bivarPlot <-
      ggplot2::ggplot(dataset, ggplot2::aes(x = y, y = x)) +
      ggplot2::geom_point(color = priColor, alpha = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::xlab(cat_col) +
      ggplot2::ylab(num_col) + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste(
        'Bivariate plot for',
        select_var_name_1,
        'and',
        select_var_name_2,
        sep = ' '
      )) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
        axis.text = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 10)
      )
  }

  return(bivarPlot)
}

## Correlation Matrix
#' @name correlationMatPlot
#' @title Correlation Matrix Plot
#' @description A correlation matrix is created and plotted across all the columns in the dataset
#' @param dataset the dataset that needs to be loaded
#' @return Correlation Matrix graph
#' @family Package EDA Utilites functions
#' @export
correlationMatPlot <- function(dataset, methodused = "everything"){
  numeric_cols <- getDatatype(dataset)['numeric_cols']
  cormatrix <- base::round(stats::cor(dataset[,unlist(numeric_cols),drop=F], use = methodused),3)
  return(R.devices::capturePlot(corrplot::corrplot(cormatrix,
                                                   method = "color",
                                                   outline = T,
                                                   addgrid.col = "darkgray",
                                                   # order="hclust",
                                                   addrect = 4,
                                                   rect.col = "black",
                                                   rect.lwd = 5,
                                                   cl.pos = "b",
                                                   tl.col = "black",
                                                   tl.cex = 1,
                                                   cl.cex = 1.5,
                                                   addCoef.col = "black",
                                                   number.digits = 2,
                                                   number.cex = 0.75,
                                                   type = "lower",
                                                   col = grDevices::colorRampPalette(c("red","white","green"))(200))))
}


##################
# MISC FUNCTIONS #
##################

## Return the column type

#' @name CheckColumnType
#' @title Check for type of column
#' @details Checking for type of columns in the datavector
#' @param dataVector a data vector of a column
#' @return column Type
#' @family Package EDA Utilites functions
#' @export
CheckColumnType <- function(dataVector) {
  #Check if the column type is "numeric" or "character" & decide type accordDingly
  if (class(dataVector) == "integer" || class(dataVector) == "numeric") {
    columnType <- "numeric"
  } else { columnType <- "character" }
  #Return the result
  return(columnType)
}

## Get numeric and categoric
#' @name getDatatype
#' @title Get Data Type
#' @details Based on the datatype the columns are seperated into categorical and numerical columns
#' @param dataset a dataset which needs to be loaded
#' @return list with \code{numeric_cols} and \code{cat_cols}
#' @family Package EDA Utilites functions
#' @export
getDatatype <- function(dataset){
  numeric_cols <- colnames(dataset)[unlist(sapply(dataset,FUN = function(x){ CheckColumnType(x) == "numeric"}))]
  cat_cols <- colnames(dataset)[unlist(sapply(dataset,FUN = function(x){CheckColumnType(x) == "character"|| CheckColumnType(x) == "factor"}))]
  return(list("numeric_cols"=numeric_cols , "cat_cols"=cat_cols))
}
