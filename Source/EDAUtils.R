# Function to load the packages
loadPackage <- function(pkgName){
  if (!do.call(require, list(pkgName))) {
    install.packages(pkgName, dependencies = T,
                     repos = "http://cran.us.r-project.org")
    do.call(library, list(pkgName))
  }
}

git_url <- NULL
project_issue_url <- NULL
session_url <- NULL
packages <- NULL
DBPackages <- NULL

projectID <- 87

dbChoices <- c('SQLite'='sqlite_db'
               ,'MySQL'='mysql_db'
               ,'PostgreSQL'='pgsdb'
               ,'Oracle'='oracle'
               ,'SQL server'='sql_server'
               ,'Teradata'='teradata'
               ,'Netezza'='netezza'
               ,'Hive' = 'hive'
               ,'Other'='other')

dbPath <- "qcmodule1-sqlite3.db"
sqlite_drv <- NULL


loadGlobals <- function(){
  packages <<- c("egcm","plyr","plotly","moments","knitr","qgraph","dplyr","corrplot","nFactors","car","reshape2","reshape","sp","gtable", "igraph", 
                 "dummies","grid","gridExtra","cluster","stats","deldir","dendextend", "MASS", "htmltools", "lubridate", "xts", "dygraphs", "tseries", "data.table", "ggplot2")
  dbPackages <<- c("rJava", "RPostgreSQL", "RMySQL", "RSQLite", "DBI","RJDBC", "odbc")
  projectID <<- 87
  git_url <<- "http://mugitlab.mu-sigma.com/api/v3"
  project_issue_url <<- "http://mugitlab.mu-sigma.com/api/v3/projects/87/issues"
  session_url <<- "http://mugitlab.mu-sigma.com/api/v3/session"
  projectID <<- 87
  noOfRowsPlot <<- 10000
  dbChoices <<- c('SQLite'='sqlite_db'
                  ,'MySQL'='mysql_db'
                  ,'PostgreSQL'='pgsdb'
                  ,'Oracle'='oracle'
                  ,'SQL server'='sql_server'
                  ,'Teradata'='teradata'
                  ,'Netezza'='netezza'
                  ,'Hive' = 'hive'
                  ,'Other'='other')
  
  dbPath <<- "qcmodule1-sqlite3.db"

  ## For test cases relative paths
  if(file.exists("Source/packageList.csv")){
    packageList <<- read.csv("Source/packageList.csv")
  } else if(file.exists("packageList.csv")){
    packageList <<- read.csv("packageList.csv")
  } else {
    packageList <<- NULL
  }
}


parseInputs <- function(inputs)
{
  entries <- names(inputs)[grep("_", names(inputs))]
  new_inputs <- list()
  lapply(entries, function(n)
  {
    i <- gregexpr("_", n)[[1]][1]
    op <- substr(n,1,i-1)
    s <- substr(n,i+1,nchar(n))
    if(!(op %in% names(new_inputs)))
    {
      new_inputs[[op]] <<- list()
    }
    new_inputs[[op]][[paste(c(op, s), collapse="_")]] <<- inputs[[n]]
  })
  return(new_inputs)
}

# Replace spaces in categorical columns
replaceSpaces <- function(dataset){
  sapply(colnames(dataset), function(x){
    if(CheckColumnType(dataset[,x]) == "character"){
      a <- dataset[,x]
      dataset[,x] <<- NULL
      col_names <- colnames(dataset)
      a <- gsub(" ","_",a)
      dataset <<- cbind(dataset,a)
      colnames(dataset) <<- c(col_names,x)
    }})
  return(dataset)
}

# Creates dummy variables for string categorical columns of a dataset
DummyVarCreation <- function(columnToConvert, outPath = "", fileName = paste("DummyVariableLog_", deparse(substitute(columnToConvert)), sep = ""), 
                             referenceLevel = NA, cutoffValue = 20) {
  
  
  levelDropVec <- c()
  
  # Checking data Quality
  
  if (length(columnToConvert) > 0 && nrow(columnToConvert) > 0){
    #Creating dummy columns to remove
    dummyColsToRemove <- c()
    
    if(is.na(referenceLevel) || is.null(referenceLevel) || referenceLevel == ""){
      freqDf <- data.frame(table(columnToConvert))
      minFreqVal <- freqDf[which.min(freqDf[, 2]), 1 ]
      minFreqVal <- as.character(minFreqVal)
      referenceLevel <- minFreqVal
    }
    columnName <- names(columnToConvert)
    columnToConvert[[1]] <- as.character(columnToConvert[[1]])
    names(columnToConvert) <- columnName
    dummyColsToRemove <- c(dummyColsToRemove, referenceLevel)
    levelDropVec <- c(levelDropVec, paste(colnames(columnToConvert), sep = "_", referenceLevel))
    
    #creation of dummy variables 
    if(length(unique(columnToConvert[[1]]))>2){
      dummys <- dummies::dummy.data.frame(columnToConvert,names = colnames(columnToConvert), sep = "_")
      #Removal of columns
      dummys <- dummys[, !colnames(dummys) %in% levelDropVec]
    }else{
      # If its a binary column
      dummys <- as.data.frame(as.numeric(columnToConvert != referenceLevel))
      colnames(dummys) <- paste(colnames(columnToConvert),
                                unique(columnToConvert)[unique(columnToConvert) != referenceLevel],sep='_')
    }
  }
  return(list(dummys, referenceLevel))
}


# Return the column type 
CheckColumnType <- function(dataVector) {
  #Check if the column type is "numeric" or "character" & decide type accordDingly
  if (class(dataVector) == "integer" || class(dataVector) == "numeric") {
    columnType <- "numeric"
  } else { columnType <- "character" }
  #Return the result
  return(columnType)
}

# Load metafile
loadMetafile <- function(varTypeFile, dataset){
  alertNoMetaFile <- ""
  typeFileErrorText <- ""
  typeFileErrorTable <- ""
  typeColumnErrorText <- ""
  if (is.null(varTypeFile)){
    typeData <- data.frame()
    alertNoMetaFile <- "Upload a variable type file"
  } else{
    alertNoMetaFile <- ""
    typeData <- read.csv(varTypeFile$datapath, stringsAsFactors = F)
    typeData <- typeData[which(typeData$Column %in% colnames(dataset)), ]
    if(any(!(typeData$Column.Type %in% c("numeric","factor","character")))) {
      typeFileErrorText <- "Error : The uploaded file should be of the format"
      typeFileErrorTable <- data.frame("Column"=c("SampleColumnName1", "SampleColumnName1","SampleColumnName1"),
                                       "Column.Type"=c("numeric", "factor","character"),
                                       "Reference.Level"=c(NA,"Yes","Male"))
    }
    if(!all(colnames(dataset) %in% typeData$Column)) {
      typeColumnErrorText <- "Error : The variable type csv does not have entries for all columns of the dataset"
    }
  }
  return(list(typeData=typeData, colErr=typeColumnErrorText, typeErrorText=typeFileErrorText, 
              typeErrorTable<-typeFileErrorTable, noMetaAlert=alertNoMetaFile))
}

## Use metafile
useMetaFile <- function(dataset, typeData)
{
  datasetNumeric <- as.character(typeData$Column[which(typeData$Column.Type == "numeric")])
  datasetNumericCategorical <- as.character(typeData$Column[which(typeData$Column.Type == "factor")])
  datasetStringCategorical <- as.character(typeData$Column[which(typeData$Column.Type == "character")])
  datasetCategorical <- union(datasetNumericCategorical, datasetStringCategorical)
  # binaryCatNames <- datasetCategorical[lapply(datasetCategorical, 
  #                                             function(i){length(levels(as.factor(dataset[,i])))}) == 2]
  orderedNames <- colnames(dataset)
  if(length(datasetCategorical)>0)
  {
    dataset[,datasetCategorical] <- cbind.data.frame(sapply(dataset[,datasetCategorical,drop=F],
                                                            function(x){as.factor(as.character(x))}), stringsAsFactors=T)
  }
  if(length(datasetNumeric)>0)
  {
    dataset[,datasetNumeric] <- cbind.data.frame(sapply(dataset[,datasetNumeric,drop=F],
                                                        function(x){as.numeric(x)}),stringsAsFactors=T)}
  dataset <- dataset[,orderedNames,drop=F]
  if(is.null(typeData$Reference.Level))
  {
    typeData$Reference.Level <- unlist(lapply(colnames(dataset),
                                              function(x)
                                              {
                                                if(typeData[which(typeData[,"Column"]==x),"Column.Type"]=="factor" ||
                                                   typeData[which(typeData[,"Column"]==x),"Column.Type"]=="character")
                                                {
                                                  names(sort(table(dataset[,x]),decreasing = T)[1])
                                                }else
                                                {
                                                  return(NA)
                                                }
                                              })) #end of unlist
  }#end of if typeData$Reference.Level is NULL
  return(list(data=dataset, typeData=typeData, numCols=datasetNumeric, catCols=datasetCategorical)) #, binCatCols=binaryCatNames))
}

#! Numeric descriptive statistics
numericStats <- function(data){
  num_desc_list <- lapply(data, function(x) {
    
    ## calculating different statistics for numerical continuous variables
    min_val <-  min(x, na.rm = T) %>% round(3)
    max_val <-  max(x, na.rm = T) %>% round(3)
    mean_val <- mean(x, na.rm = T) %>% round(3)
    median_val <- stats::median(x, na.rm = T) %>% round(3)
    sd_val <- stats::sd(x, na.rm = T) %>% round(3)
    unique_val <- unique(x) %>% length()
    no_of_na <- is.na(x) %>% sum()
    no_of_lines <- x %>% length()
    data_skewness <- moments::skewness(x, na.rm = TRUE) %>% round(3)
    data_kurtosis <- moments::kurtosis(x, na.rm = TRUE) %>% round(3)
    
    data.frame(
      "N" = no_of_lines,
      "Unique" = unique_val,
      "Missing" = no_of_na,
      "Min" = min_val,
      "Max" = max_val,
      "Mean" = mean_val,
      "Median" = median_val,
      "SD" = sd_val,
      "Skewness" = data_skewness,
      "Kurtosis" = data_kurtosis
    )
  })
  
  data_sum <- do.call(rbind, num_desc_list)
  data_sum <- cbind(data.frame(ColumnName = rownames(data_sum)), data_sum)
  rownames(data_sum) <- NULL
  return(data_sum)
}

# Categorical Stats
categoricStats <- function(data){
  cat_desc_list <- lapply(data, function(x) {
    
    mat_cat <- table(x)
    mode <- names(mat_cat)[mat_cat == max(mat_cat)]
    mode <- ifelse(length(mode) > 1, "No mode", mode)
    uniq_val <- length(unique(x))
    total_rows <- length(x)
    missing_perc <- round((sum(is.na(x))*100/total_rows), 3)
    missTable <- data.frame(total_rows,
                            uniq_val,
                            round(missing_perc,2),
                            mode)
    colnames(missTable) <- c("N", "Unique", "% of Missing Values", "Mode")
    return(missTable)
  })
  
  data_sum <- do.call(rbind, cat_desc_list)
  data_sum <- cbind(data.frame(ColumnName = rownames(data_sum)), data_sum)
  rownames(data_sum) <- NULL
  return(data_sum)
}

# Observe Categoric Levels
observeCatLevels <- function(data){
  cat_col_desc_list <- lapply(colnames(data), function(i) {
    count_lvl <- data %>%
      dplyr::group_by_(i) %>%
      dplyr::summarise(freq = n())
    j <- sapply(count_lvl, is.factor)
    count_lvl[j] <- lapply(count_lvl[j], as.character)
    count_lvl <- count_lvl %>%
      dplyr::mutate(freq_perc = round((freq / length(data[, i]))*100, 3))
    count_lvl <- cbind(i, count_lvl)
    colnames(count_lvl) <- c("Column Name", "Level", "Frequency", "Frequency %")
    count_lvl <- count_lvl[order(-count_lvl[, 4]), ]
    count_lvl
  })
  
  cat_col_sum <- do.call(rbind, cat_col_desc_list)
  return(cat_col_sum)
}

#! Missing Value Detection Function 
missingValue <- function(data, missingValues, lower = 0, upper = 100){
  missingVal <- sapply(data, function(x){
    (sum(if("NA" %in% missingValues){is.na(x)}, x %in% missingValues)*100)/length(x)
  }, USE.NAMES = F)
  missingVal <- missingVal
  missingVal <- as.data.frame(missingVal)
  missingValDF <- cbind(colnames(data), missingVal)
  missingValDF <- missingValDF[missingValDF$missingVal > lower & missingValDF$missingVal < upper, ]
  colnames(missingValDF) <- c("Column Name", "% of Missing Values")
  rownames(missingValDF) <- NULL
  return(missingValDF)
}

#! Univariate Numeric Distribution function
univarNumDistPlots <- function(data, uniCol, priColor, secColor,optionalPlots){
  # x=data[, uniCol]
  # x <- na.omit(x)
  x=data[, uniCol]
  if(!optionalPlots){
    p1<- ggplot2::ggplot(data,
                         ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ..count..),
        fill = priColor,
        size = 0.2,
        alpha=0.7
      ) +
      ggplot2::xlab(uniCol) +
      ggplot2::ylab("Count") +ggplot2::labs(uniCol) +ggplot2::theme_bw() + 
      # scale_y_continuous(expand = c(0, 0)) +
      ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),legend.position = c(0.8, 0.8),panel.grid.major.x=ggplot2::element_blank()) 
    
    p2<-ggplot2::ggplot(data,
                        ggplot2::aes(x = x)) +
      ggplot2::stat_function(ggplot2::aes(color = "Normal"), fun = dnorm,args = list(mean = mean(data[, uniCol]),sd = sd(data[, uniCol]))) +
      ggplot2::stat_density(ggplot2::aes(color = "Density"), geom = "line", linetype = "dashed")  +
      ggplot2::scale_colour_manual("", values = c(secColor, "#EE7600")) +
      ggplot2::scale_linetype_manual("", 
                                     values = c("Normal" = 2, "Density" = 1))  +
      ggplot2::guides(
        fill = ggplot2::guide_legend(keywidth = 1, keyheight = 1),
        linetype = ggplot2::guide_legend(keywidth = 3, keyheight = 1),
        colour = ggplot2::guide_legend(keywidth = 3, keyheight = 1)
      ) + 
      ggplot2::ylab("Density") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(colour="black", size=8),
        axis.text.x = ggplot2::element_text(size = 14),
        axis.ticks = ggplot2::element_line(colour = 'gray50'),
        axis.ticks.x = ggplot2::element_line(colour = "black"))
    
    g1 <- ggplot2::ggplotGrob(p1)
    g2 <- ggplot2::ggplotGrob(p2)
    
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    
    # superimpose p2 (the panel) on p1
    g <- gtable::gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                                 pp$l, pp$b, pp$l)
    
    # extract the y-axis of p2
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    
    # flip it horizontally
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    
    # add the flipped y-axis to the right
    g <- gtable::gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable::gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    distHistogram <- g
  }
  # distHistogram <- grid.draw(g)
  
  y=data[, uniCol]
  
  distBox <- ggplot2::ggplot(data,
                             ggplot2::aes(x = 1, y = y)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab(" ") +
    ggplot2::ylab("") + ggplot2::theme_bw() + 
    ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank()
    )
  if(optionalPlots)
  {
    x <- na.omit(x)
    densityDf <- density(x)
    normalDf <- data.frame(x = x, y = stats::dnorm(x, mean = mean(x), sd = sqrt(stats::var(x))))
    distHistogram <- plotly::plot_ly(x = x) %>% 
      plotly::add_histogram(name = "Frequency", marker=list(color=priColor )) %>% 
      plotly::layout(bargap = 0.05) %>%
      plotly::add_lines(x = densityDf$x, y = densityDf$y, yaxis = "y2", name = "Density") %>% 
      plotly::add_lines(x = normalDf$x, y = normalDf$y, yaxis = "y2", name = "Normal") %>%
      plotly::layout(yaxis2 = list(overlaying = "y2", side = "right", showgrid = F, zeroline = F, title = "Density", titlefont = list(size = 12), tickfont = list(size = 10)),
                     xaxis = list(title = uniCol, titlefont = list(size = 12), tickfont = list(size = 10)),
                     yaxis = list(title = "Count", titlefont = list(size = 12), tickfont = list(size = 10))) 
    
  
  }
  return(list(distHistogram, distBox))
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


#! Univariate Outlier Statistic Function
outlierStatistic <- function(data, method){
  if(method == "percentile"){
    summaryTable <- sapply(data, function(x){
      stats::quantile(x, c(0.0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1),
                      na.rm = T)
    })
    summaryTable <- round(summaryTable, 3)
    summaryTable <- t(summaryTable)
    rownames(summaryTable) <- NULL
    summaryTable <- cbind(colnames(data), summaryTable)
    colnames(summaryTable) <- c("Column Name", "P_00", "P_01", "P_05", "P_10",
                                "P_25", "P_50", "P_75", "P_90", "P_95", "P_99",
                                "P_100")
    summaryTable <- as.data.frame(summaryTable)
    return(summaryTable)
  }
  if(method == "iqr"){
    quantile13 <- sapply(data, function(x){
      stats::quantile(x, c(0.25, 0.75), na.rm = T)
    })
    quantile13 <- t(quantile13)
    iqr <- sapply(data, function(x){
      stats::IQR(x, na.rm = T)
    })
    summaryTable <- cbind(round(quantile13, 3), round(iqr, 3))
    summaryTable <- data.frame(summaryTable)
    colnames(summaryTable) <- c("first", "third", "iqr")
    summaryTable <- summaryTable %>% dplyr::mutate(first1 =
                                                     round((first - 1.5*iqr),3))
    summaryTable <- summaryTable %>% dplyr::mutate(third1 =
                                                     round((third + 1.5*iqr), 3))
    summaryTable <- summaryTable %>% dplyr::mutate(first3 =
                                                     round((first - 3*iqr), 3))
    summaryTable <- summaryTable %>% dplyr::mutate(third3 =
                                                     round((third + 3*iqr), 3))
    summaryTable <- cbind(colnames(data), summaryTable)
    colnames(summaryTable) <- c("Column Name", "25%", "75%", "IQR",
                                "-1.5*IQR", "+1.5*IQR", "-3*IQR", "+3*IQR")
    return(summaryTable)
  }
  if(method == "z_score"){
    meanData <- sapply(data, function(x){
      mean(x, na.rm =T)
    })
    stdDev <- sapply(data, function(x){
      stats::sd(x, na.rm = T)
    })
    summaryTable <- cbind(round(meanData, 3), round(stdDev, 3))
    summaryTable <- data.frame(summaryTable)
    summaryTable <- cbind(colnames(data), summaryTable)
    colnames(summaryTable) <- c("colname", "Mean", "SD")
    summaryTable <- summaryTable %>% dplyr::mutate(zp1 = round((Mean + SD), 3),
                                                   zp2 = round((Mean + (SD*2)), 3),
                                                   zp3 = round((Mean + (SD*3)), 3),
                                                   zn1 = round((Mean - SD), 3),
                                                   zn2 = round((Mean - (SD*2)), 3),
                                                   zn3 = round((Mean - (SD*3)), 3))
    colnames(summaryTable) <- c("Column Name", "Mean", "SD", "+1 SD", "+2 SD",
                                "+3 SD", "-1 SD", "-2 SD", "-3 SD")
    return(summaryTable)
  }
}


# Outlier Detection Function
outlierDetection <- function(data, method, columnName, cutoffValue){
  if(TRUE %in% unique(is.na(data[,columnName])))
  {
    data <- data[-which(is.na(data[, columnName])== T), ]
  }
  if(method == "iqr"){
    lower <- stats::quantile(data[, columnName], .25,na.rm = T) - 1.5*(stats::IQR(data[, columnName], na.rm = T))
    upper <- stats::quantile(data[,columnName],.75,na.rm = T) + 1.5*(stats::IQR(data[,columnName],na.rm = T))
    data$Outlier <- data[,columnName] > upper | data[,columnName] < lower
    return(data)
  }
  if(method == "percentile"){
    lower <- stats::quantile(data[,columnName],cutoffValue,na.rm = T)
    upper <- stats::quantile(data[,columnName],(1-cutoffValue),na.rm = T)
    data$Outlier <- data[,columnName] > upper | data[,columnName] < lower
    return(data)
  }
  if(method == "z_score"){
    lower <- mean(data[,columnName],na.rm = T) - (cutoffValue*(stats::sd(data[,columnName],na.rm = T)))
    upper <- mean(data[,columnName],na.rm = T) + (cutoffValue*(stats::sd(data[,columnName],na.rm = T)))
    data$Outlier <- data[,columnName] > upper | data[,columnName] < lower
    return(data)
  }
} 


#Outlier Plot Function 
outlierPlot<- function(data,method,columnName,cutoffValue, priColor,optionalPlots){
  if(method == "iqr"){
    outlierPlot <- ggplot2::ggplot(data, ggplot2::aes(x="", y = data[,columnName])) +
      ggplot2::geom_boxplot(fill = priColor,alpha=0.7) + 
      ggplot2::theme_bw() + 
      ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),panel.grid.minor.x=ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank(),legend.position = "bottom") +ggplot2::ylab(columnName) + ggplot2::xlab("")
    
  }
  if(method == "percentile"){
    Outlier<-data$Outlier
    Value<-data[,columnName]
    outlierPlot <- ggplot2::ggplot(data) + 
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
    outlierPlot <- 
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
    outlierPlot <- plotly::ggplotly(outlierPlot)
    outlierPlot$x$layout$margin$l <- outlierPlot$x$layout$margin$l + 30
    outlierPlot$x$layout$margin$b <- outlierPlot$x$layout$margin$b + 3
    
  }
  
  return(outlierPlot)
 
}



#!Multivariate Outlier Detection
multiVarOutlier <- function(data,depCol,indepCol,cutoffValue){
  
  if(TRUE %in% unique(is.na(data[,depCol])))
  {
    data <- data[-which(is.na(data[,depCol])== T),]
  }
  
  indep_form <- paste(indepCol, collapse = "+")
  form <- paste(depCol, indep_form, sep = "~")
  form <- formula(form)
  
  lmObject <- lm(form,data)
  limit <- nrow(data)
  outlierDetect <- car::outlierTest(lmObject,cutoffValue,n.max = limit)
  outlierDetect <- data.frame(outlierDetect[c(1,2,3)])
  outlierDetect <- round(outlierDetect, 4)
  colnames(outlierDetect) <- c("Studentized Residuals","P-Value", "P-Value(Bonferroni Correction)")
  data$Outlier <- ifelse(rownames(data) %in% rownames(outlierDetect),"Outlier","Normal")
  outlierTable <- data[data$Outlier == "Outlier", ]
  outlierTable <- cbind(outlierDetect,outlierTable)
  return(list(data,outlierTable))
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

getCorrMat <- function(dataset, methodused = "everything"){
  cormat <-base::round(cor(dataset, use = methodused),3)
  return(cormat)
}

#! Correlation Matrix Plot Function
correlationMatPlot <- function(cormatrix){
  return(corrplot::corrplot(cormatrix, 
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
                            col = grDevices::colorRampPalette(c("red","white","green"))(200)))
}

#! Bivariate Plots
bivarPlot <- function(dataset, var1, var2, type1, type2, priColor, secColor){
  if(type1 == "numeric" && type2 == "numeric"){
    return(ggplot2::ggplot(dataset, ggplot2::aes(dataset[, var1], dataset[, var2]))+
             ggplot2::geom_point(color=priColor) +
             ggplot2::geom_smooth(method = lm,color=secColor)+
             ggplot2::xlab(var1) +
             ggplot2::ylab(var2) +
             ggplot2::ggtitle(paste('Bivariate plot for',var1,'and',var2,sep=' '))+
             ggplot2::theme(panel.border=element_rect(size=0.1),plot.title = element_text(hjust = 0.5, size = 10),axis.text = element_text(size=10), 
                            axis.title=element_text(size=10)))
  } else if(type1 == "character" && type2 == "character"){
    new_df <- dataset %>% dplyr::group_by_(.dots=c(var1,var2)) %>% dplyr::summarise(n = n())
    colfunc <- grDevices::colorRampPalette(c(priColor, "white" , secColor))              
    colorvar <- length(unique(new_df[[var2]]))
    return(ggplot2::ggplot(new_df, ggplot2::aes(x= new_df[[var1]], y=n, fill = new_df[[var2]])) +
             ggplot2::geom_bar(position = "dodge", stat = "identity") +
             ggplot2::guides(fill=guide_legend(title=var2)) +
             ggplot2::coord_flip() +
             ggplot2::xlab(var1) +
             ggplot2::ylab("count") +
             ggplot2::ggtitle(paste('Bivariate plot for',var1,'and',var2,sep=' ')) +
             ggplot2::theme(panel.border=element_rect(size=0.1),plot.title = element_text(hjust = 0.5, size = 10),axis.text = element_text(size=10), 
                            axis.title=element_text(size=10),legend.position="bottom")+ ggplot2::scale_fill_manual(values = colfunc(colorvar)))
  } else{
    return(ggplot2::ggplot(dataset, ggplot2::aes(x =dataset[[var2]], y = dataset[[var1]])) + 
             ggplot2::geom_dotplot(dotsize = 0.1, color=priColor, binaxis = 'y') +
             ggplot2::coord_flip() +
             ggplot2::xlab(var2) +
             ggplot2::ylab(var1) +
             ggplot2::ggtitle(paste('Bivariate plot for',var1,'and',var2,sep=' '))+
             ggplot2::theme(panel.border=element_rect(size=0.1),plot.title = element_text(hjust = 0.5, size = 10),axis.text = element_text(size=10),
                            axis.title=element_text(size=10)))
  }
}
#!
freq.fun <- function(x, y, z)
{
  # .(y) is using plyr
  library(plyr)
  k <- as.data.frame(table(x, y, z),nrow=length(levels(y)))
  k <- plyr::ddply(k, .(y), transform, percent = Freq/sum(Freq) * 100)
  k <- plyr::ddply(k, .(y), transform, pos = (cumsum(Freq) - 0.5 * Freq))
  k$label <- paste0(sprintf("%.0f", k$percent), "%")
  k$label <- ifelse(k$Freq<25 | k$percent<5, "", k$label) #hard-coded values, change as required
  return(k)
}

corNetPlot <- function(processed_dataset,numeric_cols,corr_filtering,cor_net_filter,remove_missing_values_corrnet,corr_limit)
{
  
  corr_net_num <- cor(processed_dataset[, c(numeric_cols)])
  
  if (corr_filtering == "Numeric") {
    correlationNetworkDataDownload <- reshape2::melt(corr_net_num, na.rm = TRUE)
    if(any(is.na(corr_net_num))){
      cols <- colnames(corr_net_num)[colSums(is.na(corr_net_num)) == nrow(corr_net_num)-1]
      remove_missing_values_corrnet$flag <- TRUE
      corrnetwarning <- paste0("The data has missing values in '",cols,"'  because of which the correlation of these columns is zero")
    }else{
      corrnetwarning <- ""
      remove_missing_values_corrnet$flag <- FALSE
    }
    correlationNetworkData <- qgraph::qgraph(corr_net_num,
                                             vsize = 3,
                                             layout="spring",
                                             labels = T,
                                             threshold = corr_limit)
  } else if (corr_filtering == "Selective") {
    subcorr <- cor(processed_dataset[, c(numeric_cols)][,cor_net_filter])
    
    correlationNetworkDataDownload <- reshape2::melt(subcorr, na.rm = TRUE)
    if(any(is.na(subcorr))){
      cols <- colnames(subcorr)[colSums(is.na(subcorr)) == nrow(subcorr)-1]
      remove_missing_values_corrnet$flag <- TRUE
      corrnetwarning <- paste0("The data has missing values in '",cols,"'  because of which the correlation of these columns is zero")
    }else{
      corrnetwarning <- ""
      remove_missing_values_corrnet$flag <- FALSE
    }
    correlationNetworkData <- qgraph::qgraph(subcorr,
                                             size = 3,
                                             threshold = corr_limit,
                                             layout = "spring",
                                             labels = T)
  }
  
  if (corr_filtering == "Numeric") {
    corr_net_table  <- data.frame(c(1:nrow(corr_net_num)), colnames(corr_net_num))
  } else if (corr_filtering == "Selective") {
    corr_net_table <- data.frame(c(1:nrow(cor(processed_dataset[, c(numeric_cols)][,cor_net_filter]))), colnames(cor(processed_dataset[, c(numeric_cols)][,cor_net_filter])))
  }
  names(corr_net_table) = c("ID", "VariableName")
  return(list(correlationNetworkData=correlationNetworkData,correlationNetworkDataDownload=correlationNetworkDataDownload,corrnetwarning=corrnetwarning,corr_net_table=corr_net_table))
}
#!
preprocessing <- function(processed_dataset,typedata,cat_cols)
{
  dataset_dummified <- data.frame()
  if("Reference.Level" %in% colnames(typedata)){
    for(i in 1:length(cat_cols)){
      dummified <- DummyVarCreation(processed_dataset[,cat_cols[i],drop = F],
                                    referenceLevel = as.character(typedata[which(typedata$Column == cat_cols[i]),"Reference.Level"]))
      
      if(i==1){
        dataset_dummified <- data.frame(dummified[[1]])
        reference_level <- data.frame(rep(dummified[[2]], ncol(dummified[[1]])))
      }
      else{
        dataset_dummified <- cbind(dataset_dummified, dummified[[1]])
        reference_level <- rbind(reference_level,data.frame(rep(dummified[[2]], ncol(dummified[[1]]))))
      }
      
    }
  }
  else {
    for(i in 1:length(cat_cols)){
      dummified  <- DummyVarCreation(processed_dataset[,cat_cols[i],drop = F])
      if(i==1){
        dataset_dummified <- data.frame(dummified[[1]])
        reference_level <- data.frame(rep(dummified[[2]], ncol(dummified[[1]])))
      }
      else{
        dataset_dummified <- cbind(dataset_dummified, dummified[[1]])
        reference_level <- rbind(reference_level,data.frame(rep(dummified[[2]], ncol(dummified[[1]]))))
      }
    }}
  return(list(dataset_dummified=dataset_dummified,reference_level=reference_level))
}
#!

factorPlot <- function(dummyDataset,singular_cols,cat_cols, sub_Fact,date_cols,priColor,secColor,optionalPlots)
{
  
  # Get dataset, filter and scale
  include_cols <- setdiff(colnames(dummyDataset), c(singular_cols,cat_cols, sub_Fact, date_cols))
  fac_data <- dummyDataset[,include_cols]
  fac_data <- na.omit(reshape::rescaler.data.frame(fac_data, type = 'sd'))
  ev <- eigen(stats::cor(fac_data, use = "pairwise.complete.obs"))
  
  ev.values <- ev$values
  ev.values <- as.data.frame(ev.values)
  # parallel uses nFactors
  library(nFactors)
  aparallel <- as.data.frame(parallel(subject = nrow(fac_data), var = ncol(fac_data), rep = 100, cent = .05)$eigen$qevpea)
  # GGPLOT SCREE TEST
  x=1:nrow(cbind(ev.values, aparallel))
  scree_plot <- ggplot2::ggplot(ggplot2::aes(x = x, 
                                                              y = ev.values, color= "Eigen-Values"), 
                                                 data = cbind(ev.values, aparallel)) +
                                   ggplot2::theme_bw() + 
                                   ggplot2::theme(panel.grid.minor.x=ggplot2::element_blank(),legend.position = "bottom",axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0))) +
                                   ggplot2::geom_point(color=priColor) +
                                   ggplot2::geom_line(size = 1,color=priColor) +
                                   ggplot2::labs(title = "Scree Plot") +
                                   ggplot2::xlab("Component")+
                                   ggplot2::ylab("Eigen Values") +
                                   ggplot2::geom_hline(yintercept = 1, linetype = "dashed",color=secColor) +
                                   ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),legend.position = "none") +
                                   ggplot2::scale_y_continuous(breaks = round(seq(0, 10, by = 1), 1)) + 
                                   ggplot2::scale_x_continuous(breaks = 1:nrow(cbind(ev.values, aparallel)))
  ggplot2::ggsave(scree_plot,file="Source/scree_plot.png")
  #conditionToBe
  if(optionalPlots){
    scree_plot<-plotly::ggplotly(scree_plot,tooltip=c("x","y")) 
    scree_plot$x$layout$margin$l <- scree_plot$x$layout$margin$l + 30
    scree_plot$x$layout$margin$b <- scree_plot$x$layout$margin$b + 30
  }
  return(scree_plot)
  
}

runFactorAnalysis <- function(dummyDataset,singular_cols,cat_cols, sub_Fact, date_cols,k_value,factor_names,lower_val_factanal,facLimit)
{
  # Get dataset, filter and scale
  include_cols <- setdiff(colnames(dummyDataset), c(singular_cols,cat_cols, sub_Fact, date_cols))
  fac_data <- dummyDataset[, include_cols, drop = FALSE]
  
  fac_data <- na.omit(reshape::rescaler.data.frame(fac_data, type = 'sd'))
  k <-  as.numeric(k_value)
  p <-  ncol(fac_data)
  
  if(!(((p-k)**2 > (p+k)) && k <= p)) {
    stop("Invalid k input. Select a different k and submit again.")
    
  } else {
    print("building model")
    # Build factor analysis model
    factor_model <- tryCatch(
      stats::factanal(fac_data, 
                      as.numeric(k), 
                      rotation = "varimax",
                      lower = lower_val_factanal),
      
      error = function(e) {
        print("cannot optimise")
        print(e)
        stop(as.character(e))
      }
    )
    if(class(factor_model)[1] == "factanal"){
      
      # factors in graph
      
      fact_plot <- qgraph::qgraph.efa(factor_model,
                                      threshold = facLimit,
                                      labels = T,
                                      legend = T)
      
      
      
      # calculating factor scores
      loads <- round(unclass(factor_model$loadings),3)
      factor_scores <- as.matrix(fac_data) %*% loads
      
      
      # giving user defined names to each factor
      
      colnames(loads) <- factor_names
      
      columnKey <- data.frame("ID" = c(1:ncol(fac_data)), "ColumnName" = colnames(fac_data))
      
      loads <- cbind(columnKey,loads)
      
      
      
      
      # Variance explained
      SS.loadings <- colSums(round(factor_model$loadings,3)^2)
      Prop_Var <- round(SS.loadings/nrow(factor_model$loadings),3)
      Cum_Var <- round(cumsum(SS.loadings/nrow(factor_model$loadings)),3)
      
      fact_var_exp <- data.frame(rbind(round(SS.loadings,3), Prop_Var, Cum_Var))
      colnames(fact_var_exp) <- factor_names
      fact_var_exp <- data.frame(cbind(data.frame("Analysis Statistic" = c("SS Loadings", "Proportional Variance", "Cumulative Variance")), fact_var_exp))
      rownames(fact_var_exp) <- NULL
      
      # uniqueness
      uniq <- data.frame("Uniqueness" = round(factor_model$uniquenesses, 3))
      uniq <- cbind(columnKey, uniq)
      uniq <- uniq %>% dplyr::mutate(Communality = 1 - Uniqueness)
      
      
      
      
    }
  }
  return(list(factor_scores=factor_scores,fact_plot=fact_plot,loads=loads,factor_model=factor_model,fact_var_exp=fact_var_exp,fac_data=fac_data,uniq=uniq))
}

singularityRemoval <- function(dep_var,dummyDataset,numeric_cols,dummy_cols)
{
  # Run OLS with all columns, remove columns that have NA coefficients. They are your singular columns
  formula <- paste0(dep_var, "~.")
  lm_obj <- lm(formula,dummyDataset[, c(numeric_cols, dummy_cols)])
  
  coeff <- lm_obj$coefficients
  coeff <- as.data.frame(coeff)
  colnames(coeff) <- c("Coefficients")
  rem_sing <- rownames(coeff)[which(is.na(coeff$Coefficients))]
  
  rem_cols <- as.data.frame(rem_sing)
  colnames(rem_cols) <- c("Columns removed causing singularity")
  return(list(rem_cols=rem_cols,rem_sing=rem_sing))
}
#!

kMeansKneePlot <- function(type, data, metric, kRange, iter, primaryColor, dissMeasure = NULL)
{
  lower <- kRange[1]
  upper <- kRange[2]
  diss <- cluster::daisy(data, metric = metric)
  df <- matrix(unlist(lapply(lower:upper, function(x){
    model<- cluster::pam(x = diss, k = as.numeric(x), metric = metric, diss = TRUE, keep.data = F)
    cluster <- model$clustering
    if(var(model$cluster)==0)
      return(NULL)
    sil <- mean(cluster::silhouette(cluster, diss)[,3])
    return(c(x, round(sil,3)))
  })), ncol = 2, byrow = T, dimnames = list(NULL, c("K", "Silhouette")))
  df <- as.data.frame(df)
  x <- df[which(df[,"Silhouette"] == max(df[,"Silhouette"]))[[1]], "K"]
  g <- generateSilPlot(df, x, primaryColor)
  return(list(plot = g, k = x))
}


generateSilPlot <- function(df, x, primaryColor)
{
  g <- ggplot2::ggplot(df) + 
    ggplot2::geom_line(ggplot2::aes_string(x = "K", y = "Silhouette"), color=primaryColor) + 
    ggplot2::theme(legend.position="none",plot.title = ggplot2::element_text(hjust = 0.5,size=12),axis.title=ggplot2::element_text(size=14)) +
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),panel.grid.minor.x=ggplot2::element_blank(),legend.position = "bottom") +
    ggplot2::ggtitle(paste0("Plot of Average Silhouette Width for different K")) + 
    ggplot2::xlab("Number of clusters") + 
    ggplot2::ylab("Average Silhouette Width") + 
    ggplot2::scale_x_continuous(breaks=df[,"K"]) + 
    ggplot2::geom_vline(ggplot2::aes(xintercept=x), linetype="dotted")
  return(g)
}

dataDendogram <- function(clusters,cat_cols,numeric_cols,singular_cols,sub_clust,dummyDataset,priColor,optionalPlots)
{
  include_cols <- setdiff(c(cat_cols, numeric_cols), c(singular_cols, sub_clust))
  data <- dummyDataset[,include_cols, drop = FALSE]
  data <- na.omit(data)
  if(nrow(data) > 0) {
    
    # build cluster
    pam_fit <- cluster::pam(data, diss = FALSE, k = clusters, stand = TRUE)
    kmeans_cluster <- tryCatch(
      data %>% dplyr::mutate(cluster = pam_fit$clustering) %>% dplyr::group_by(cluster) %>% dplyr::do(the_summary = summary(.)),
      error = function(e) return(e)
    )
    rm(data)
    gc() 
    # downloadData$clusterData <- data.frame(ClusterNumber=kmeans_cluster$cluster)
    if(class(kmeans_cluster)[1] != 'rowwise_df') {
      stop(paste0("There was an error while building the cluster.", kmeans_cluster$message))
      
    } else {
      
      # Get cluster centers
      center <- pam_fit$medoids
      rownames(center) <- 1:nrow(center)
      # get hierarchical cluster of the centers
      hcluster <- stats::hclust(cluster::daisy(center, metric = "gower"))
      # Plot dendogram with colour coded cluster
      dend <- stats::as.dendrogram(hcluster)
      dend_c <- dendextend::color_branches(dend, k = clusters)
      
      ggdend <- dendextend::as.ggdend(dend_c)
      
      
      dendoPlot <- clusterDendoplot(ggdend,optionalPlots)
      
      
      
      
      # agglomeration schedule
      aggSch <- data.frame(
        index = 1:length(hcluster$height),
        height = sort(hcluster$height,T),
        merge.left = hcluster$merge[,1],
        merge.right = hcluster$merge[,2]
      )
      
      # Agglomeration plot
      
      aggPlot <- clusterAggPlot(aggSch,priColor,center,optionalPlots)
      
    }
  }
  return(list(pam_fit=pam_fit, agg_plot=aggPlot, dendoPlot=dendoPlot))
}

clusterDendoplot <- function(ggdend,optionalPlots)
{
  dendoPlot <- ggplot2::ggplot(ggdend, horiz = TRUE, theme = NULL) + 
                                  ggplot2::ylab("Height") + 
                                  ggplot2::xlab("Cluster") +
                                  ggplot2::ggtitle("Cluster Centroids Dendogram") +
                                  ggplot2::theme_bw() + 
                                  
                                  ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),legend.position="none",
                                                 plot.title = ggplot2::element_text(hjust=0.5,vjust = 0.5),
                                                 axis.text = ggplot2::element_text(size = 14),
                                                 axis.text.x = ggplot2::element_text(vjust = 1, size = 18, hjust = 1)
                                  )
  if(optionalPlots){
    dendoPlot<-plotly::ggplotly(dendoPlot)
    dendoPlot$x$layout$margin$l <- dendoPlot$x$layout$margin$l + 30
    dendoPlot$x$layout$margin$b <- dendoPlot$x$layout$margin$b + 10
  }
  return(dendoPlot)
  
}
clusterAggPlot <- function(aggSch,priColor,center,optionalPlots)
{
  aggPlot <-  ggplot2::ggplot(aggSch, ggplot2::aes(x = index, y = height)) + 
                                 ggplot2::geom_line(color=priColor) +
                                 ggplot2::geom_point(color=priColor) +
                                 ggplot2::xlab("Number of clusters") +
                                 ggplot2::ylab("Error coefficient") +
                                 ggplot2::scale_x_continuous(breaks = c(1:length(center))) +
                                 ggplot2::ggtitle("Agglomeration Schedule") + 
                                 ggplot2::theme_bw() + 
                                 ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),panel.grid.minor.x=ggplot2::element_blank(),legend.position = "bottom") +
                                 ggplot2::theme(
                                   plot.title = ggplot2::element_text(hjust=0.5,vjust = 0.5),
                                   axis.text = ggplot2::element_text(size = 14),
                                   axis.text.x = ggplot2::element_text(vjust = 1, size = 18, hjust = 1)
                                 ) 
  if(optionalPlots){
    aggPlot<-plotly::ggplotly(aggPlot)
    aggPlot$x$layout$margin$l <- aggPlot$x$layout$margin$l + 30
    aggPlot$x$layout$margin$b <- aggPlot$x$layout$margin$b + 10
  }
  return(aggPlot)
}




clusterDistData <- function(kmeans_cluster,priColor,cluster_names,optionalPlots)
{
  
  tcenter <- as.data.frame(kmeans_cluster$medoids)
  rownames(tcenter) <- 1:nrow(tcenter)
  is.num <- sapply(tcenter, is.numeric)
  tcenter[is.num] <- lapply(tcenter[is.num], scale, TRUE, TRUE)
  tcenter[is.num] <- lapply(tcenter[is.num], round, 3)
  tcenter <- data.frame(t(tcenter))
  cluster_size <- data.frame("ClusterName" = colnames(tcenter), "Frequency" = kmeans_cluster$clusinfo[,1])
  
  
  # naming clusters
  
  colnames(tcenter) <- cluster_names
  
  # Displaying cluster centers table
  tcenter <- cbind(data.frame('ColumnName' = rownames(tcenter)), tcenter)
  
  ClusterName=cluster_size$ClusterName
  Frequency=cluster_size$Frequency
  clusterDistPlot <- ggplot2::ggplot(cluster_size, ggplot2::aes(x=ClusterName, y=Frequency)) +
                                        ggplot2::geom_col(alpha=0.7) +
                                        ggplot2::geom_bar(color=priColor,fill=priColor,stat="identity",alpha=0.7) +
                                        ggplot2::ylab("Frequency") +
                                        ggplot2::xlab("Cluster Name") +
                                        ggplot2::ggtitle("Cluster Distribution") +
                                        ggplot2::theme_bw() + 
                                        ggplot2::theme(panel.grid.minor.x=ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank(),legend.position = "bottom") +
                                        ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),
                                                       plot.title = ggplot2::element_text(hjust=0.5,vjust = 0.5),
                                                       axis.text = ggplot2::element_text(size = 14),
                                                       axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, size = 18, hjust = 1)
                                        )
  if(optionalPlots){
    clusterDistPlot<-plotly::plot_ly(y=Frequency,x=ClusterName,type="bar")
    
  }
  
  
  
  return(list(tcenter=tcenter,clusterDistPlot=clusterDistPlot,cluster_size=cluster_size))
  
}

clusterMultiplePlots <- function(tcenter_num,priColor,secColor,i,title_plot,optionalPlots)
{
  a<-rownames(tcenter_num)
  categ <- as.factor(ifelse(tcenter_num[,i]>1.96,"1",
                            ifelse(tcenter_num[,i]<(-1.96),"2","3")))
  x=rownames(tcenter_num)
  Zscore=tcenter_num[,i]
  
  cluster_plot <- ggplot2::ggplot(ggplot2::aes(x = x, y = Zscore), data = tcenter_num) +
                                     ggplot2::geom_col(ggplot2::aes(fill = categ),alpha=0.7) +
                                     
                                     ggplot2::coord_flip() +
                                     ggplot2::xlab("variables") + 
                                     ggplot2::ylab("Z-score") + ggplot2::theme_bw() + 
                                     ggplot2::theme(panel.border=ggplot2::element_rect(size=0.1),
                                                    plot.title = ggplot2::element_text(hjust=1),panel.grid.major.x=ggplot2::element_blank(),
                                                    legend.position = "none",
                                                    axis.text.x=ggplot2::element_text(angle=45, hjust=1)
                                     ) +
                                     
                                     ggplot2::geom_hline(yintercept = c(1.96,-1.96), color =secColor) +
                                     ggplot2::ggtitle(title_plot) + 
                                     ggplot2::scale_fill_manual(values = c("1"="green","2"= "red","3"=priColor))

  if(optionalPlots){
    a<-unique(as.character(row.names(tcenter_num)))
    cluster_plot<-plotly::plot_ly(x=tcenter_num[[i]],y=row.names(tcenter_num),type="bar",orientation='h') %>% plotly::add_segments(x=1.96,xend=1.96,y=sort(row.names(tcenter_num))[0],yend=tail(sort(row.names(tcenter_num)),n=1)) %>% plotly::add_segments(x=-1.96,xend=-1.96,y=sort(row.names(tcenter_num))[0],yend=tail(sort(row.names(tcenter_num)),n=1))
    cluster_plot$x$layout$margin$l <- cluster_plot$x$layout$margin$l + nchar(a[which.max(lapply(a, nchar))])*5
    cluster_plot$x$layout$margin$b <- cluster_plot$x$layout$margin$b + 10
  }
  
  
  return(cluster_plot)
}

clusterSammonsData <- function(dummyDataset,singular_cols,sub_clust,cat_cols,clust_anal_go,kmeans_cluster,cluster_names)
{
  # get dataset and scale
  include_cols <- setdiff(colnames(dummyDataset), c(singular_cols,sub_clust))
  dataset <- dummyDataset[,include_cols,drop = FALSE]
  dataset <- na.omit(dataset)
  include_cols <- setdiff(colnames(dataset), c(cat_cols))
  dataset <- dataset[,include_cols, drop = FALSE]
  scaleddata <- reshape::rescaler.data.frame(dataset, type = 'sd')
  
  
  kdataset <- data.frame(kmeans_cluster$medoids)
  kclus <- kmeans_cluster$clustering
  ksize <- kmeans_cluster$clusinfo[,1]
  kdataset <- reshape::rescaler.data.frame(kdataset, type = 'sd')
  distmat <- cluster::daisy(kdataset, "euclidean")
  samn <- MASS::sammon(distmat, trace=F)
  final <- data.frame(samn$points)
  polygondata <- final
  polygondata$id <- cluster_names
  
  vor_pts <- sp::SpatialPointsDataFrame(cbind(polygondata$X1,polygondata$X2),polygondata,match.ID=TRUE)
  vor_desc <- deldir::tile.list(deldir::deldir(vor_pts@coords[,1], vor_pts@coords[,2]))
  vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
    
    # tile.list gets us the points for the polygons but we
    # still have to close them, hence the need for the rbind
    tmp <- cbind((vor_desc[[i]]$x), vor_desc[[i]]$y)
    tmp <- rbind(tmp, (tmp[1,]))
    
    # now we can make the Polygon(s)
    sp::Polygons(list(sp::Polygon(tmp)), ID=i)
  })
  
  vor_dat <- vor_pts@data
  rownames(vor_dat) <- sapply(slot(sp::SpatialPolygons(vor_polygons), 'polygons'), slot, 'ID')
  
  vor <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(vor_polygons), data=vor_dat)
  sammonsdata <- na.omit(dataset)
  return(list(sammonsdata=sammonsdata,kclus=kclus,scaleddata=scaleddata,vor=vor,scaleddata=scaleddata,polygondata=polygondata,ksize=ksize))
}

clusterSammonsPlot <- function(scaleddata,kclus,sammonsdata,vor,ksize,polygondata,priColor,clus_var_name,j,cluster_names)
{
  scaleddata$newcol <- kclus
  newclusvarname <- clus_var_name[j]
  scaleddata$newone <- sammonsdata[,newclusvarname]
  scaleddata_grouped <- aggregate(scaleddata[, "newone"], list(scaleddata$newcol), mean)
  colnames(scaleddata_grouped) <- c("newcol","avg_lf")
  b <- scaleddata_grouped[,c("newcol","avg_lf")]
  b <- data.frame(unique(b))
  b <- b[order(+b["newcol"]),]
  d <- b$avg_lf
  
  vor_df <- ggplot2::fortify(vor)
  
  
  for (k in 1:nrow(vor_df)){
    vor_df$varname[k] <- d[as.integer(vor_df$id[k])]
  }
  
  colorfill <- as.integer(as.factor(cluster_names))
  polyplots_title <- paste0("voronoi representation of cluster centers :",
                            clus_var_name[j])
  
  
  
  
  sammons_plot <- ggplot2::ggplot() +
    ggplot2::geom_map(data=vor_df,
                      map=vor_df,
                      ggplot2::aes(x=long, y=lat, map_id=vor_df$id,fill = varname),size=0.25) +
    ggplot2::geom_path(data=vor_df,ggplot2::aes(x=long, y=lat, map=vor_df$id),lwd=0.1)+
    ggplot2::geom_point(data = polygondata, ggplot2::aes(X1,X2, colour=cluster_names, size=ksize),
                        fill=factor(colorfill),
                        shape=21)+
    ggplot2::scale_size_continuous(range = c(3, 7),guide = FALSE)+
    ggplot2::guides(fill=ggplot2::guide_legend(title="Color scale"))+                         
    ggplot2::labs(x="X1",y="X2")+                                                    
    ggplot2::labs(colour="clusters")+                                              
    ggplot2::ggtitle(polyplots_title)+                                               
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+                        
    # scale_fill_gradient(low = "white", high = "steelblue")+
    ggplot2::scale_fill_gradient(low = "white", high = priColor)+
    ggplot2::theme(legend.position = "bottom")
  report_sammons_plot <<- sammons_plot
  return(list(sammons_plot=sammons_plot,report_sammons_plot=report_sammons_plot))
  
}


downloadClusterTable <- function(dummyDataset,singular_cols,sub_clust,kmeans_cluster,working_dir)
{
  include_cols <- setdiff(colnames(dummyDataset), c(singular_cols, sub_clust))
  dataset <- na.omit(dummyDataset[,include_cols,drop = FALSE])
  
  scaleddata <- reshape::rescaler.data.frame(dataset, type = 'sd')
  scaleddata$cluster <- kmeans_cluster$cluster
  
  return(scaleddata)
  
}


#!# Download Report

downloadReport <- function(reactData, input, downloadData, fileName){
  require(rmarkdown)
  rmarkdown::render('Source/report.Rmd',
                    params = list(reactData = reactData,
                                  downloadData = downloadData,
                                  input = input),
                    switch(input$format,
                           PDF = pdf_document(),
                           PrettyHTML = prettydoc::html_pretty(css = "../Styles/pretty_styles.css"),
                           HTML = html_document(css = "../Styles/styles.css" ,toc= T,toc_float= T),
                           
                           MSWord = word_document(toc = T)),
                    output_dir = "Downloads/" , 
                    output_file = paste(fileName,switch( input$format, 
                                                         PDF = '.pdf', 
                                                         HTML = '.html',
                                                         PrettyHTML = '_pretty.html', 
                                                         MSWord = ".doc"),sep='')
  )
}

columnalign <- function(x) {
  columnclass <- lapply(x, class)
  return(which(columnclass != "factor")-1)
}




bivarCatCat <- function(dataset,select_var_name_1,select_var_name_2,priColor,secColor,optionalPlots){
  if(optionalPlots){
   
    new_df <- dataset %>% dplyr::group_by_(.dots=c(select_var_name_1,select_var_name_2)) %>% dplyr::summarise(n = n())
    a=as.vector(as.character(unique(new_df[[select_var_name_1]])))
    unique_loc <- unique(new_df[[select_var_name_2]])
    colfunc <- grDevices::colorRampPalette(c(priColor, "white" , secColor))
    colorvar <- length(unique(new_df[[select_var_name_2]]))
    eval(parse(text=paste0("first<- new_df %>% dplyr::filter(",select_var_name_2,"==unique_loc[1])")))
    p<-plotly::plot_ly(y=first[[select_var_name_1]] ,x=first[['n']], name=unique_loc[1],type="bar",orientation='h',colors  = I(colfunc(colorvar)[1]))
    for(i in 2:length(unique_loc)){
      eval(parse(text=paste0("first<- new_df %>% filter(",select_var_name_2,"==unique_loc[i])")))
      p<-plotly::add_trace(p,name=unique_loc[i],y=first[[select_var_name_1]] ,x=first[['n']], type="bar",orientation='h',marker=list(color=colfunc(colorvar)[i]))
      p$x$layout$margin$l <- p$x$layout$margin$l + nchar(a[which.max(lapply(a, nchar))])
      p$x$layout$margin$b <- p$x$layout$margin$b + 3
  }
  }
  else{
    new_df <- dataset %>% dplyr::group_by_(.dots=c(select_var_name_1,select_var_name_2)) %>% dplyr::summarise(n = n())
    colfunc <- grDevices::colorRampPalette(c(priColor, "white" , secColor))
    colorvar <- length(unique(new_df[[select_var_name_2]]))
    a=as.vector(as.character(unique(new_df[[select_var_name_1]])))
    y=new_df[[select_var_name_1]]
    label=new_df[[select_var_name_2]]
    p <- ggplot2::ggplot(new_df, ggplot2::aes(x = y, y= n, fill = label)) +
      ggplot2::geom_bar(position = "dodge", stat = "identity",alpha=0.9) +
      ggplot2::guides(fill=ggplot2::guide_legend(title=select_var_name_2)) +
      ggplot2::coord_flip()+
      ggplot2::xlab(select_var_name_1) +
      ggplot2::ylab("count") + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste('Bivariate plot for',select_var_name_1,'and',select_var_name_2,sep=' '))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),axis.text = ggplot2::element_text(size=10),
                     axis.title=ggplot2::element_text(size=10),legend.position="bottom",axis.text.x=ggplot2::element_text(angle=45, hjust=1))+ ggplot2::scale_fill_manual(values = colfunc(colorvar))
    

    
            
    
    }
  
  return(p)
}
