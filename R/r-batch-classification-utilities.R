#################################################
# Title: Classification utils
# Created on: October 15, 2018
# Authors : Dheekshitha PS

#################################################

list_to_df <- function(listfordf){
  if(!is.list(listfordf)) stop("it should be a list")
  
  df <- list(list.element = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))
  
  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }
  
  df
}



# Removing linear dependent columns from the data
#' @name removeLinearDependentCols
#' @title Removes linear dependent columns from the data
#' @details Removes the columns which are linearly dependent on each other from the given data
#' @param dataset the data from which linearly dependent columns should be removed
#' @param depVar the dependent variable in the data for  building a linear model
#' @param removeMissingValues boolean to say if the missing values are to be removed or not
#' @param positiveClass the positive class from the dependent variable
#' @return returns a dataset with linearly dependent columns removed
#' @family Package Classification Utilites functions
#' @export

removeLinearDependentCols <- function(dataset,
           depVar,
           removeMissingValues = T,
           positiveClass) {
    
    miss <- handleMissingValues(dataset, remove = removeMissingValues)
    #miss$mssg
    dataset <- miss$data
    
    meta <- metaFileLoad(varTypeFile = NULL, dataset)
    typeData <- meta$typeData
    
    meta <- useMetaFile(dataset, typeData)
    dataset <- meta$data
    typeData <- meta$typeData
    datasetCategorical <- meta$catCols
    datasetNumeric <- meta$numCols
    binaryCatNames <- meta$binCatCols
    
    binaryCatData <- dataset[, datasetCategorical, drop = F]
    
    
    catCols <- setdiff(datasetCategorical, binaryCatNames)
    binCols <- setdiff(binaryCatNames, depVar)
    
    dummifiedCatData <- as.data.frame(lapply(catCols,
                                             function(x)
                                             {
                                               DummyVarCreation(dataset[, x, drop = F],
                                                                referenceLevel =
                                                                  typeData$Reference.Level[which(typeData$Column == x)])
                                             }))
    dummifiedBinCatData <- as.data.frame(lapply(binCols,
                                                function(x)
                                                {
                                                  DummyVarCreation(binaryCatData[, x, drop = F],
                                                                   referenceLevel =
                                                                     typeData$Reference.Level[which(typeData$Column == x)])
                                                }))
    
    
    if (length(catCols) == 0)
    {
      print("catcols-0")
      dummifiedCatData <- rep(NA, nrow(dataset))
    }
    # To handle the absence of categorical variables with factors = 2
    if (length(binCols) == 0) {
      print("bincols-0")
      dummifiedBinCatData <- rep(NA, nrow(dataset))
    }
    origColumns <- colnames(dataset)
    dataset <- dataset[, setdiff(colnames(dataset),
                                 c(catCols, binCols, depVar)), drop = F]
    dataset <-
      cbind.data.frame(dummifiedCatData,
                       dummifiedBinCatData,
                       dataset,
                       binaryCatData[, depVar, drop = F])
    
    # To remove columns of NAs that could have been introduced by the absence of categorical variables
    dataset <- dataset[, which(colSums(is.na(dataset)) < nrow(dataset))]
    
    colnames(dataset)[ncol(dataset)] <- depVar
    
    formula <- stats::as.formula(paste0(depVar, "~."))
    
    lm_model <- stats::lm(formula, dataset)
    col_names <- setdiff(colnames(dataset), depVar)
    
    coeff <- as.data.frame(lm_model$coefficients)
    colnames(coeff) <- c("Coefficient")
    
    rem_col <- col_names[which(is.na(coeff$Coefficient))]
    datasetRemCols <-
      dataset[,!(colnames(dataset) %in% rem_col), drop = F]
    modLevels <- c(setdiff(levels(as.factor(binaryCatData[, depVar])),
                           positiveClass), positiveClass)
    
    outputtbl <- list(dataset = datasetRemCols, factorLabels = modLevels)
    return(list_to_df(outputtbl))
    #return(dataset = datasetRemCols)
    #cols_with_singularity <- as.data.frame(reactData$rem_col)
  }

# Select features using logistic regression
#' @name logisticRegressionFeatureSelection
#' @title Get importance of features using logistic regression
#' @details Get the rank of importance of each of the feature in the data using logistic regresssion
#' @param dataset the data from which linearly dependent columns are removed
#' @param factorLabels the levels of factors present in dependent variable
#' @param fsWindow number of features to be displayed in graph
#' @param depVar predicted Variable
#' @param color the color of the graph
#' @return gives a graph showing the rank of importance of the features
#' @family Package classification Utilites functions
#' @export

logisticRegressionFeatureSelection <-
  function(dataset,factorLabels,
           depVar ,
           fsWindow,
           color="blue")
  {

    
    reactDataComponents <- list()
    logisticModel <-
      stats::glm(as.formula(paste0(depVar, "~.")),
                 data = dataset,
                 family = binomial(link = "logit"))
    
    LogRegVarImp <- caret::varImp(logisticModel)
    LogRegVarImp <-
      data.frame(Features = rownames(LogRegVarImp),
                 Importance = LogRegVarImp$Overall)
    LogRegVarImp <-
      LogRegVarImp[with(LogRegVarImp, order(-Importance)),]
    rownames(LogRegVarImp) <- 1:nrow(LogRegVarImp)
    reactDataComponents$LogRegPlot <-
      ggplot2::ggplot(LogRegVarImp[1:fsWindow, , drop = F],
                      ggplot2::aes(x = stats::reorder(Features, Importance), y = Importance)) +
      ggplot2::geom_bar(
        color = "#FFFFFF",
        fill = color,
        stat = "identity",
        position = "dodge"
      ) +
      ggplot2::coord_flip() + ggplot2::ylab("Importance") + ggplot2::xlab("Features") +
      ggplot2::labs(title = "Logistic Regression") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
        axis.title = ggplot2::element_text(size = 14)
      )
    reactDataComponents$LRImpVar <- LogRegVarImp
    # reactDataComponents$LRImpVar <- as.character(LogRegVarImp$Features)
    reactDataComponents$LogRegFSError <- ""
    outputtbl <-  list(
      dataset = dataset,
      reactDataComponents,
      factorLabels = factorLabels
    )
    return(list_to_df(outputtbl))
    
  }


# Split the data into train, test and validation
#' @name splitTrainValidation
#' @title Split the data into train, test and validation
#' @details Split the given data into train, test and validation 
#' @param dataset the data from which linearly dependent columns are removed
#' @param factorLabels the levels of factors present in dependent variable
#' @param TrainTestSplitPercent the percentage of data to be used for training
#' @param depVar predicted Variable
#' @return Gives the training and testing data
#' @family Package Classification Utilites functions
#' @export

splitTrainValidation <- function(dataset,factorLabels,TrainTestSplitPercent ,
           depVar) {
    
    #TrainTestSplitPercent=70
    trainingDataRowCount <-
      floor(TrainTestSplitPercent * 0.01 * nrow(dataset))
    trainInd <-
      sample(seq_len(nrow(dataset)), size = trainingDataRowCount)
    testInd <- setdiff(c(1:nrow(dataset)), trainInd)
    dataSplitInfo <- data.frame(c(as.integer(
      TrainTestSplitPercent * 0.01 * nrow(dataset)
    )),
    c(as.integer((100 - TrainTestSplitPercent) * 0.01 * nrow(dataset)
    )))
    
    colnames(dataSplitInfo) <- c("Train", "Validation")
    rownames(dataSplitInfo) <- c("Number of rows")
    
    
    trainingData <- trainInd
    testingData <- testInd
    testRowLim <- length(testingData)
    testingData <- c(1:testRowLim)
    trainingData = c((testRowLim + 1):nrow(dataset))
    
    trainClassFreq <- data.frame(table(dataset[trainInd, depVar]))
    colnames(trainClassFreq) <- c("Class Labels", "Number of samples")
    testClassFreq <- data.frame(table(dataset[testInd, depVar]))
    colnames(testClassFreq) <- c("Class Labels", "Number of samples")
    outputtbl <-list(
      dataSplitInfo = dataSplitInfo,
      trainClassFreq = trainClassFreq,
      testClassFreq = testClassFreq,
      trainingData = dataset[trainingData, , drop = F],
      testingData = dataset[testingData, , drop = F],
      factorLabels = factorLabels
    )
    return(list_to_df(outputtbl))
  }
# Fit a Logistic Regression Model
#' @name lrfit
#' @title Fit a Logistic Regression Model
#' @details Fit a Logistic Regression Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export
lrFit <- function(train,test,factorLabels,xSelected,depVar ,cvK = 3,
                  modelColours ="yellow",primaryColor="red",
                  secondaryColor="blue" )
  {
    caretOut <- lrTrain(train, xSelected, depVar, cvK, factorLabels)
    model <- caretOut$finalModel
    
    # generate cross validation plots
    if (cvK > 1) {
      cvPlot <-
        crossValidationPlot(
          caretOut$resample,
          'Accuracy',
          'Logistic Regression',
          primaryColor,
          secondaryColor
        )
      cvCM <-
        caretOut$resampledCM[, c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
      colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
    } else {
      cvPlot <- cvCM <- NULL
    }
    
    predTrain <- lrPredictProb(model, train, factorLabels)
    lrOutput <-
      allAnalysisOnPreds(
        factor(train[, depVar], levels = factorLabels),
        predTrain,
        modelColours,
        factorLabels,
        primaryColor,
        secondaryColor
      )
    ### Pretty plots
    reactDataComponents = list()
    reactDataComponents$lrcvPlot <- cvPlot
    reactDataComponents$lrcvCM <- cvCM
    plotTheme <- ggplot2::theme(
      legend.position = c(0.8, 0.8),
      legend.background = ggplot2::element_rect(fill =
                                                  ggplot2::alpha('white', 0)),
      legend.direction = "vertical",
      plot.title = ggplot2::element_text(hjust = 0.5, size =
                                           20),
      axis.title = ggplot2::element_text(size =
                                           14)
    )
    reactDataComponents$lrFancyProbDenPlot <-
      densityFunc(factor(train[, depVar], levels = factorLabels),
                  predTrain,
                  primaryColor,
                  secondaryColor)
    accPresRecallPlots <-
      customPlots(factor(train[, depVar], levels = factorLabels),
                  predTrain,
                  factorLabels,
                  primaryColor)
    reactDataComponents$lrPrecPlot <-
      accPresRecallPlots[[2]] + plotTheme
    reactDataComponents$lrAccPlot <-
      accPresRecallPlots[[1]] + plotTheme
    reactDataComponents$lrRecallPlot <-
      accPresRecallPlots[[3]] + plotTheme
    
    reactDataComponents$lrROCPlot <- lrOutput[[1]]
    reactDataComponents$lrSSTPlot <- lrOutput[[3]]
    
    Metric = c("Area Under Curve", "Optimal threshold")
    Values = c(lrOutput[[2]], lrOutput[[4]][[1]])
    reactDataComponents$lrOptTh <- round(lrOutput[[4]], 2)
    reactDataComponents$lrAOCTable <- data.frame(Metric, Values)
    summ <- summary(model)
    if (min(summ$coefficients[, 4]) < 0.000001)
    {
      reactDataComponents$lrSubErrorSumP <-
        "WARNING: P-value is very small and is being replaced with 0.000001 for aesthetic purposess"
    } else
    {
      reactDataComponents$lrSubErrorSumP <- ""
    }
    summ$coefficients <- round(summ$coefficients, 4)
    summ$coefficients[, 4] <-
      ifelse(summ$coefficients[, 4] < 0.000001, 0.000001, summ$coefficients[, 4])
    reactDataComponents$lrModelSummary <- summ
    
    modelSubmitTrain <-  list(
      model = model,
      predProb = predTrain,
      label = factor(train[, depVar], levels = factorLabels),
      rocLinePlot = lrOutput[[6]],
      auc = lrOutput[[2]]
    )
    ### For validation
    predValid = lrPredictProb(model, test, factorLabels)
    lrOutputValid <-
      allAnalysisOnPreds(
        factor(test[, depVar], levels = factorLabels),
        predValid,
        modelColours,
        factorLabels,
        primaryColor,
        secondaryColor
      )
    modelSubmit <-  list(
      model = model,
      predProb = predValid,
      label = factor(test[, depVar], levels = factorLabels),
      rocLinePlot = lrOutputValid[[6]],
      auc = lrOutputValid[[2]]
    )
    remove(model,
           predTrain,
           lrOutput,
           accPresRecallPlots,
           summ,
           predValid,
           lrOutputValid)
    outputtbl<-
      list(
        msTrain = modelSubmitTrain,
        ms = modelSubmit,
        rd = reactDataComponents,
        factorLabels = factorLabels
      )
    
    return(list_to_df(outputtbl))
  }

# Set the threshold for predicting the test data
#' @name lrsetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using logistic regression
#' @param modelSubmitTrain list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

lrsetThreshold <- function(modelSubmitTrain,factorLabels,modelSubmit, xSelected, thresh,  positiveClass)
{
  reactDataComponents = list()
  modelSubmitTrain$LR$pred = predictClass(modelSubmitTrain$LR$predProb,
                                          factorLabels,
                                          as.numeric(thresh))
  reactDataComponents$lrcm <-
    caret::confusionMatrix(modelSubmitTrain$LR$pred,
                           modelSubmitTrain$LR$label,
                           positive = positiveClass)
  reactDataComponents$commonCM <- reactDataComponents$lrcm
  modelSubmitTrain$LR$confusionMatrix <- reactDataComponents$lrcm
  modelSubmitTrain$LR$featuresSelected <- xSelected
  modelSubmitTrain$LR$factorLabels <- factorLabels
  modelSubmitTrain$LR$threshold <- thresh
  modelSubmitTrain$LR$predFunc = function(model, test, factorLabels, thresh =
                                            0.5) {
    pred = stats::predict(model, as.data.frame(test, stringsAsFactors = F), type =
                            "response")
    p = cut(
      pred,
      breaks = c(0, as.numeric(thresh), 1),
      labels = factorLabels,
      include.lowest = T
    )
    return(p)
  }
  ### For validation
  modelSubmit$LR$pred = predictClass(modelSubmit$LR$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$LR$confusionMatrix = caret::confusionMatrix(modelSubmit$LR$pred, modelSubmit$LR$label,
                                                          positive = positiveClass)
  modelSubmit$LR$featuresSelected = xSelected
  modelSubmit$LR$factorLabels = factorLabels
  modelSubmit$LR$threshold = thresh
  modelSubmit$LR$predFunc = function(model, test, factorLabels, thresh =
                                       0.5) {
    pred = predict(model, as.data.frame(test, stringsAsFactors = F), type =
                     "response")
    p = cut(
      pred,
      breaks = c(0, as.numeric(thresh), 1),
      labels = factorLabels,
      include.lowest = T
    )
    return(p)
  }
  outputtbl <-  list(msTrain = modelSubmitTrain, ms = modelSubmit, rd = reactDataComponents)
  return(list_to_df(outputtbl))
}

# Select features using logistic regression
#' @name partialLeastSquaresFeatureSelection
#' @title Get importance of features using Partial Least Squares
#' @details Get the rank of importance of each of the feature in the data using Partial Least Squares
#' @param dataset the data from which linearly dependent columns are removed
#' @param factorLabels the levels of factors present in dependent variable
#' @param fsWindow number of features to be displayed in graph
#' @param depVar predicted Variable
#' @param color the color of the graph
#' @return gives a graph showing the rank of importance of the features
#' @family Package classification Utilites functions
#' @export


partialLeastSquaresFeatureSelection <-
  function(dataset,factorLabels,
           depVar,
           fsWindow,
           color)
  {
    reactDataComponents <- list()
    model <-
      caret::plsda(dataset[, setdiff(colnames(dataset), depVar), drop = F], dataset[, depVar], ncomp =
                     2, probMethod = "softmax")
    res <- data.frame(plsVarSel::VIP(model, 2))
    colnames(res) <- "Importance"
    plotDataFrame <- res
    plotDataFrame <-
      plotDataFrame[order(plotDataFrame[, "Importance"], decreasing = T), , drop =
                      F]
    plsPlot <- ggplot2::ggplot(plotDataFrame[1:fsWindow, , drop = F],
                               ggplot2::aes(x = factor(
                                 rownames(plotDataFrame[1:fsWindow, , drop = F]),
                                 levels = rownames(plotDataFrame)[order(plotDataFrame[1:fsWindow, "Importance"])]
                               ),
                               y = plotDataFrame[1:fsWindow, "Importance"])) +
      ggplot2::geom_bar(
        color = "#FFFFFF",
        fill = color,
        stat = "identity",
        position = "dodge"
      ) +
      ggplot2::coord_flip() + ggplot2::ylab("Importance") + ggplot2::xlab("Features") + ggplot2::labs(title = "PLS") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
        axis.title = ggplot2::element_text(size = 14)
      )
    reactDataComponents$PLSImpVar <-
      data.frame("Features" = rownames(plotDataFrame),
                 "Importance" = plotDataFrame)
    # reactDataComponents$PLSImpVar <- rownames(plotDataFrame)
    reactDataComponents$plsFSError <- ""
    outputtbl <- list(
      dataset = dataset,
      reactDataComponents,
      factorLabels = factorLabels,
      plsPlot
    )
    return(list_to_df(outputtbl))
  }

# Fit a Partial Least Squares Model
#' @name plsfit
#' @title Fit a Partial Least Squares Model
#' @details Fit a Partial Least Squares Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param ncomp Number of components
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export
plsFit <- function(train,test,factorLabels,
                   xSelected,
                   depVar,
                   cvK = 3,
                   modelColours,
                   ncomp = 2,
                   primaryColor,
                   secondaryColor)
{
  caretOut <-
    plsTrain(train[, xSelected, drop = F],
             factor(train[, depVar], levels = factorLabels),
             cvK,
             factorLabels,
             ncomp = 2)
  
  model <- caretOut$finalModel
  
  # generate cross validation plots
  if (cvK > 1) {
    cvPlot <-
      crossValidationPlot(caretOut$resample,
                          'Accuracy',
                          'PLS',
                          primaryColor,
                          secondaryColor)
    cvCM <-
      caretOut$resampledCM[, c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  
  reactDataComponents = list()
  reactDataComponents$plscvPlot <- cvPlot
  reactDataComponents$plscvCM <- cvCM
  
  # PLS loadings
  reactDataComponents$plsModelSummary <-
    summary(model$loadings, digits = 4)
  reactDataComponents$plsModelLoadings <- model$loadings
  
  # get RMSEP plot
  # This library load is required to access the MSEP function called by RMSEP
  library(pls)
  temp <- pls::RMSEP(model)
  # Detaching pls alone fails: says that plsVarSel requires pls so cannot be detached
  # unloadNamespace(plsVarSel)
  # unloadNamespace(pls)
  # detach("package:plsVarSel", unload = TRUE)
  detach("package:pls")
  # print("4")
  t <- temp$val
  table <- as.data.frame(t[1, 1, temp$comps])
  t2 <- as.data.frame(temp$comps)
  table <- cbind.data.frame(t2[2:(ncomp + 1), ], table)
  rownames(table) <- NULL
  colnames(table) <- c("Components", "RMSEP")
  reactDataComponents$plsRmsepplot <-
    plsRmsepPlot(table, primaryColor) + ggplot2::theme(
      legend.position = c(0.8, 0.8),
      legend.background = ggplot2::element_rect(fill =
                                                  ggplot2::alpha('white', 0)),
      legend.direction = "vertical",
      plot.title = ggplot2::element_text(hjust = 0.5, size =
                                           20),
      axis.title = ggplot2::element_text(size =
                                           14)
    )
  
  pred <-
    plsPredictProb(model, train[, xSelected, drop = F], factorLabels)
  plsOutput <-
    allAnalysisOnPreds(
      factor(train[, depVar], levels = factorLabels),
      pred,
      modelColours,
      factorLabels,
      primaryColor,
      secondaryColor
    )
  modelSubmitTrain <- list(
    model = model,
    predProb = pred,
    label = factor(train[, depVar], levels = factorLabels),
    rocLinePlot = plsOutput[[6]],
    auc = plsOutput[[2]]
  )
  ### Pretty plots
  reactDataComponents$plsFancyProbDenPlot <-
    densityFunc(factor(train[, depVar], levels = factorLabels),
                pred,
                primaryColor,
                secondaryColor)
  accPresRecallPlots <-
    customPlots(factor(train[, depVar], levels = factorLabels),
                pred,
                factorLabels,
                primaryColor)
  plotTheme <- ggplot2::theme(
    legend.position = c(0.8, 0.8),
    legend.background = ggplot2::element_rect(fill =
                                                ggplot2::alpha('white', 0)),
    legend.direction = "vertical",
    plot.title = ggplot2::element_text(hjust = 0.5, size =
                                         20),
    axis.title = ggplot2::element_text(size =
                                         14)
  )
  reactDataComponents$plsPrecPlot <-
    accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$plsAccPlot <-
    accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$plsRecallPlot <-
    accPresRecallPlots[[3]] + plotTheme
  reactDataComponents$plsROCplot <- plsOutput[[1]]
  reactDataComponents$plsSSTplot <- plsOutput[[3]]
  Metric = rbind("Area Under Curve", "Optimal threshold")
  Values = as.numeric(rbind(plsOutput[[2]], plsOutput[[4]]))
  if (length(xSelected) > 2)
  {
    mat <-
      cor(
        train[, xSelected, drop = F] ,
        y = NULL,
        use = "everything",
        method = c("pearson", "kendall", "spearman")
      )
    reactDataComponents$mat <- mat
    corMat <- base::round(mat, 3)
    corMat <- reshape2::melt(corMat)
    meltd <- reshape2::melt(corMat, na.rm = T)
    loadingPlotAndTable <- PLSloadingsPlot(modelSubmitTrain$model)
    reactDataComponents$loadingPlotAndTable <- loadingPlotAndTable
    reactDataComponents$PLSloadingsPlot <-
      qgraph::qgraph.loadings(loadingPlotAndTable[[1]],
                              posCol = primaryColor,
                              negCol = secondaryColor)
    reactDataComponents$load <- loadingPlotAndTable[[2]]
    # reactDataComponents$plsAOCTable <- data.frame(Metric, Values)
    # reactDataComponents$plsOptTh <- round(plsOutput[[4]],2)
    reactDataComponents$mat <- mat
    corrNetTable <- cbind(c(1:ncol(mat)), colnames(mat))
    colnames(corrNetTable) <- c("Node ID", "Column Name")
    remove(mat, corMat, meltd, loadingPlotAndTable, corrNetTable)
  }
  reactDataComponents$plsAOCTable <- data.frame(Metric, Values)
  reactDataComponents$plsOptTh <- round(plsOutput[[4]], 2)
  predValid <-
    plsPredictProb(modelSubmitTrain$model, test[, xSelected, drop = F], factorLabels)
  plsAnalysis <-
    allAnalysisOnPreds(
      factor(test[, depVar], levels = factorLabels),
      predValid,
      modelColours,
      factorLabels,
      primaryColor,
      secondaryColor
    )
  modelSubmit <- list(
    model = modelSubmitTrain$model,
    predProb = predValid,
    label = factor(test[, depVar], levels = factorLabels),
    rocLinePlot = plsAnalysis[[6]],
    auc = plsAnalysis[[2]]
  )
  reactDataComponents$plsFitError <- ""
  remove(
    temp,
    t,
    table,
    t2,
    model,
    label,
    pred,
    plsOutput,
    accPresRecallPlots,
    predValid,
    plsAnalysis
  )
  
  outputtbl<-list(
    msTrain = modelSubmitTrain,
    ms = modelSubmit,
    rd = reactDataComponents,
    factorLabels = factorLabels
  )
  return(list_to_df(outputtbl))
}

# Set the threshold for predicting the test data
#' @name plssetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using partial least squares
#' @param modelSubmitTrain list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

plssetThreshold <- function(mst,ms,factorLabels, xSelected, thresh,  positiveClass)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$PLS <- mst
  modelSubmit$PLS <- ms
  
  reactDataComponents = list()
  
  modelSubmitTrain$PLS$pred = predictClass(modelSubmitTrain$PLS$predProb,
                                           factorLabels,
                                           as.numeric(thresh))
  reactDataComponents$plsSummary = caret::confusionMatrix(modelSubmitTrain$PLS$pred,
                                                          modelSubmitTrain$PLS$label,
                                                          positive = positiveClass)
  modelSubmitTrain$PLS$confusionMatrix = reactDataComponents$plsSummary
  modelSubmitTrain$PLS$featuresSelected = xSelected
  modelSubmitTrain$PLS$threshold = thresh
  modelSubmitTrain$PLS$factorLabels = factorLabels
  modelSubmitTrain$PLS$predFunc = function(model, test, factorLabels, thresh =
                                             0.5) {
    # pred <- predict(model, as.matrix(test), type = "prob")
    
    pred <- predict(model, test, type = "prob")
    pred <- pred[, , 1]
    pred <- pred[, factorLabels[2]]
    
    p = cut(
      pred,
      breaks = c(0, as.numeric(thresh), 1),
      labels = factorLabels,
      include.lowest = T
    )
    return(p)
  }
  ### validation
  modelSubmit$PLS$pred <-
    predictClass(modelSubmit$PLS$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$PLS$confusionMatrix = caret::confusionMatrix(modelSubmit$PLS$pred, modelSubmit$PLS$label,
                                                           positive = positiveClass)
  modelSubmit$PLS$featuresSelected = xSelected
  modelSubmit$PLS$threshold = as.numeric(thresh)
  modelSubmit$PLS$factorLabels = factorLabels
  modelSubmit$PLS$predFunc = function(model, test, factorLabels, thresh =
                                        0.5) {
    # pred <- predict(model, as.matrix(test), type = "prob")
    pred <- stats::predict(model, test, type = "prob")
    pred <- pred[, , 1]
    pred <- pred[, factorLabels[2]]
    p = cut(
      pred,
      breaks = c(0, as.numeric(thresh), 1),
      labels = factorLabels,
      include.lowest = T
    )
    return(p)
  }
  reactDataComponents$plsSubError <- ""
  outputtbl <- list(msTrain = modelSubmitTrain, ms = modelSubmit, rd = reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}

# Select features using logistic regression
#' @name marsFeatureSelection
#' @title Get importance of features using MARS
#' @details Get the rank of importance of each of the feature in the data using MARS
#' @param dataset the data from which linearly dependent columns are removed
#' @param factorLabels the levels of factors present in dependent variable
#' @param fsWindow number of features to be displayed in graph
#' @param depVar predicted Variable
#' @param color the color of the graph
#' @return gives a graph showing the rank of importance of the features
#' @family Package classification Utilites functions
#' @export


marsFeatureSelection <-function(dataset,factorLabels,
                  depVar,
                  fsWindow,
                  color )
{
  reactDataComponents <- list()
  ev <- MARSRegression(dataset, depVar, factorLabels)
  ranks <-  ev[, 4, drop = F]
  plotDataFrame <- as.data.frame(ev[, 4], drop = F)
  colnames(plotDataFrame)[1] <- "Importance"
  vidf <-
    data.frame(Features = row.names(ranks), Importance = as.vector(round(ev[, 4], 2)))
  vidf <- vidf[order(-vidf$Importance), ]
  colnames(vidf) <- c("Features", "Importance")
  reactDataComponents$MARSImpVar <- vidf
  # reactDataComponents$MARSImpVar <- names(sort(-ranks))
  # min() is used for plotting since MARS doesn't necessarily return all the features
  reactDataComponents$marsPlot <-
    ggplot2::ggplot(plotDataFrame[1:min(fsWindow, nrow(plotDataFrame)), , drop =
                                    F],
                    ggplot2::aes(x = factor(
                      rownames(plotDataFrame[1:min(fsWindow, nrow(plotDataFrame)), , drop = F]),
                      levels =
                        rownames(plotDataFrame)[order(plotDataFrame[1:min(fsWindow, nrow(plotDataFrame)), "Importance"])]
                    ),
                    y = plotDataFrame[1:min(fsWindow, nrow(plotDataFrame)), "Importance"])) +
    ggplot2::geom_bar(
      color = "#FFFFFF",
      fill = color,
      stat = "identity",
      position = "dodge"
    ) +
    ggplot2::coord_flip() + ggplot2::ylab("Importance") + ggplot2::xlab("Features") + ggplot2::labs(title = "MARS") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
      axis.title = ggplot2::element_text(size = 14)
    )
  reactDataComponents$MARSFSError <- ""
  outputtbl <-
    list(
      dataset = dataset,
      reactDataComponents = reactDataComponents,
      factorLabels = factorLabels
    )
  return(list_to_df(outputtbl))
}

# Fit a MARS Model
#' @name marsfit
#' @title Fit a MARS Model
#' @details Fit a MARS Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export

marsFit <- function(train,test,factorLabels,
                    xSelected,
                    depVar,
                    cvK = 3,
                    modelColours,
                    primaryColor,
                    secondaryColor)
{
  caretOut <- marsTrain(train, xSelected, depVar, cvK, factorLabels)
  model <- caretOut$finalModel
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'MARS', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  
  pred <- marsPredictProb(model, train[,xSelected,drop=F], factorLabels)
  analysis <- allAnalysisOnPreds(factor(train[,depVar], levels=factorLabels), pred, modelColours, 
                                 factorLabels, primaryColor, secondaryColor)
  
  reactDataComponents = list()
  reactDataComponents$marscvPlot <- cvPlot
  reactDataComponents$marscvCM <- cvCM
  
  reactDataComponents$marsFancyProbDenPlot <- densityFunc(factor(train[,depVar], levels=factorLabels), 
                                                          pred, primaryColor, secondaryColor)
  accPresRecallPlots <- customPlots(factor(train[,depVar], levels=factorLabels),pred, factorLabels, primaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill=ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  reactDataComponents$marsPrecPlot <- accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$marsAccPlot <- accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$marsRecallPlot <- accPresRecallPlots[[3]] + plotTheme
  
  reactDataComponents$marsROCPlot <- analysis[[1]]
  reactDataComponents$marsSSTPlot <- analysis[[3]]
  
  Metrics = rbind("Area Under Curve","Optimal threshold")
  Values = as.numeric(rbind(analysis[[2]],analysis[[4]]))
  reactDataComponents$marsAOCTable <- data.frame(Metrics, Values)
  reactDataComponents$marsOptTh <- round(analysis[[4]],2)
  
  reactDataComponents$marsModelSummary <- summary(model, digits = 4)
  modelSubmitTrain <- list(
    model = model,
    predProb = analysis[[5]],
    label = factor(train[,depVar], levels=factorLabels),
    rocLinePlot = analysis[[6]],
    auc = analysis[[2]]
  )
  ### validation
  predValid <- marsPredictProb(model, test[,xSelected,drop=F], factorLabels)
  analysisValid <- allAnalysisOnPreds(factor(test[,depVar], levels=factorLabels), predValid, modelColours, 
                                      factorLabels, primaryColor, secondaryColor)
  modelSubmit <- list(
    model = model,
    predProb = analysisValid[[5]],
    label = factor(test[,depVar], levels=factorLabels),
    rocLinePlot = analysisValid[[6]],
    auc = analysisValid[[2]]
  )
  remove(model, pred, analysis, accPresRecallPlots, predValid, analysisValid)
  outputtbl <- list(msTrain=modelSubmitTrain, ms=modelSubmit, rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}
# Set the threshold for predicting the test data
#' @name marssetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using MARS
#' @param modelSubmitTrain list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param y the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

marssetThreshold <- function(mst,ms,factorLabels, xSelected, thresh,  y)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$MARS <- mst
  modelSubmit$MARS <- ms
  reactDataComponents = list()
  modelSubmitTrain$MARS$pred <- predictClass(modelSubmitTrain$MARS$predProb, factorLabels, as.numeric(thresh))
  reactDataComponents$marsSummary <- caret::confusionMatrix(modelSubmitTrain$MARS$pred, modelSubmitTrain$MARS$label, 
                                                            positive = factorLabels[2])
  reactDataComponents$commonCM <- reactDataComponents$marsSummary
  
  modelSubmitTrain$MARS$confusionMatrix <- reactDataComponents$marsSummary
  modelSubmitTrain$MARS$featuresSelected <- xSelected
  modelSubmitTrain$MARS$threshold <- thresh
  modelSubmitTrain$MARS$factorLabels <- factorLabels
  modelSubmitTrain$MARS$predFunc <- function(model, test, factorLabels, thresh=0.5){
    return(predict(model, as.matrix(test), type='class', thresh=thresh))
  }
  ### validation
  modelSubmit$MARS$pred <- predictClass(modelSubmit$MARS$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$MARS$confusionMatrix <- caret::confusionMatrix(modelSubmit$MARS$pred, modelSubmit$MARS$label, 
                                                             positive = factorLabels[2])
  modelSubmit$MARS$featuresSelected <- xSelected
  modelSubmit$MARS$threshold <- as.numeric(thresh)
  modelSubmit$MARS$factorLabels <- factorLabels
  modelSubmit$MARS$predFunc <- function(model, test, factorLabels, thresh=0.5){
    return(predict(model, as.matrix(test), type='class', thresh=thresh))
  }
  outputtbl <- list(msTrain=modelSubmitTrain, ms=modelSubmit, rd=reactDataComponents)
  return(list_to_df(outputtbl))
}


# Select features using LARS
#' @name larsFeatureSelection
#' @title Get importance of features using Partial Least Squares
#' @details Get the rank of importance of each of the feature in the data using LARS
#' @param dataset the data from which linearly dependent columns are removed
#' @param factorLabels the levels of factors present in dependent variable
#' @param fsWindow number of features to be displayed in graph
#' @param depVar predicted Variable
#' @param color the color of the graph
#' @return gives a graph showing the rank of importance of the features
#' @family Package classification Utilites functions
#' @export


larsFeatureSelection <- function(dataset,factorLabels,
                   depVar,
                   fsWindow,
                   color )
{
  reactDataComponents <- list()
  plotDataFrame <- LARSRegression(dataset, depVar, factorLabels) 
  rowNamesForLarsFS <- plotDataFrame[,1]
  plotDataFrame = as.data.frame(plotDataFrame[,2])
  colnames(plotDataFrame)[1] <- "Importance"
  rownames(plotDataFrame) <- rowNamesForLarsFS
  plotD <- plotDataFrame
  vidf <- data.frame("Features" = rownames(plotD), "Importance" = plotD)
  reactDataComponents$larsPlot <- ggplot2::ggplot(plotD[1:fsWindow,,drop=F], ggplot2::aes(x=factor(rownames(plotD[1:fsWindow,,drop=F]),
                                                                                                   levels=rownames(plotD)[order(plotD[1:fsWindow,"Importance"])]),
                                                                                          y=plotD[1:fsWindow,"Importance"])) +
    ggplot2::geom_bar(color="#FFFFFF",fill=color,stat = "identity",position = "dodge") +
    ggplot2::coord_flip() + ggplot2::ylab("Importance") + ggplot2::xlab("Features")+ ggplot2::labs(title= "LARS") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                   axis.title= ggplot2::element_text(size=14))
  reactDataComponents$LARSImpVar <-  vidf
  reactDataComponents$LARSFSError <- ""
  
  outputtbl<-    list(
    dataset = dataset,
    reactDataComponents = reactDataComponents,
    factorLabels = factorLabels
  )
  return(list_to_df(outputtbl))
}


# Select features using random forest
#' @name randomForestFeatureSelection
#' @title Get importance of features using random forest
#' @details Get the rank of importance of each of the feature in the data using random forest
#' @param dataset the data from which linearly dependent columns are removed
#' @param factorLabels the levels of factors present in dependent variable
#' @param fsWindow number of features to be displayed in graph
#' @param depVar predicted Variable
#' @param color the color of the graph
#' @return gives a graph showing the rank of importance of the features
#' @family Package classification Utilites functions
#' @export


randomForestFeatureSelection <- function(dataset,factorLabels, depVar ,
                           fsWindow,
                           color , ntrees=100, mtry=6, minNode=1, imp="impurity", rep=T)
{
  reactDataComponents <- list()
  rfVarImp <- RandomForestVariableImportance(dataset, setdiff(colnames(dataset), depVar), depVar, 
                                             numTrees=ntrees, mTry=mtry, minNodeSize=as.integer(minNode), 
                                             Importance= imp, replace=rep) 
  rows = rev(rownames(rfVarImp)[order(rfVarImp[,1])])
  rfVarImp = as.data.frame(rfVarImp[rows,])
  rownames(rfVarImp) <- rows
  colnames(rfVarImp) <- "Importance"
  
  fsWindow <- min(fsWindow, nrow(rfVarImp))
  xRandomForest = factor(rownames(rfVarImp[1:fsWindow,,drop=F]), 
                         levels = rownames(rfVarImp)[order(rfVarImp[1:fsWindow,"Importance"])])
  
  reactDataComponents$rfPlot <- ggplot2::ggplot(rfVarImp[1:fsWindow,,drop=F], ggplot2::aes(x=xRandomForest,y=rfVarImp[1:fsWindow,"Importance"])) + 
    ggplot2::geom_bar(color="#FFFFFF",fill=color, stat = "identity",position = "dodge") + ggplot2::coord_flip() +
    ggplot2::ylab("Importance") + ggplot2::xlab("Features") + ggplot2::labs(title= "Random Forest") + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                   axis.title= ggplot2::element_text(size=14))
  reactDataComponents$RFImpVar <- data.frame("Features" = rownames(rfVarImp), "Importance" = rfVarImp)
  reactDataComponents$RFFSError <- ""
  outputtbl<-list(
    dataset = dataset,
    reactDataComponents = reactDataComponents,
    factorLabels = factorLabels
  )
  return(list_to_df(outputtbl))
}


# Fit a Random forest Model
#' @name rffit
#' @title Fit a Random forest Model
#' @details Fit a Random forest Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param ntree number of trees in the forest
#' @param mtry maximum feaatures to consider for split
#' @param nodesize minimum size of terminal nodes
#' @param RFImportance variable importance mode
#' @param RFReplace boolean to say whether to replace while sampling
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export
rfFit <- function(train,test,factorLabels, xSelected, ntree, mtry,nodesize, RFImportance, RFReplace,y ,
                  cvK = 3,
                  modelColours,
                  primaryColor,
                  secondaryColor)
  
{
  train[,y] = factor(train[,y], levels=factorLabels)
  reactDataComponents = list()
  
  # trees vs error
  reactDataComponents$mseRFPlot <- rfErrorVsTreePlot(train, xSelected, y, ntree, mtry, nodesize,
                                                     RFImportance, RFReplace, primaryColor)
  # model
  caretOut <- rfTrain(train, xSelected, y, cvK, ntree, mtry, nodesize, RFImportance, RFReplace)
  model <- caretOut$finalModel
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'Random Forest', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  reactDataComponents$rfcvPlot <- cvPlot
  reactDataComponents$rfcvCM <- cvCM
  
  reactDataComponents$rfPred <- rfPredictProb(model, train[,xSelected,drop=F], factorLabels)[,factorLabels[2]]
  rfOutput <- allAnalysisOnPreds(factor(train[,y], levels=factorLabels), reactDataComponents$rfPred, 
                                 modelColours$RF, factorLabels, primaryColor, secondaryColor)
  modelSubmitTrain <- list(
    model = model,
    predProb = reactDataComponents$rfPred, 
    label = factor(train[,y], levels=factorLabels),
    rocLinePlot = rfOutput[[6]],
    auc = rfOutput[[2]]
  )
  
  ### Pretty plots
  reactDataComponents$rfFancyProbDenPlot <- densityFunc(factor(train[,y],levels = factorLabels),
                                                        reactDataComponents$rfPred, primaryColor, secondaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  tryCatch(
    {
      accPresRecallPlots <- customPlots(factor(train[,y],levels = factorLabels),reactDataComponents$rfPred,
                                        factorLabels, primaryColor)
      reactDataComponents$rfPrecPlot <- accPresRecallPlots[[2]] + plotTheme
      reactDataComponents$rfAccPlot <- accPresRecallPlots[[1]] + plotTheme
      reactDataComponents$rfRecallPlot <- accPresRecallPlots[[3]] + plotTheme
    },error = function(x)
    {
      accPresRecallPlots <- customPlots(factor(train[,y],levels = factorLabels),reactDataComponents$rfPred,factorLabels)
      reactDataComponents$rfPrecPlot <- accPresRecallPlots[[2]] + plotTheme
      reactDataComponents$rfAccPlot <- accPresRecallPlots[[1]] + plotTheme
      reactDataComponents$rfRecallPlot <- accPresRecallPlots[[3]] + plotTheme
    })
  reactDataComponents$rfROCPlot = rfOutput[[1]]
  reactDataComponents$rfSSTPlot = rfOutput[[3]]
  # AUC and Optimal threshold values
  Metric = rbind("Area Under Curve","Optimal threshold")
  Values = as.numeric(rbind(rfOutput[[2]],rfOutput[[4]]))
  reactDataComponents$rfAOCTable <- data.frame(Metric, Values)
  reactDataComponents$rfOptTh <- round(rfOutput[[4]],2)
  reactDataComponents$rfModelSummary <- model
  
  ### validation
  predValid <- rfPredictProb(model, test[,xSelected,drop=F], factorLabels)[,factorLabels[2]]
  rfAnalysis <- allAnalysisOnPreds(factor(test[,y], levels=factorLabels), predValid, modelColours$RF, 
                                   factorLabels, primaryColor, secondaryColor)
  modelSubmit <- list(
    model = model,
    predProb = predValid,
    label = factor(test[,y], levels=factorLabels),
    rocLinePlot = rfAnalysis[[6]],
    auc = rfAnalysis[[2]]
  )
  remove(model, rfOutput, accPresRecallPlots, predValid, rfAnalysis)
  outputtbl<-
    list(
      msTrain = modelSubmitTrain,
      ms = modelSubmit,
      rd = reactDataComponents,
      factorLabels = factorLabels,
      test = test
    )
  return(list_to_df(outputtbl))
}

# Set the threshold for predicting the test data
#' @name rfsetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using Random forest
#' @param modelSubmitTrain list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @param test the test data
#' @param rfPred Random forest prediction probabilities
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

rfsetThreshold <- function( mst,ms,factorLabels, xSelected, thresh,  positiveClass,test,rfPred)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$RF <- mst
  modelSubmit$RF <- ms
  test <- object[[5]]
  
  reactDataComponents = list()
  modelSubmitTrain$RF$pred <- predictClass(modelSubmitTrain$RF$predProb, factorLabels, as.numeric(thresh))
  reactDataComponents$rfSummary = caret::confusionMatrix(modelSubmitTrain$RF$pred, modelSubmitTrain$RF$label,
                                                         positive = positiveClass)
  reactDataComponents$commonCM <- reactDataComponents$rfSummary
  modelSubmitTrain$RF$confusionMatrix = reactDataComponents$rfSummary
  modelSubmitTrain$RF$featuresSelected = xSelected
  modelSubmitTrain$RF$threshold = thresh
  modelSubmitTrain$RF$factorLabels = factorLabels
  modelSubmitTrain$RF$predFunc = function(model, test, factorLabels, thresh=0.5){
    pred = ranger::predictions(stats::predict(model, data.frame(test), type="response"))
    pred = pred[,make.names(factorLabels)]
    p = cut(pred[,2], breaks=c(0,as.numeric(thresh),1), labels=factorLabels, include.lowest = T)
    return(p)
  }
  # validation
  modelSubmit$RF$pred <- predictClass(modelSubmit$RF$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$RF$confusionMatrix = caret::confusionMatrix(modelSubmit$RF$pred, modelSubmit$RF$label,positive = positiveClass)
  modelSubmit$RF$featuresSelected = xSelected
  modelSubmit$RF$threshold = as.numeric(thresh)
  modelSubmit$RF$factorLabels = factorLabels
  modelSubmit$RF$predFunc = function(model, test, factorLabels, thresh=0.5, testCat=NULL, xSelected=NULL){
    if(!is.null(testCat))
    {
      test <- getCategoricalData(test, testCat, xSelected)$data
      # dataset <- cbind.data.frame(reactData$dataset[,predVariable, drop = FALSE], newData$data)
      # xSelected <- newData$x
      # remove(newData)
    }
    pred = ranger::predictions(stats::predict(model, data.frame(test), type="response"))
    pred = pred[,make.names(factorLabels)]
    p = cut(pred[,2], breaks=c(0,as.numeric(thresh),1), labels=factorLabels, include.lowest = T)
    return(p)
  }
  outputtbl <-list(msTrain = modelSubmitTrain, ms = modelSubmit, rd = reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}

# Select features using XG Boost
#' @name xgboostFeatureSelection
#' @title Get importance of features using XG Boost
#' @details Get the rank of importance of each of the feature in the data using XG Boost
#' @param dataset the data from which linearly dependent columns are removed
#' @param factorLabels the levels of factors present in dependent variable
#' @param fsWindow number of features to be displayed in graph
#' @param depVar predicted Variable
#' @param color the color of the graph
#' @return gives a graph showing the rank of importance of the features
#' @family Package classification Utilites functions
#' @export


xgboostFeatureSelection <- function(dataset,factorLabels,
                      depVar,
                      fsWindow ,
                      color ,maxDepth=4, learnRate=0.3, iters=10)
{
  
  reactDataComponents <- list()
  caretOut <- xgbTrain(dataset, depVar, factorLabels, 1, maxDepth, learnRate, iters)
  reactDataComponents$xgboostImpVar <- xgboost::xgb.importance(feature_names = setdiff(colnames(dataset),depVar), 
                                                               model = caretOut$finalModel)
  t <- as.data.frame(reactDataComponents$xgboostImpVar)
  t <- data.frame(t[,"Gain"], row.names=t[,"Feature"])
  colnames(t) <- "Gain"
  reactDataComponents$xgboostPlot <- ggplot2::ggplot(t[1:min(fsWindow,nrow(t)),,drop=F], ggplot2::aes(x=factor(rownames(t[1:min(fsWindow,nrow(t)),,drop=F]),
                                                                                                               levels=rownames(t)[order(t[1:min(fsWindow,nrow(t)),"Gain"])]),
                                                                                                      y=t[1:min(fsWindow,nrow(t)),"Gain"])) +
    ggplot2::geom_bar(color="#FFFFFF",fill=color,stat = "identity",position = "dodge") +
    ggplot2::coord_flip() + ggplot2::ylab("Importance") + ggplot2::xlab("Features")+ ggplot2::labs(title= "XGBoost") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                   axis.title= ggplot2::element_text(size=14))
  reactDataComponents$xgboostFSError <- ""
  outputtbl<-
    list(
      dataset = dataset,
      reactDataComponents = reactDataComponents,
      factorLabels = factorLabels
    )
  return(list_to_df(outputtbl))
}


# Fit a XG BOOST Model
#' @name xgbfit
#' @title Fit a XG BOOST Model
#' @details Fit a XG BOOST Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param xgboostMaxDepth number of trees in the forest
#' @param xgboostNoOfIterations maximum feaatures to consider for split
#' @param xgboostLeraningRate minimum size of terminal nodes
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export
xgbFit<-function(train,test,factorLabels, xSelected, depVar,
                 cvK = 3,xgboostMaxDepth =4, 
                 xgboostLeraningRate =0.3, xgboostNoOfIterations=10,
                 modelColours,
                 primaryColor,
                 secondaryColor)
  
{
  
  
  reactDataComponents<-list()
  train[,depVar] = factor(train[,depVar], levels=factorLabels)
  
  caretOut <- xgbTrain(train[,c(xSelected,depVar),drop=F], depVar, factorLabels, cvK, xgboostMaxDepth, 
                       xgboostLeraningRate, xgboostNoOfIterations)
  xgbModelOutput <- caretOut$finalModel
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'XGBoost', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  
  reactDataComponents$xgbcvPlot <- cvPlot
  reactDataComponents$xgbcvCM <- cvCM
  
  pred <- xgbPredictProb(xgbModelOutput, as.matrix(sapply(train[,xSelected,drop=F], function(x) as.numeric(x))))
  xgboostOutput <- allAnalysisOnPreds(factor(train[,depVar], levels=factorLabels), pred, modelColours$XGB, 
                                      factorLabels, primaryColor, secondaryColor)
  modelSubmitTrain<- list(
    model = xgbModelOutput,
    predProb = pred,
    label = factor(train[,depVar], levels=factorLabels),
    rocLinePlot = xgboostOutput[[6]],
    auc = xgboostOutput[[2]]
  )
  ### Pretty plot
  reactDataComponents$xgbFancyProbDenPlot <- densityFunc(factor(train[,depVar],levels = factorLabels), 
                                                         pred, primaryColor, secondaryColor)
  accPresRecallPlots <- customPlots(factor(train[,depVar],levels = factorLabels), pred, factorLabels, primaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  reactDataComponents$xgbPrecPlot <- accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$xgbAccPlot <- accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$xgbRecallPlot <- accPresRecallPlots[[3]] + plotTheme
  reactDataComponents$xgboostROCPlot = xgboostOutput[[1]]
  reactDataComponents$xgboostSSTPlot = xgboostOutput[[3]]
  # AUC and Optimal threshold values
  Metric = rbind("Area Under Curve","Optimal threshold")
  Values = as.numeric(rbind(xgboostOutput[[2]],xgboostOutput[[4]]))
  reactDataComponents$xgboostAOCTable <- data.frame(Metric, Values)
  reactDataComponents$xgboostOptTh <- round(xgboostOutput[[4]],2)
  predValid <- xgbPredictProb(xgbModelOutput, as.matrix(sapply(test[,xSelected,drop=F], function(x) as.numeric(x))))
  xgbAnalysis <- allAnalysisOnPreds(factor(test[,depVar], levels=factorLabels), predValid, modelColours$XGB, 
                                    factorLabels, primaryColor, secondaryColor)
  modelSubmit <- list(
    model = xgbModelOutput,
    predProb = predValid,
    label = factor(test[,depVar], levels=factorLabels),
    rocLinePlot = xgbAnalysis[[6]],
    auc = xgbAnalysis[[2]]
  )
  reactDataComponents$xgbFitError2 <- ""
  remove(xgbModelOutput, pred, xgboostOutput, accPresRecallPlots, predValid, xgbAnalysis)
  outputtbl<-
    list(
      msTrain = modelSubmitTrain,
      ms = modelSubmit,
      rd = reactDataComponents,
      factorLabels = factorLabels
    )
  return(list_to_df(outputtbl))
  
}
# Set the threshold for predicting the test data
#' @name xgbsetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using Xg boost
#' @param mst list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param ms list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

xgbsetThreshold<-function( mst,ms,factorLabels, xSelected, thresh,  positiveClass)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$XGB <- mst
  modelSubmit$XGB <- ms
  
  reactDataComponents = list()
  modelSubmitTrain$XGB$pred <- predictClass(modelSubmitTrain$XGB$predProb, factorLabels, as.numeric(thresh))
  reactDataComponents$xgboostSummary = caret::confusionMatrix(modelSubmitTrain$XGB$pred, 
                                                              modelSubmitTrain$XGB$label, positive = positiveClass)
  reactDataComponents$commonCM <- reactDataComponents$xgboostSummary
  modelSubmitTrain$XGB$confusionMatrix = reactDataComponents$xgboostSummary
  modelSubmitTrain$XGB$featuresSelected = xSelected
  modelSubmitTrain$XGB$threshold = as.numeric(thresh)
  modelSubmitTrain$XGB$factorLabels = factorLabels
  modelSubmitTrain$XGB$predFunc = function(model, test, factorLabels, thresh=0.5){
    names = colnames(test)
    test = data.frame(sapply(test, as.numeric))
    colnames(test) <- names
    pred = predict(model, as.matrix(test), probability = T)
    pred = predict(model, as.matrix(test), probability = T)
    p = cut(pred, breaks=c(0,as.numeric(thresh),1), labels=factorLabels, include.lowest = T)
    return(p)
  }
  #validation
  modelSubmit$XGB$pred <- predictClass(modelSubmit$XGB$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$XGB$confusionMatrix = caret::confusionMatrix(modelSubmit$XGB$pred, modelSubmit$XGB$label, 
                                                           positive = positiveClass)
  modelSubmit$XGB$featuresSelected = xSelected
  modelSubmit$XGB$threshold = as.numeric(thresh)
  modelSubmit$XGB$factorLabels = factorLabels
  modelSubmit$XGB$predFunc = function(model, test, factorLabels, thresh=0.5){
    names = colnames(test)
    test = data.frame(sapply(test, as.numeric))
    colnames(test) <- names
    pred = predict(model, as.matrix(test), probability = T)
    p = cut(pred, breaks=c(0,as.numeric(thresh),1), labels=factorLabels,include.lowest = T)
    return(p)
  }
  outputtbl <-list(msTrain = modelSubmitTrain, ms = modelSubmit, rd = reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}
# Fit a GAM Model
#' @name gamfit
#' @title Fit a GAM Model
#' @details Fit a GAM Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param splines the splines method to smooth
#' @param colsToSmooth the columns which are to be smoothed
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export

gamFit <- function(train,test,factorLabels,
                   xSelected,
                   depVar,
                   cvK = 3,
                   modelColours,
                   primaryColor,
                   secondaryColor,colsToSmooth, 
                   splines)
{
  
  reactDataComponents <- list()
  formula<-gamCreateFormula(train[,c(xSelected,depVar),drop=F],depVar,colsToSmooth,splines)
  train[,depVar] <- factor(train[,depVar], levels=factorLabels)
  
  caretOut <- gamTrainWithCV(data = train[,c(xSelected,depVar),drop=F], y = depVar, cvK = cvK, formula = formula, 
                             method = "REML", family = 'binomial')
  model <- caretOut$finalModel
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'GAM', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  reactDataComponents$gamcvPlot <- cvPlot
  reactDataComponents$gamcvCM <- cvCM
  
  # GAM smooth plots
  reactDataComponents$smoothPlot <- NULL
  
  if((length(colsToSmooth)!=0))
  {
    model$data <- train[,c(xSelected,depVar),drop=F]
    plotdata <- visreg::visreg(model, type = "contrast", plot = FALSE)
    model$data <- NULL
    if(length(xSelected)==1) {
      part<-plotdata
      smooths <- data.frame(variable = part$meta$x,
                            x=part$fit[[part$meta$x]],
                            smooth=part$fit$visregFit,
                            lower=part$fit$visregLwr,
                            upper=part$fit$visregUpr)
      smooths <- smooths[(smooths$variable %in% colsToSmooth),,drop=F]
      
    } else {
      smooths <- plyr::ldply(plotdata, function(part)
        data.frame(variable = part$meta$x,
                   x=part$fit[[part$meta$x]],
                   smooth=part$fit$visregFit,
                   lower=part$fit$visregLwr,
                   upper=part$fit$visregUpr))
      smooths <- smooths[(smooths$variable %in% colsToSmooth),,drop=F]
    }
    plotDataNative <- graphics::plot(model, pages = 1)
    plotDataNativeClean <- data.frame(raw = numeric(0), yDummy = numeric(0), variable = character(0))
    ### Apologies for the for loop in R
    for(i in 1:length(plotDataNative)) {
      raw = plotDataNative[[i]]$raw      
      variable = plotDataNative[[i]]$xlab
      plotDataNativeClean <- rbind(plotDataNativeClean, data.frame(raw = raw, yDummy = 1, variable = variable))
    }
    yDummy <- 1
    reactDataComponents$smoothPlot <- ggplot2::ggplot(smooths, ggplot2::aes(x, smooth)) + ggplot2::geom_line(color=primaryColor) +
      ggplot2::geom_line(ggplot2::aes(y=lower), linetype="dashed" , col= secondaryColor) +
      ggplot2::geom_line(ggplot2::aes(y=upper), linetype="dashed" , col= secondaryColor) +
      ggplot2::geom_rug(data = plotDataNativeClean, ggplot2::aes(raw , yDummy), sides="b") +
      ggplot2::facet_wrap( ~ variable, scales="free")
    
    remove(plotdata, plotDataNative, plotDataNativeClean, smooths)
  }
  ####
  pred<-gamPredictProb(model, train[,xSelected,drop=F], "response")
  gamOutput <- allAnalysisOnPreds(train[,depVar], pred, modelColours$GAM, factorLabels, primaryColor, secondaryColor)
  modelSubmitTrain <- list(
    model = model,
    predProb = pred,
    label = factor(train[,depVar],levels = factorLabels),
    rocLinePlot = gamOutput[[6]],
    auc = gamOutput[[2]]
  )
  reactDataComponents$gamROCplot <- gamOutput[[1]]
  reactDataComponents$gamSSTplot <- gamOutput[[3]]
  
  Metric = rbind("Area Under Curve","Optimal threshold")
  Values = as.numeric(rbind(gamOutput[[2]],gamOutput[[4]]))
  
  reactDataComponents$gamAOCTable <- data.frame(Metric, Values)
  summ1<-summary(modelSubmitTrain$model)
  if(!is.null(summ1$s.table))
  {
    sTableP <- which(summ1$s.table[,"p-value"]<0.000001)
    summ1$s.table[sTableP,"p-value"] <- 0.000001
  }
  pTableP <- which(summ1$p.table[,"Pr(>|z|)"]<0.000001)
  summ1$p.table[pTableP,"Pr(>|z|)"] <- 0.000001
  if(length(pTableP)>0 || (!is.null(sTableP) && length(sTableP)>0))
  {
    reactDataComponents$gamSubErrorSummP <- "WARNING: P-value is very small and is being replaced with 0.000001 for aesthetic purpose"
  }
  
  reactDataComponents$gamModelSummary<-summ1
  reactDataComponents$gamOptTh <- round(gamOutput[[4]],2)
  pred = gamOutput[[5]]
  reactDataComponents$gamFancyProbDenPlot <- densityFunc(factor(train[,depVar], levels=factorLabels), 
                                                         pred, primaryColor, secondaryColor)
  accPresRecallPlots <- customPlots(factor(train[,depVar], levels=factorLabels),pred,factorLabels, primaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill=ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  reactDataComponents$gamPrecPlot <- accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$gamAccPlot <- accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$gamRecallPlot <- accPresRecallPlots[[3]] + plotTheme
  reactDataComponents$gamFitError <- ""
  ### validation
  predValid <- gamPredictProb(modelSubmitTrain$model, test[,xSelected,drop=F], "response")
  gamAnalysis <- allAnalysisOnPreds(factor(test[,depVar],levels = factorLabels), predValid, modelColours$GAM, 
                                    factorLabels, primaryColor, secondaryColor)
  modelSubmit <- list(
    model = modelSubmitTrain$model,
    predProb = predValid,
    label = factor(test[,depVar],levels = factorLabels),
    rocLinePlot = gamAnalysis[[6]],
    auc = gamAnalysis[[2]]
  )
  remove(formula, pred, summ1, accPresRecallPlots, predValid, gamAnalysis)
  outputtbl <-list(ms=modelSubmit, msTrain=modelSubmitTrain,rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}

# Set the threshold for predicting the test data
#' @name gamsetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using GAM
#' @param mst list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param ms list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

gamsetThreshold <- function(mst,ms,factorLabels, xSelected, thresh,  positiveClass)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$GAM <- mst
  modelSubmit$GAM <- ms
  
  reactDataComponents <- list()
  modelSubmitTrain$GAM$pred = predictClass(modelSubmitTrain$GAM$predProb,factorLabels, as.numeric(thresh))
  reactDataComponents$gamSummary = caret::confusionMatrix(modelSubmitTrain$GAM$pred, modelSubmitTrain$GAM$label,
                                                          positive = factorLabels[2])
  modelSubmitTrain$GAM$confusionMatrix = reactDataComponents$gamSummary
  modelSubmitTrain$GAM$featuresSelected = xSelected
  modelSubmitTrain$GAM$threshold = as.numeric(thresh)
  modelSubmitTrain$GAM$factorLabels = factorLabels
  modelSubmitTrain$GAM$predFunc = function(model, test, factorLabels, thresh=0.5){
    pred <- gamPredictProb(model,test, "response")
    p = cut(pred, breaks=c(0,as.numeric(thresh),1), labels=factorLabels,include.lowest = T)
    return(p)
  } 
  ### validation
  modelSubmit$GAM$pred <- predictClass(modelSubmit$GAM$predProb,factorLabels, as.numeric(thresh))
  modelSubmit$GAM$confusionMatrix = caret::confusionMatrix(modelSubmit$GAM$pred, modelSubmit$GAM$label,
                                                           positive = factorLabels[2])
  modelSubmit$GAM$featuresSelected = xSelected
  modelSubmit$GAM$threshold = as.numeric(thresh)
  modelSubmit$GAM$factorLabels = factorLabels
  modelSubmit$GAM$predFunc = function(model, test, factorLabels, thresh=0.5){
    pred<-gamPredictProb(model,test,"response")
    p = cut(pred, breaks=c(0,as.numeric(thresh),1), labels=factorLabels,include.lowest = T)
    return(p)
  } 
  reactDataComponents$gamSubError <- ""
  outputtbl <-list(ms=modelSubmit, msTrain=modelSubmitTrain, rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}

# Fit a K-NN Model
#' @name knnfit
#' @title Fit a K-NN Model
#' @details Fit a K-NN Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param k k number of nearest neighbour
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export


knnFit <- function(train,test,factorLabels,
                   xSelected,
                   depVar,
                   cvK = 3,k=3,
                   modelColours,
                   primaryColor,
                   secondaryColor)
{
  train[,depVar] <- factor(train[,depVar],levels = factorLabels)
  reactDataComponents <- list()
  
  if (as.numeric(k) > 1)
  {
    reactDataComponents$knnPlots <- knnPlots(train, train, xSelected, depVar, as.numeric(k), 
                                             factorLabels, primaryColor, secondaryColor)
  }else
  {
    reactDataComponents$knnPlots <- NULL
  }
  
  caretOut <- knnTrain(train, xSelected, depVar, cvK, as.numeric(k), factorLabels)
  model <- caretOut$finalModel
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'k-NN', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  reactDataComponents$knncvPlot <- cvPlot
  reactDataComponents$knncvCM <- cvCM
  
  pred <- knnPredictProb(model, train[,xSelected,drop=F], factorLabels)
  analysis <- allAnalysisOnPreds(train[,depVar], pred, modelColours$KNN, factorLabels, primaryColor, secondaryColor)
  
  reactDataComponents$knnOptTh = round(analysis[[4]], 2)
  modelSubmitTrain <-  list(
    model = model,
    predProb = pred,
    label = train[,depVar],
    rocLinePlot = analysis[[6]],
    auc = analysis[[2]]
  )
  
  reactDataComponents$knnFancyProbDenPlot <- densityFunc(train[,depVar], pred, primaryColor, secondaryColor)
  accPresRecallPlots <- customPlots(train[,depVar],pred,factorLabels,primaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill=ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  reactDataComponents$knnPrecPlot <- accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$knnAccPlot <- accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$knnRecallPlot <- accPresRecallPlots[[3]] + plotTheme
  reactDataComponents$knnROCPlot <- analysis[[1]]
  reactDataComponents$knnSSTPlot <- analysis[[3]]
  
  Metrics = rbind("Area Under Curve","Optimal threshold")
  Values = as.numeric(rbind(analysis[[2]],analysis[[4]]))
  reactDataComponents$knnAOCTable <- data.frame(Metrics, Values)
  reactDataComponents$knnOptTh <- round(analysis[[4]],2)
  # validation
  predictionsValid = knnPredictProb(model, test[,xSelected,drop=F], factorLabels)
  analysisValid = allAnalysisOnPreds(factor(test[,depVar], levels=factorLabels), predictionsValid, modelColours$KNN, 
                                     factorLabels, primaryColor, secondaryColor)
  
  modelSubmit <-  list(
    model = model,
    predProb = predictionsValid,
    label = factor(test[,depVar], levels=factorLabels),
    rocLinePlot = analysisValid[[6]],
    auc = analysisValid[[2]]
  )
  reactDataComponents$knnFitError <- ""
  remove(model, analysis, pred, accPresRecallPlots, analysisValid, predictionsValid)
  outputtbl <- list(ms=modelSubmit, msTrain=modelSubmitTrain, rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}

# Set the threshold for predicting the test data
#' @name knnsetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using K-NN
#' @param modelSubmitTrain list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @param y the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

knnsetThreshold <- function(mst,ms,factorLabels, xSelected, thresh,  y)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$KNN <- mst
  modelSubmit$KNN <- ms
  reactDataComponents <- list()
  modelSubmitTrain$KNN$pred <- predictClass(modelSubmitTrain$KNN$predProb, factorLabels, as.numeric(thresh))
  reactDataComponents$knnSummary <- caret::confusionMatrix(modelSubmitTrain$KNN$pred, 
                                                           modelSubmitTrain$KNN$label, 
                                                           positive = factorLabels[2])
  modelSubmitTrain$KNN$confusionMatrix = reactDataComponents$knnSummary
  modelSubmitTrain$KNN$featuresSelected = xSelected
  modelSubmitTrain$KNN$factorLabels = factorLabels
  modelSubmitTrain$KNN$threshold = as.numeric(thresh)
  modelSubmitTrain$KNN$predFunc = function(model, test, factorLabels, thresh){
    predictions <- stats::predict(model, test, type = 'prob')[,2]
    p = cut(predictions, breaks=c(0,thresh,1),labels=factorLabels, include.lowest=T)
    return(p)
  }
  ### validation
  modelSubmit$KNN$pred <- predictClass(modelSubmit$KNN$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$KNN$confusionMatrix = caret::confusionMatrix(modelSubmit$KNN$pred, modelSubmit$KNN$label, 
                                                           positive = factorLabels[2])
  modelSubmit$KNN$featuresSelected = xSelected
  modelSubmit$KNN$factorLabels = factorLabels
  modelSubmit$KNN$threshold = as.numeric(thresh)
  modelSubmit$KNN$predFunc = function(model, test, factorLabels, thresh){
    predictions <- stats::predict(model, test, type = 'prob')[,2]
    p = cut(predictions, breaks=c(0,thresh,1),labels=factorLabels, include.lowest=T)
    return(p)
  }
  outputtbl <-list(ms=modelSubmit, msTrain=modelSubmitTrain, rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}

# Fit a LVQ Model
#' @name lvqfit
#' @title Fit a LVQ Model
#' @details Fit a LVQ Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param k k value
#' @param size number of codebooks
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export

lvqFit <- function(train,test,factorLabels,
                   xSelected,
                   depVar,
                   cvK = 3,k=3, size=10,
                   modelColours,
                   primaryColor,
                   secondaryColor)
{
  reactDataComponents <- list()
  lvqModel <- NULL
  predictions <- NULL
  
  train[,y] <- factor(train[,y], levels = factorLabels)
  
  # Genrating error for each k plot
  if(as.numeric(k) > 1)
  {
    lvqPlotData = c()
    ### Apologies for the for loop
    for(i in 1:as.numeric(k))
    {
      cvk <- ifelse(i == k, cvK, 1) # because we dont want to cross validaiton for all iterations of k. pointless.
      caretOut <- lvqTrain(train, xSelected, y, cvk, factorLabels, k=i, size=size)
      predictions <- lvqPredictProb(caretOut$finalModel, train[,xSelected,drop=F], factorLabels)
      cm = caret::confusionMatrix(predictions[,"max.class"], train[,y], positive = factorLabels[2])
      lvqPlotData <- rbind(lvqPlotData, c(i, cm$overall[[1]]))
    }
    lvqModel <- caretOut$finalModel
    ### the last lvqModel created inside the loop is the final model retained
    lvqPlotData = data.frame(lvqPlotData)
    colnames(lvqPlotData) = c("K","Accuracy")
    lvqPlotData[,2] = as.numeric(lvqPlotData[,2])
    reactDataComponents$lvqplot = ggplot2::ggplot(data=lvqPlotData, ggplot2::aes(x=K, y=Accuracy, group=1)) + 
      ggplot2::geom_line(color=primaryColor) + ggplot2::geom_point() + 
      ggplot2::coord_cartesian(xlim = c(1:nrow(lvqPlotData)), ylim = c(0,1)) +
      ggplot2::labs(title= "Accuracy Vs K") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
      ggplot2::scale_x_continuous(breaks = seq(1,as.numeric(k),1))
    remove(lvqPlotData)
  }else
  {
    reactDataComponents$lvqplot = NULL 
    caretOut <- lvqTrain(train, xSelected, y, cvK, factorLabels, k=as.numeric(k), size=size)
    lvqModel <- caretOut$finalModel
    predictions = lvqPredictProb(lvqModel, train[,xSelected,drop=F], factorLabels) 
  }
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'LVQ', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  
  reactDataComponents$lvqcvPlot <- cvPlot
  reactDataComponents$lvqcvCM <- cvCM
  
  analysis = allAnalysisOnPreds(train[,y], predictions[,2], 
                                modelColours$LVQ, factorLabels, primaryColor, secondaryColor)
  reactDataComponents$lvqOptTh = round(analysis[[4]], 2)
  modelSubmitTrain <-  list(
    model = lvqModel,
    predProb = predictions[,2],
    label = train[,y],
    rocLinePlot = analysis[[6]],
    auc = analysis[[2]]
  )
  ### Pretty plots
  reactDataComponents$lvqFancyProbDenPlot <- densityFunc(train[,y], predictions[,2], primaryColor, secondaryColor)
  accPresRecallPlots <- customPlots(train[,y], predictions[,2], factorLabels, primaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  reactDataComponents$lvqPrecPlot <- accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$lvqAccPlot <- accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$lvqRecallPlot <- accPresRecallPlots[[3]] + plotTheme
  reactDataComponents$lvqROCPlot <- analysis[[1]]
  reactDataComponents$lvqSSTPlot <- analysis[[3]]
  Metrics = rbind("Area Under Curve","Optimal threshold")
  Values = as.numeric(rbind(analysis[[2]],analysis[[4]]))
  reactDataComponents$lvqAOCTable <- data.frame(Metrics, Values)
  reactDataComponents$lvqOptTh <- round(analysis[[4]],2)
  ### validation
  predictionsValid = lvqPredictProb(lvqModel, test[,xSelected,drop=F], factorLabels)
  analysisValid = allAnalysisOnPreds(factor(test[,y], levels=factorLabels), predictionsValid[,2], 
                                     modelColours$LVQ, factorLabels, primaryColor, secondaryColor)
  modelSubmit <-  list(
    model = list(model=lvqModel, k=as.numeric(k)),
    predProb = predictionsValid[,2],
    label = factor(test[,y], levels=factorLabels),
    rocLinePlot = analysisValid[[6]],
    auc = analysisValid[[2]]
  )
  reactDataComponents$lvqFitError <- ""
  remove(lvqModel, predictions, analysis, accPresRecallPlots, predictionsValid, analysisValid)
  outputtbl<-list(ms=modelSubmit, msTrain=modelSubmitTrain, rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}

# Set the threshold for predicting the test data
#' @name lvqsetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using LVQ
#' @param modelSubmitTrain list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @param y the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

lvqsetThreshold <- function(mst,ms,factorLabels, xSelected, thresh,  y)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$LVQ <- mst
  modelSubmit$LVQ <- ms
  reactDataComponents <- list()
  modelSubmitTrain$LVQ$pred = predictClass(modelSubmitTrain$LVQ$predProb, factorLabels, as.numeric(thresh))
  reactDataComponents$lvqSummary <- caret::confusionMatrix(modelSubmitTrain$LVQ$pred, 
                                                           factor(modelSubmitTrain$LVQ$label, levels=factorLabels), 
                                                           positive = factorLabels[2])
  modelSubmitTrain$LVQ$confusionMatrix = reactDataComponents$lvqSummary
  modelSubmitTrain$LVQ$featuresSelected = xSelected
  modelSubmitTrain$LVQ$factorLabels = factorLabels
  modelSubmitTrain$LVQ$threshold = as.numeric(thresh)
  modelSubmitTrain$LVQ$predFunc = function(model, test, factorLabels, thresh){
    lvqPredict <- function(codebook, testX, k=1)
    {
      testX = as.matrix(testX)
      tempFunc <- function(a, codebook, k)
      {
        d = unlist(apply(codebook$x, 1, function(x,y){return(dist(rbind(x,y), method='euclidean')[[1]])}, y=a))
        index = sort(d, index.return=T)$ix
        t = table(codebook$cl[index[1:k]])
        if(t[[1]]>t[[2]])
        {
          finali = 1
        }else
        {
          finali = 2
        }
        return(c(sort(names(t))[finali], t[finali]/sum(t)))
      }
      predictions = t(unname(unlist(apply(testX, 1, tempFunc, codebook=codebook, k=k))))
      predictions = as.data.frame(predictions, stringsAsFactors = F)
      colnames(predictions) <- c("class", "prob")
      predictions[,2] = as.numeric(predictions[,2]) * (as.numeric(as.factor(predictions[,1]))-1)
      predictions[,1] = as.factor(predictions[,1])
      return(predictions)
    } #end of lvqPredict
    pred = lvqPredict(model$model, test, model$k)[,'prob']
    p = cut(pred, breaks=c(0,thresh,1),labels=factorLabels, include.lowest=T)
    return(p)
  } #end of predFunc
  ### validation
  modelSubmit$LVQ$pred = predictClass(modelSubmit$LVQ$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$LVQ$confusionMatrix = caret::confusionMatrix(modelSubmit$LVQ$pred, 
                                                           factor(modelSubmit$LVQ$label, levels=factorLabels), 
                                                           positive = factorLabels[2])
  modelSubmit$LVQ$featuresSelected = xSelected
  modelSubmit$LVQ$factorLabels = factorLabels
  modelSubmit$LVQ$threshold = as.numeric(thresh)
  modelSubmit$LVQ$predFunc = function(model, test, factorLabels, thresh){
    lvqPredict <- function(codebook, testX, k=1)
    {
      testX = as.matrix(testX)
      tempFunc <- function(a, codebook, k)
      {
        d = unlist(apply(codebook$x, 1, function(x,y){return(dist(rbind(x,y), method='euclidean')[[1]])}, y=a))
        index = sort(d, index.return=T)$ix
        t = table(codebook$cl[index[1:k]])
        if(t[[1]]>t[[2]])
        {
          finali = 1
        }else
        {
          finali = 2
        }
        return(c(sort(names(t))[finali], t[finali]/sum(t)))
      }
      predictions = t(unname(unlist(apply(testX, 1, tempFunc, codebook=codebook, k=k))))
      predictions = as.data.frame(predictions, stringsAsFactors = F)
      colnames(predictions) <- c("class", "prob")
      predictions[,2] = as.numeric(predictions[,2]) * (as.numeric(as.factor(predictions[,1]))-1)
      predictions[,1] = as.factor(predictions[,1])
      return(predictions)
    } #end of lvqPredict
    pred = lvqPredict(model$model, test, model$k)[,'prob']
    p = cut(pred, breaks=c(0,thresh,1),labels=factorLabels, include.lowest=T)
    return(p)
  } #end of predFunc
  reactDataComponents$lvqSubError <- ""
  outputtbl <- list(ms=modelSubmit, msTrain=modelSubmitTrain, rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}
# Fit a SVM Model
#' @name svmfit
#' @title Fit a SVM Model
#' @details Fit a SVM Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param svmKernel the type of kernel
#' @param svmGamma gamma value
#' @param svmDegree degree value
#' @param svmCoef0 coefficient value
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export

svmFit <- function(train,test,factorLabels,
                   xSelected,
                   y,
                   cvK = 3,svmKernel="Linear", svmGamma=0.0312,
                   svmDegree=3, svmCoef0=0,
                   modelColours,
                   primaryColor,
                   secondaryColor)
{
  reactDataComponents<-list()
  train[,y] <- factor(train[,y], levels = factorLabels)
  
  # Building model with CV
  caretOut <- svmTrainWithCV(train[,c(xSelected,y),drop=F], y, cvK, 
                             svmKernel, svmGamma, svmDegree, svmCoef0)
  svmModel <- caretOut$finalModel
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'SVM', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  
  reactDataComponents$svmcvPlot <- cvPlot
  reactDataComponents$svmcvCM <- cvCM
  
  ## Analysis
  svmPreds <- svmPredictProb(svmModel, train[,xSelected,drop=F], factorLabels)
  svmOutput <- list(analysis = allAnalysisOnPreds(factor(train[,y],levels = factorLabels), svmPreds[,2], 
                                                  modelColours$SVM,factorLabels, primaryColor, secondaryColor), model = svmModel)
  analysis = svmOutput$analysis
  reactDataComponents$svmOptTh <- round(analysis[[4]],2)
  reactDataComponents$svmROCPlot = analysis[[1]]
  reactDataComponents$svmSSTPlot = analysis[[3]]
  Metric = rbind("Area Under Curve","Optimal threshold")
  Values = rbind(analysis[[2]], analysis[[4]][[1]])
  reactDataComponents$svmAOCTable <- data.frame(Metric, Values)
  modelForDisplay <- svmOutput$model[[1]]
  modelForDisplay$call <- "svm.default"
  reactDataComponents$svmModelSummary<-summary(modelForDisplay)
  ### Pretty plots
  reactDataComponents$svmFancyProbDenPlot <- densityFunc(factor(train[,y],levels = factorLabels), analysis[[5]], 
                                                         primaryColor, secondaryColor)
  accPresRecallPlots <- customPlots(factor(train[,y],levels = factorLabels),analysis[[5]],factorLabels, primaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  reactDataComponents$svmPrecPlot <- accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$svmAccPlot <- accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$svmRecallPlot <- accPresRecallPlots[[3]] + plotTheme
  modelSubmitTrain <-  list(
    model = svmOutput$model,
    predProb = analysis[[5]],
    label = factor(train[,y],levels = factorLabels),
    rocLinePlot = analysis[[6]],
    auc = analysis[[2]]
  )
  ### validation
  predValid = svmPredictProb(svmOutput$model,test[,xSelected,drop=F],factorLabels)
  svmAnalysis <- allAnalysisOnPreds(factor(test[,y], levels=factorLabels), predValid[,2], 
                                    modelColours$SVM, factorLabels, primaryColor, secondaryColor)
  modelSubmit <-  list(
    model = svmOutput$model,
    predProb = predValid[,2],
    label = factor(test[,y], levels=factorLabels),
    rocLinePlot = svmAnalysis[[6]],
    auc = svmAnalysis[[2]]
  )
  reactDataComponents$svmFitError <- ""
  remove(svmModel, svmPreds, svmOutput, analysis, accPresRecallPlots, predValid, svmAnalysis)
  outputtbl <- list(msTrain=modelSubmitTrain, ms=modelSubmit, rd=reactDataComponents,factorLabels=factorLabels,test=test)
  return(list_to_df(outputtbl))
}
# Set the threshold for predicting the test data
#' @name svmsetThreshold
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using SVM
#' @param mst list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param ms list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @param y the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

svmsetThreshold <-function(mst,ms,factorLabels,test, xSelected, thresh,  y)
{
  modelSubmitTrain <- list()
  modelSubmit <- list()
  modelSubmitTrain$SVM <- mst
  modelSubmit$SVM <- ms
  reactDataComponents <- list()
  modelSubmitTrain$SVM$pred = predictClass(modelSubmitTrain$SVM$predProb, factorLabels, as.numeric(thresh))
  reactDataComponents$svmSummary <- caret::confusionMatrix(modelSubmitTrain$SVM$pred, modelSubmitTrain$SVM$label, 
                                                           positive = positiveClass)
  reactDataComponents$commonCM <- reactDataComponents$svmSummary
  modelSubmitTrain$SVM$confusionMatrix = reactDataComponents$svmSummary
  modelSubmitTrain$SVM$featuresSelected = xSelected
  modelSubmitTrain$SVM$threshold = thresh
  modelSubmitTrain$SVM$factorLabels = factorLabels
  modelSubmitTrain$SVM$predFunc = function(model, test, factorLabels, thresh=0.5){
    p = c()
    ncore = length(model)
    p = lapply(c(1:ncore),function(x){
      pred = stats::predict(model[[x]], test, probability = TRUE)
      pred = attr(pred, 'probabilities')
      pred = pred[,factorLabels]
      return(pred[,2])
    })
    p = rowSums(cbind.data.frame(p)) / ncore
    p = cut(p, breaks=c(0,as.numeric(thresh),1), labels=factorLabels, include.lowest = T)
    return(p)
  }
  ### validation
  modelSubmit$SVM$pred <- predictClass(modelSubmit$SVM$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$SVM$confusionMatrix = caret::confusionMatrix(modelSubmit$SVM$pred, modelSubmit$SVM$label, 
                                                           positive = positiveClass)
  modelSubmit$SVM$featuresSelected = xSelected
  modelSubmit$SVM$threshold = thresh
  modelSubmit$SVM$factorLabels = factorLabels
  modelSubmit$SVM$predFunc = function(model, test, factorLabels, thresh=0.5){
    p = c()
    ncore = length(model)
    p = lapply(c(1:ncore),function(x){
      pred = stats::predict(model[[x]], test, probability = TRUE)
      pred = attr(pred, 'probabilities')
      pred = pred[,factorLabels]
      return(pred[,2])
    })
    p = rowSums(cbind.data.frame(p)) / ncore
    p = cut(p, breaks=c(0,as.numeric(thresh),1), labels=factorLabels,include.lowest = T)
    return(p)
  }
  outputtbl <-list(msTrain=modelSubmitTrain, ms=modelSubmit, rd=reactDataComponents,factorLabels=factorLabels)
  return(list_to_df(outputtbl))
}
# Fit a Decision tree Model
#' @name pdtfit
#' @title Fit a Decision tree Model
#' @details Fit a Decision tree Model for the given data and cross validation value
#' @param train training data
#' @param test test data
#' @param xSelected important varibales
#' @param depVar dependent variable
#' @param factorLabels the levels of factors present in dependent variable
#' @param cvK cross validation  K value
#' @param modelColours model color
#' @param primaryColor primary color for displaying the graph
#' @param secondaryColor secondary color for displaying the graph
#' @return Returns the list of graphs and model fitted
#' @family Package classification Utilites functions
#' @export


pdtFit <- function(train, test, xSelected, y, factorLabels, cvK, modelColours, primaryColor, secondaryColor)
{
  
  reactDataComponents<-list()
  train[,y] <- factor(train[,y], levels=factorLabels)
  
  caretOut <-pdtTrain(train, xSelected, y, cvK)
  PDTModel <- caretOut$finalModel
  
  # generate cross validation plots
  if(cvK > 1) {
    cvPlot <- crossValidationPlot(caretOut$resample, 'Accuracy', 'Decision Tree', primaryColor, secondaryColor)
    cvCM <- caretOut$resampledCM[,c('Resample', 'cell1', 'cell2', 'cell3', 'cell4')]
    colnames(cvCM) <- c('Fold', 'TP', 'FP', 'FN', 'TN')
  } else {
    cvPlot <- cvCM <- NULL
  }
  
  reactDataComponents$pdtcvPlot <- cvPlot
  reactDataComponents$pdtcvCM <- cvCM
  
  modelPredictions <- stats::predict(PDTModel, train[,xSelected,drop=F], type="prob")
  pred = t(rbind.data.frame(lapply(modelPredictions, function(x) return(cbind(x[1],x[2])))))
  colnames(pred) <- factorLabels   
  rownames(pred) <- c(1:length(pred[,1]))
  
  reactDataComponents$PDTModel <- PDTModel
  pdtAnalysis <- allAnalysisOnPreds(train[,y],pred[,2],color=modelColours$PDT,factorLabels, 
                                    primaryColor, secondaryColor)
  pred <- pdtAnalysis[[5]]
  
  reactDataComponents$pdtOptTh <- round(pdtAnalysis[[4]],2)
  reactDataComponents$pdtROCPlot = pdtAnalysis[[1]]
  reactDataComponents$pdtSSTPlot = pdtAnalysis[[3]]
  Metric = rbind("Area Under Curve","Optimal threshold")
  Values = rbind(pdtAnalysis[[2]], pdtAnalysis[[4]][[1]])
  reactDataComponents$pdtAOCTable = data.frame(Metric, Values)
  reactDataComponents$pdtModelSummary <- PDTModel
  modelSubmitTrain <-  list(
    model = PDTModel,
    predProb = pred,
    label = train[,y],
    rocLinePlot = pdtAnalysis[[6]],
    auc = pdtAnalysis[[2]]
  )
  ### Pretty plots
  reactDataComponents$pdtFancyProbDenPlot <- densityFunc(factor(train[,y],levels=factorLabels),
                                                         pred, primaryColor, secondaryColor)
  accPresRecallPlots <- customPlots(factor(train[,y],levels = factorLabels),pred,factorLabels, primaryColor)
  plotTheme <- ggplot2::theme(legend.position=c(0.8,0.8),
                              legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
                              legend.direction = "vertical",
                              plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                              axis.title = ggplot2::element_text(size=14))
  reactDataComponents$pdtPrecPlot <- accPresRecallPlots[[2]] + plotTheme
  reactDataComponents$pdtAccPlot <- accPresRecallPlots[[1]] + plotTheme
  reactDataComponents$pdtRecallPlot <- accPresRecallPlots[[3]] + plotTheme
  ### validation
  predValid <- stats::predict(PDTModel, test[,xSelected,drop=F], type="prob")
  predValid = t(rbind.data.frame(lapply(predValid, function(x) return(cbind(x[1],x[2])))))
  colnames(predValid) <- factorLabels 
  rownames(predValid) <- c(1:length(predValid[,1]))
  analysisValid <- allAnalysisOnPreds(factor(test[,y], levels=factorLabels), predValid[,2], modelColours$PDT,
                                      factorLabels, primaryColor, secondaryColor)
  modelSubmit <-  list(
    model = PDTModel,
    predProb = predValid[,2],
    label = factor(test[,y], levels=factorLabels),
    rocLinePlot = analysisValid[[6]],
    auc = analysisValid[[2]]
  )
  remove(modelPredictions, pred, pdtAnalysis, accPresRecallPlots, predValid, analysisValid)
  outputtbl <-list(msTrain=modelSubmitTrain, ms=modelSubmit, rd=reactDataComponents)
  return(list_to_df(outputtbl))
}

# Set the threshold for predicting the test data
#' @name pdtSubmit
#' @title Set the threshold for predicting the test data 
#' @details Set the threshold for predicting the test data using Decision tree
#' @param modelSubmitTrain list of the model, prediction probability and labels
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param xSelected the list of independent variables selected
#' @param thresh the threshold for probability above which the dependent variable is classified as positive class
#' @param positiveClass the positive class of the dependent variable
#' @param y the dependent variable
#' @return Returns a list of model fitted, confusion matrix and other statistics of the model
#' @family Package classification Utilites functions
#' @export

pdtsetThreshold <- function(xSelected, y, factorLabels, thresh, modelSubmitTrain, modelSubmit,positiveClass)
{
  reactDataComponents = list()
  modelSubmitTrain$PDT$pred <- predictClass(modelSubmitTrain$PDT$predProb, factorLabels, as.numeric(thresh))
  reactDataComponents$commonCM <- caret::confusionMatrix(modelSubmitTrain$PDT$pred, modelSubmitTrain$PDT$label, 
                                                         positive = positiveClass)
  modelSubmitTrain$PDT$confusionMatrix = reactDataComponents$commonCM
  modelSubmitTrain$PDT$featuresSelected = xSelected
  modelSubmitTrain$PDT$threshold = as.numeric(thresh)
  modelSubmitTrain$PDT$factorLabels = factorLabels
  modelSubmitTrain$PDT$predFunc = function(model, test, factorLabels, thresh=0.5){
    modelPredictions <- stats::predict(model, test, type="prob")
    pred = t(rbind.data.frame(lapply(modelPredictions, function(x) return(cbind(x[1],x[2])))))
    colnames(pred) <- factorLabels
    rownames(pred) <- c(1:length(pred[,1]))
    p = cut(pred[,2], breaks=c(0,as.numeric(thresh),1), labels=factorLabels,include.lowest = T)
    return(p)
  }
  reactDataComponents$pdtSummary <- modelSubmitTrain$PDT$confusionMatrix 
  ### validation
  modelSubmit$PDT$pred <- predictClass(modelSubmit$PDT$predProb, factorLabels, as.numeric(thresh))
  modelSubmit$PDT$confusionMatrix = caret::confusionMatrix(modelSubmit$PDT$pred, modelSubmit$PDT$label, 
                                                           positive = positiveClass)
  modelSubmit$PDT$featuresSelected = xSelected
  modelSubmit$PDT$threshold = as.numeric(thresh)
  modelSubmit$PDT$factorLabels = factorLabels
  modelSubmit$PDT$predFunc = function(model, test, factorLabels, thresh=0.5, testCat=NULL, xSelected=NULL){
    if(!is.null(testCat))
    {
      test <- getCategoricalData(test, testCat, xSelected)$data
    }
    modelPredictions <- predict(model, test, type="prob")
    pred = t(rbind.data.frame(lapply(modelPredictions, function(x) return(cbind(x[1],x[2])))))
    colnames(pred) <- factorLabels
    rownames(pred) <- c(1:length(pred[,1]))
    p = cut(pred[,2], breaks=c(0,as.numeric(thresh),1), labels=factorLabels,include.lowest = T)
    return(p)
  }
  # modelSubmit$PDT$predFunc = function(model, test, factorLabels, thresh=0.5)
  # {
  # modelPredictions <- predict(model, test, type="prob")
  # pred = t(rbind.data.frame(lapply(modelPredictions, function(x) return(cbind(x[1],x[2])))))
  # colnames(pred) <- factorLabels
  # rownames(pred) <- c(1:length(pred[,1]))
  # p = cut(pred[,2], breaks=c(0,as.numeric(thresh),1), labels=factorLabels,include.lowest = T)
  # return(p)
  # }
  outputtbl <- list(msTrain=modelSubmitTrain, ms=modelSubmit, rd=reactDataComponents)
  return(list_to_df(outputtbl))
}

# Predict the test data
#' @name predictTestData
#' @title Classify the dependent variable in the test dataset
#' @details Classify the dependent variable in the test dataset
#' @param factorLabels the levels of factors present in dependent variable
#' @param modelSubmit list of the model, prediction probability for validation and labels
#' @param testdataset the test dataset
#' @param selectModel string having the model name 
#' @param selectImpVar the important variables to used for predicting
#' @param depVar the dependent variable
#' @param checkGroundTruth compare the prediction with the original classes
#' @return Returns the test data with dependent variable classified
#' @family Package classification Utilites functions
#' @export

predictTestData <-function(modelSubmit,factorLabels,
                           testdataset,
                           depVar,
                           dataset,
                           selectImpVar,
                           selectModel,
                           checkGroundTruth = TRUE) {
  
  miss <- handleMissingValues(testdataset, remove = T)
  #miss$mssg
  testdataset <- miss$data
  if (depVar %in% colnames(testdataset))
  {
    groundTruth <- as.factor(testdataset[, depVar])
  } else
  {
    groundTruth <- NULL
  }
  
  
  testdataset <-
    testdataset[, setdiff(colnames(testdataset), depVar), drop = F]
  meta <- metaFileLoad(varTypeFile = NULL, dataset)
  typeData <- meta$typeData
  
  meta <- useMetaFile(dataset, typeData)
  dataset <- meta$data
  typeData <- meta$typeData
  datasetCategorical <- meta$catCols
  datasetNumeric <- meta$numCols
  binaryCatNames <- meta$binCatCols
  datasetCat<-  datasetCategorical
  binaryCatData <- dataset[, datasetCategorical, drop = F]
  
  origColumns <-
    getCategoricalData(dataset, binaryCatData, selectImpVar)$x
  eval <- origColumns %in% colnames(testdataset)
  if (!all(eval)) {
    stop(paste0(
      "The columns ",
      paste0(origColumns[which(eval == FALSE)], collapse = ", "),
      " are missing from the uploaded test data set."
    ))
  }
  # Subsetting uploaded data set to work with original columns selected for model building
  testdataset <- testdataset[, origColumns, drop = F]
  
  #####################
  ## Dummify columns ##
  #####################
  # meta <- useMetaFile(testdataset, reactData$typeData[which(reactData$typeData$Column %in% colnames(testdataset)),,drop=F])
  # testdataset <- meta$data
  colsToDummy <-
    intersect(colnames(binaryCatData), colnames(testdataset))
  loadedCatData <- NULL
  
  if (length(colsToDummy) > 0)
  {
    dummifiedCatData <- lapply(colsToDummy, function(x) {
      newLevels <- levels(testdataset[, x])
      origLevels <- levels(binaryCatData[, x])
      dummyDF <- DummyVarCreation(testdataset[, x, drop = F],
                                  referenceLevel = typeData$Reference.Level[which(typeData$Column == x)])
      
      # Making sure the levels of all categorical columns in the training data is the same as test data
      # If some levels are missing, create a new column with all 0s
      if (length(origLevels) != length(newLevels)) {
        zeroLevels <- setdiff(origLevels, newLevels)
        zeroDF <-
          as.data.frame(matrix(
            as.integer(rep(
              0, length(zeroLevels) * nrow(dummyDF)
            )),
            nrow = nrow(dummyDF),
            ncol = length(zeroLevels)
          ))
        colnames(zeroDF) <- paste0(x, '_', zeroLevels)
        dummyDF <- cbind(dummyDF, zeroDF)
      }
      return(dummyDF)
    })
    dummifiedCatData <- as.data.frame(dummifiedCatData)
    
    # Making sure the levels of all categorical columns in the training data is the same as test data
    # Setting the levels to match the factor variables of the training data
    loadedCatData <- testdataset[, colsToDummy, drop = F]
    for (x in colsToDummy) {
      loadedCatData[, x] <-
        factor(loadedCatData[, x], levels = levels(binaryCatData[, x]))
    }
    testdataset <-
      cbind.data.frame(testdataset[, setdiff(colnames(testdataset), colsToDummy), drop = F], dummifiedCatData)
  }
  testdataset <- testdataset[, selectImpVar, drop = F]
  modelSelected <- selectModel
  workingModel <- modelSubmit[[modelSelected]]
  
  pred <-predictLoadedData(
    loadedData = testdataset,
    workingModel,
    modelSelected,
    checkGroundTruth,
    groundTruth,
    factorLabels ,
    binaryCatData[, datasetCat, drop = FALSE],
    depVar,
    loadedCatData,
    selectImpVar
  )
  outputtbl<- pred
  return(list_to_df(outputtbl))  
  
}

########################HIDDEN / HELPER FUNCTIONS######################################
handleMissingValues <- function(data, remove = T, y = NULL)
{
  #print(head(data))
  missN <- nrow(data[rowSums(is.na(data)) > 0, ])
  if (missN > 0)
  {
    if (!remove)
      missValueMsg <- paste0(
        "WARNING: There are ",
        missN,
        " row(s) with missing values in the data set. ",
        "Because of this, there is
        a possibility that models may misbehave. Please use the 'Data Wrangling Brick'
        to create your analytical data set."
      )
    else
      missValueMsg <- paste0(
        "NOTE: There are ",
        missN,
        " row(s) with missing values in the data set, which have been removed."
      )
  } else
  {
    missValueMsg <- ""
  }
  if (remove)
    data <- na.omit(data)
  return(list(
    data = data,
    mssg = missValueMsg,
    missTable = NULL
  ))
}
DummyVarCreation <-
  function(columnToConvert,
           outPath = getwd(),
           fileName = paste("DummyVariableLog_", deparse(substitute(columnToConvert)), sep = ""),
           referenceLevel = NA,
           cutoffValue = 20) {
    # Check if required s are installed
    # if (is.element("dummies", installed.s()[, 1]) == TRUE) {
    #   library(dummies)
    # } else {
    #   install.packages("dummies", dependencies = T, repos = "http://cloud.r-project.org/")
    #   library(dummies)
    # }
    
    # Checking data Quality
    #create empty vector
    levelDropVec <- c()
    
    if (length(columnToConvert) > 0) {
      #Creating dummy columns to remove
      dummyColsToRemove <- c()
      
      if (is.na(referenceLevel) ||
          is.null(referenceLevel) || referenceLevel == "") {
        freqDf <- data.frame(table(columnToConvert))
        minFreqVal <- freqDf[which.min(freqDf[, 2]), 1]
        minFreqVal <- as.character(minFreqVal)
        referenceLevel <- minFreqVal
      }
      columnName <- names(columnToConvert)
      columnToConvert[[1]] <- as.character(columnToConvert[[1]])
      names(columnToConvert) <- columnName
      dummyColsToRemove <- c(dummyColsToRemove, referenceLevel)
      levelDropVec <-
        c(levelDropVec, paste(colnames(columnToConvert), sep = "_", referenceLevel))
      
      #creation of dummy variables
      if (length(unique(columnToConvert[[1]])) > 2) {
        dummys <-
          dummies::dummy.data.frame(columnToConvert,
                                    names = colnames(columnToConvert),
                                    sep = "_")
        #Removal of columns
        dummys <- dummys[,!colnames(dummys) %in% levelDropVec]
      } else{
        # If its a binary column
        dummys <-
          as.data.frame(as.numeric(columnToConvert != referenceLevel))
        colnames(dummys) <- paste(colnames(columnToConvert),
                                  unique(columnToConvert)[unique(columnToConvert) != referenceLevel],
                                  sep = '_')
      }
      
    }
    return(dummys)
  }
getCategoricalData <- function(data, catData, xSelected) {
  # get list of dummy variables in the selected list
  
  includeCols <- sapply(colnames(catData), function(x) {
    !(x == substr(xSelected, 1, nchar(x)))
  })
  if (length(includeCols) > 0) {
    if (length(xSelected) > 1) {
      xSelected <- xSelected[apply(includeCols, 1, all)]
      catSelected <-
        colnames(catData)[apply(includeCols, 2, function(x)
          any(!x))]
    }
    else{
      xSelected <- xSelected[all(includeCols)]
      catSelected <- colnames(catData)[any(!includeCols)]
    }
    # This if condition is to handle the difference in data size after class balancing
    # This is needed for the Load-predict section
    if (nrow(data) != nrow(catData)) {
      dataset <- NULL
    } else {
      dataset <-
        cbind.data.frame(data[, xSelected, drop = FALSE], catData[, catSelected, drop = FALSE])
    }
  } else {
    dataset <- data[, xSelected, drop = FALSE]
    catSelected <- c()
  }
  return(list(data = dataset, x = c(xSelected, catSelected)))
}


metaFileLoad <- function(varTypeFile, dataset)
{
  alertNoMetaFile <- ""
  typeFileErrorText <- ""
  typeFileErrorTable <- ""
  typeColumnErrorText <- ""
  if (is.null(varTypeFile))
  {
    typeData <- data.frame("Column" = colnames(dataset))
    typeData$Column.Type <-
      unname(lapply(dataset, function(x) {
        CheckColumnType(x, 2)
      }))
    alertNoMetaFile <-
      "NOTE: For ease of handling integer and discrete numbers, any numeric and integer columns will
    be converted into factor if the number of unique values is less than 20. To prevent such conversions, upload a relevant
    meta file mentioning the variable types to be typecasted. The Data Wrangling brick can be used to generate such meta files."
  } else
  {
    alertNoMetaFile <- ""
    typeData <-
      read.csv(varTypeFile$datapath, stringsAsFactors = FALSE)
    typeData <-
      data.table::fread(
        varTypeFile$datapath,
        sep = ",",
        header = T,
        na.strings = c('', 'NA', NA),
        data.table = F,
        stringsAsFactors = FALSE
      )
    typeData <-
      typeData[which(typeData$Column %in% colnames(dataset)), ]
    if (any(!(
      typeData$Column.Type %in% c("numeric", "factor", "character")
    )))
    {
      typeFileErrorText <-
        "Error : The uploaded file should be of the format"
      typeFileErrorTable <-
        data.frame(
          "Column" = c(
            "SampleColumnName1",
            "SampleColumnName1",
            "SampleColumnName1"
          ),
          "Column.Type" = c("numeric", "factor", "character"),
          "Reference.Level" = c(NA, "Yes", "Male")
        )
    }
    if (!all(colnames(dataset) %in% typeData$Column))
    {
      typeColumnErrorText <-
        "Error : The variable type csv does not have entries for all columns of the dataset"
    }
  }#end of if varFileType exists
  return(
    list(
      typeData = typeData,
      colErr = typeColumnErrorText,
      typeErrorText = typeFileErrorText,
      typeErrorTable <-
        typeFileErrorTable,
      noMetaAlert = alertNoMetaFile
    )
  )
  }

CheckColumnType <- function(dataVector, cutoffValue) {
  #Check if the column type is "numeric" or "character" & decide type accordingly
  if (class(dataVector) == "integer" ||
      class(dataVector) == "numeric") {
    #Extract number of unique levels in the dataVector
    numUnique <- length(unique(dataVector))
    #Check for cut-off condition
    if (numUnique > cutoffValue) {
      columnType <- "numeric"
    } else {
      columnType <- "factor"
    }
  } else {
    columnType <- "character"
  }
  
  #Return the result
  return(columnType)
  
}
useMetaFile <- function(dataset, typeData)
{
  datasetNumeric <-
    as.character(typeData$Column[which(typeData$Column.Type == "numeric")])
  datasetNumericCategorical <-
    as.character(typeData$Column[which(typeData$Column.Type == "factor")])
  datasetStringCategorical <-
    as.character(typeData$Column[which(typeData$Column.Type == "character")])
  datasetCategorical <-
    union(datasetNumericCategorical, datasetStringCategorical)
  binaryCatNames <- datasetCategorical[lapply(datasetCategorical,
                                              function(i) {
                                                length(levels(as.factor(dataset[, i])))
                                              }) == 2]
  orderedNames <- colnames(dataset)
  if (length(datasetCategorical) > 0)
  {
    dataset[, datasetCategorical] <-
      cbind.data.frame(sapply(dataset[, datasetCategorical, drop = F],
                              function(x) {
                                as.factor(as.character(x))
                              }),
                       dataset[, datasetNumeric, drop =
                                 F], stringsAsFactors = T)
  }
  if (length(datasetNumeric) > 0)
  {
    dataset[, datasetNumeric] <-
      cbind.data.frame(sapply(dataset[, datasetNumeric, drop = F],
                              function(x) {
                                as.numeric(x)
                              }),
                       dataset[, datasetCategorical, drop =
                                 F], stringsAsFactors = T)
  }
  dataset <- dataset[, orderedNames, drop = F]
  if (is.null(typeData$Reference.Level))
  {
    typeData$Reference.Level <- unlist(lapply(colnames(dataset),
                                              function(x)
                                              {
                                                if (typeData[which(typeData[, "Column"] == x), "Column.Type"] == "factor" ||
                                                    typeData[which(typeData[, "Column"] ==
                                                                   x), "Column.Type"] == "character")
                                                {
                                                  names(sort(table(dataset[, x]), decreasing = T)[1])
                                                } else
                                                {
                                                  return(NA)
                                                }
                                              })) #end of unlist
  }#end of if typeData$Reference.Level is NULL
  return(
    list(
      data = dataset,
      typeData = typeData,
      numCols = datasetNumeric,
      catCols = datasetCategorical,
      binCatCols = binaryCatNames
    )
  )
}




# typeData <- data.frame("Column"=colnames(dataset))
# typeData$Column.Type <- unname(lapply(dataset,function(x){CheckColumnType(x,2) }))


# reactData$binaryDummies <- colnames(dummifiedBinCatData)
#
#
#
#
#
# reactData$modLevels <- c(setdiff(levels(as.factor(reactData$binaryCatData[,input$predictedVariable])),
#                                  input$positiveClass),input$positiveClass)
lrTrain <- function(data, xSelected, depVar, k, factorLabels)
{
  data[, depVar] = factor(data[, depVar], levels = factorLabels)
  if (k > 1) {
    trControl <- caret::trainControl(method = 'cv', number = k)
  } else {
    trControl <- caret::trainControl(method = 'none')
  }
  caretOut <-
    caret::train(
      method = 'glm',
      family = stats::binomial(link = 'logit'),
      trControl = trControl,
      x = data[, xSelected, drop = FALSE],
      y = data[, depVar]
    )
  return(caretOut)
}

crossValidationPlot <-
  function(plotdata,
           metric,
           modelName,
           primaryColor,
           secondaryColor) {
    g <-
      ggplot2::ggplot(plotdata, ggplot2::aes(x = Resample, y = Accuracy)) +
      ggplot2::geom_bar(
        stat = 'identity',
        position = 'dodge',
        fill = primaryColor,
        color = 'white',
        alpha = 0.75
      ) +
      ggplot2::labs(
        y = metric,
        x = 'Cross Validation Folds',
        title = paste0(nrow(plotdata), '-fold Cross Validation for ', modelName)
      )
    
    if (metric == 'Accuracy') {
      g <- g + ggplot2::ylim(c(0, 1))
    }
    g
  }

predictClass <- function(predProb, factorLabels, threshold)
{
  return(cut(
    predProb,
    breaks = c(0, threshold, 1),
    labels = factorLabels,
    include.lowest = T
  ))
}

lrPredictProb <- function(model, data, factorLabels)
{
  predictions = stats::predict(model, data, type = "response")
  return(predictions)
}
allAnalysisOnPreds <-
  function(true,
           preds,
           color,
           factorLabels,
           primaryColor,
           secondaryColor)
  {
    rocObj = pROC::roc(true, preds)
    thresh = rocObj$thresholds[2:(length(rocObj$thresholds) - 1)]
    sense = rocObj$sensitivities[2:(length(rocObj$thresholds) - 1)]
    specs = rocObj$specificities[2:(length(rocObj$thresholds) - 1)]
    plotData = cbind(thresh, sense, specs)
    diff = sense - specs
    i = sum(diff > 0)
    if (rocObj$thresholds[1] == -Inf)
    {
      len = length(rocObj$thresholds)
      plotData = rbind(c(0, rocObj$sensitivities[1], rocObj$specificities[1]),
                       plotData)
      plotData = rbind(plotData,
                       c(1, rocObj$sensitivities[len], rocObj$specificities[len]))
      i = i + 1
    }
    plotData[which((-Inf) == plotData[, 1]), 1] = 0
    plotData[which(Inf == plotData[, 1]), 1] = 1
    if (i > 0 && nrow(plotData))
    {
      x1 = x3 = plotData[i, 1]
      x2 = x4 = plotData[i + 1, 1]
      y1 = plotData[i, 2]
      y2 = plotData[i + 1, 2]
      y3 = plotData[i, 3]
      y4 = plotData[i + 1, 3]
      x = unname(intersection(x1, y1, x2, y2, x3, y3, x4, y4)[1])
    } else
    {
      x = thresh[1]
    }
    specs = 1 - specs
    rocData = data.frame(specs, sense)
    rocData = rocData[rev(c(1:nrow(rocData))), ]
    rocData <- rbind(c(0, 0), rocData, c(1, 1))
    auc <- round(rocObj$auc[1], 4)
    linePlot <-
      ggplot2::geom_line(
        data = rocData,
        ggplot2::aes(x = specs, y = sense, color = color),
        color = color,
        linetype = "solid"
      )
    g1 = ggplot2::ggplot() + linePlot +
      ggplot2::ggtitle("ROC plot") +
      ggplot2::xlab("(1 - Specificity)") +
      ggplot2::ylab("Sensitivity") +
      ggplot2::geom_line(ggplot2::aes(x, y),
                         data = data.frame(x = rbind(0, 1), y = rbind(0, 1)),
                         linetype = "dotted") +
      ggplot2::annotate(
        "text",
        x = 0.6,
        y = 0.4,
        label = paste0("AUC \n ", auc),
        size = 7,
        color = "#337ab7"
      ) +
      ggplot2::theme(
        legend.position = c(0.8, 0.8),
        legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
        legend.direction = "vertical",
        plot.title = ggplot2::element_text(hjust = 0.5, size =
                                             20),
        axis.title = ggplot2::element_text(size = 14)
      )
    
    g2 = ggplot2::ggplot(data.frame(plotData), ggplot2::aes(thresh)) +
      ggplot2::geom_line(ggplot2::aes(y = sense, colour = "sensitivity")) +
      ggplot2::geom_line(ggplot2::aes(y = specs, colour = "specifity")) +
      ggplot2::xlab("Threshold") +
      ggplot2::ylab("Sensitivity-Specificity values") +
      ggplot2::ggtitle("Sensitivity-Specificity vs Threshold plot") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x)) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "")) +
      ggplot2::scale_color_manual(values = c(primaryColor, secondaryColor)) +
      ggplot2::theme(
        legend.position = c(0.8, 0.8),
        legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
        legend.direction = "vertical",
        plot.title = ggplot2::element_text(hjust = 0.5, size =
                                             20),
        axis.title = ggplot2::element_text(size = 14)
      )
    
    p = cut(
      preds,
      breaks = c(0, x, 1),
      labels = factorLabels,
      include.lowest = T
    )
    options(digits = 4)
    return(c(
      list(g1),
      list(rocObj$auc[1]),
      list(g2),
      list(x),
      list(preds),
      list(linePlot)
    ))
  }
intersection <- function(x1, y1, x2, y2, x3, y3, x4, y4)
{
  arguments <- list(x1, y1, x2, y2, x3, y3, x4, y4)
  m1 = (y2 - y1) / (x2 - x1)
  m2 = (y4 - y3) / (x4 - x3)
  if (any(sapply(arguments, is.null)))
  {
    return("One or more points in argument is NULL")
  }
  else if (any(sapply(arguments, is.na)))
  {
    return("One or more points in argument is NA")
  }
  else if (!all(sapply(arguments, is.finite)) &
           all(sapply(arguments, is.numeric)))
  {
    return("One or more points in argument is non finite")
  }
  else
  {
    if (m1 == m2)
    {
      return("Lines are parallel. No intersection.")
    }
  }
  c1 = y1 - m1 * x1
  c2 = y3 - m2 * x3
  x = (c2 - c1) / (m1 - m2)
  y = m1 * x + c1
  return(c(x, y))
}
densityFunc <-
  function (test.y,
            pred.prob,
            primaryColor,
            secondaryColor)
  {
    ## The dplyr package is loaded since the %>% operator cannot be scope resolved
    library(dplyr)
    factorLabels = levels(test.y)
    negClass = which(test.y == factorLabels[1])
    pred.prob[negClass] = 1 - pred.prob[negClass]
    ground.truth <- factor(test.y, levels = factorLabels)
    density_tbl <-
      data.table::data.table(`Prediction probability` = pred.prob,
                             `Ground Truth` = ground.truth)
    testLevels <- levels(as.factor(test.y))
    annotation <-
      paste0(
        "Training set size: ",
        ifelse(length(test.y) ==
                 5e+05, ">= 500,000", length(test.y)),
        "\n",
        testLevels[1],
        " : ",
        format(100 * sum(test.y == testLevels[1]) / length(test.y), digits = 3),
        "%\n",
        testLevels[2],
        " :   ",
        format(100 * sum(test.y == testLevels[2]) / length(test.y),
               digits = 3),
        "%"
      )
    plt <- ggplot2::ggplot(density_tbl) +
      ggplot2::geom_density(
        ggplot2::aes(x = `Prediction probability`, fill = `Ground Truth`),
        alpha = 0.75,
        color = ggplot2::alpha("white", 0),
        size = 1.5
      ) +
      ggplot2::scale_y_continuous(name = "Density", expand = c(0, 0)) +
      ggplot2::ggtitle("Probability density of prediction") +
      ggplot2::annotation_custom(grob = grid::textGrob(
        annotation,
        x = 0.05,
        y = 0.87,
        just = c("left", "top")
      )) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Classes")) +
      ggplot2::scale_fill_manual(values = c(secondaryColor, primaryColor))  # in the order of reactData$modLevels
    ggplot2::theme(
      legend.position = c(0.8, 0.8),
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha('white', 0)),
      legend.direction = "vertical",
      plot.title = ggplot2::element_text(hjust = 0.5, size =
                                           20),
      axis.title = ggplot2::element_text(size = 14)
    )
    return(plt)
  }



customPlots <- function(true, pred, factorLabels, primaryColor)
{
  # green_str <- "#78b45a"
  # blue_str <- "#51a7f9"
  green_str <- primaryColor
  blue_str <- "#000000"
  thseq <- seq(0.1, 0.9 , 0.01)
  metricTable <-
    data.frame(
      threshold = thseq,
      accuracy = rep(NA, length(thseq)),
      precision = rep(NA, length(thseq)),
      recall = rep(NA, length(thseq)),
      fscore = rep(NA, length(thseq))
    )
  for (th in thseq) {
    predLablesForTh <-
      cut(
        pred,
        breaks = c(0, th, 1),
        labels = factorLabels,
        include.lowest = T
      )
    cmForTh <-
      caret::confusionMatrix(predLablesForTh, true, positive = factorLabels[2])
    metricTable[metricTable[, "threshold"] == th, "accuracy"] <-
      round(cmForTh$overall[[1]] * 100, 2)
    metricTable[metricTable[, "threshold"] == th, "precision"] <-
      round(cmForTh$byClass['Precision'][[1]] * 100, 2)
    metricTable[metricTable[, "threshold"] == th, "recall"] <-
      round(cmForTh$byClass['Recall'][[1]] * 100, 2)
    metricTable[metricTable[, "threshold"] == th, "fscore"] <-
      round(cmForTh$byClass['F1'][[1]] * 100, 2)
  }
  # Creating a subset of the dataset for labels in the plot
  metricTableSubset <- metricTable[seq(1, nrow(metricTable), 10), ]
  
  accuracyPlotGG <-
    ggplot2::ggplot(
      metricTable,
      ggplot2::aes(x = threshold, y = accuracy),
      fill = green_str,
      alpha = "0.2"
    ) + ggplot2::geom_line(color = green_str, size = 1.5) +
    ggplot2::scale_x_continuous(name = "thresholded to positive class", breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_y_continuous(
      name = "Accuracy (%)",
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    ggplot2::geom_text(
      data = metricTableSubset,
      ggplot2::aes(
        x = threshold,
        y = accuracy,
        label = paste0(format(accuracy, digits = 4), "%")
      ),
      hjust = 0.3,
      vjust = -0.5,
      size = 4,
      color = I(blue_str)
    ) +
    ggplot2::ggtitle("Accuracy @ k")
  
  precisionPlotGG <-
    ggplot2::ggplot(
      metricTable,
      ggplot2::aes(x = threshold, y = precision),
      fill = green_str,
      alpha = "0.2"
    ) + ggplot2::geom_line(color = green_str, size = 1.5) +
    ggplot2::scale_x_continuous(name = "thresholded to positive class", breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_y_continuous(
      name = "Precision (%)",
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    ggplot2::geom_text(
      data = metricTableSubset,
      ggplot2::aes(
        x = threshold,
        y = precision,
        label = paste0(format(precision, digits = 4), "%")
      ),
      hjust = 0.3,
      vjust = -0.5,
      size = 4,
      color = I(blue_str)
    ) +
    ggplot2::ggtitle("Precision @ k")
  
  recallPlotGG <-
    ggplot2::ggplot(
      metricTable,
      ggplot2::aes(x = threshold, y = recall),
      fill = green_str,
      alpha = "0.2"
    ) + ggplot2::geom_line(color = green_str, size = 1.5) +
    ggplot2::scale_x_continuous(name = "thresholded to positive class", breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_y_continuous(
      name = "Recall (%)",
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    ggplot2::geom_text(
      data = metricTableSubset,
      ggplot2::aes(
        x = threshold,
        y = recall,
        label = paste0(format(recall, digits = 4), "%")
      ),
      hjust = 0.3,
      vjust = -0.5,
      size = 4,
      color = I(blue_str)
    ) +
    ggplot2::ggtitle("Recall @ k")
  
  accuracyPlotGG <-
    accuracyPlotGG + ggplot2::expand_limits(x = c(0, 1))
  precisionPlotGG <-
    precisionPlotGG + ggplot2::expand_limits(x = c(0, 1))
  recallPlotGG <- recallPlotGG + ggplot2::expand_limits(x = c(0, 1))
  
  return(list(accuracyPlotGG, precisionPlotGG, recallPlotGG))
}


aggregateData <-
  function(dataset,
           aggregateBy,
           aggregateColumn,
           aggregationMetric) {
    if (length(aggregateBy) > 1) {
      formula <-
        as.character(paste0(
          "cbind(",
          paste0(aggregateColumn, collapse = " , "),
          ") ~ ",
          paste0(aggregateBy, collapse = " + ")
        ))
    } else{
      formula <-
        as.character(paste0(aggregateColumn, " ~ ", paste0(aggregateBy, collapse = " + ")))
    }
    
    dataset <-
      stats::aggregate(as.formula(formula), dataset = dataset, noquote(aggregationMetric))
    dataset <- as.data.frame(dataset)
    return(dataset)
  }





plsTrain <- function(train_x, train_y, k, factorLabels, ncomp = 2)
{
  #The loading of these libraries are reuiqred to run partial least squares
  library(pls)
  library(caret)
  
  if (k > 1) {
    trControl <- caret::trainControl(method = 'cv', number = k)
  } else {
    trControl <- caret::trainControl(method = 'none')
  }
  library(caret)
  library(pls)
  # rmsepModel<-plsda(data, y, ncomp = ncomp, probMethod=probMethod, max.iter=iter)
  plsOut <-
    caret::train(
      x = train_x,
      y = train_y,
      method = 'pls',
      trControl = trControl,
      tuneGrid = data.frame(ncomp = 2)
    )
  return(plsOut)
}
plsRmsepPlot <- function(data, primaryColor)
{
  return(
    ggplot2::ggplot(data = data, ggplot2::aes(
      x = Components, y = RMSEP, group = 1
    )) +
      ggplot2::geom_line(color = primaryColor) +
      ggplot2::geom_point() + ggplot2::xlab("Number of Components") +
      ggplot2::ylab("RMSEP") + ggplot2::ggtitle("Components vs RMSEP plot") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  )
}
plsPredictProb <- function(model, data, factorLabels)
{
  pred <- stats::predict(model, data, type = "prob")
  pred <- pred[, , 1]
  pred <- pred[, factorLabels[2]]
  return(pred)
}

PLSloadingsPlot <- function(model)
{
  PLSModelLoading <-
    model$loadings[1:nrow(model$loadings), 1:ncol(model$loadings)]
  PLSModelNames <- rownames(PLSModelLoading)
  rownames(PLSModelLoading) <- seq(1, nrow(PLSModelLoading), 1)
  plsPlotData <- PLSModelLoading
  rownames(PLSModelLoading) <- seq(1, nrow(PLSModelLoading), 1)
  PLSModelLoading <-
    cbind(rownames(PLSModelLoading), PLSModelNames, PLSModelLoading)
  colnames(PLSModelLoading)[1:2] <- c("ID", "Features")
  loadTable <- PLSModelLoading[, c("ID", "Features")]
  remove(PLSModelLoading, PLSModelNames)
  return(list(plsPlotData, loadTable))
}
marsTrain <- function(data, xSelected, y, k, factorLabels)
{
  if(k > 1) {
    trControl <- caret::trainControl(method = 'cv', number = k)
  } else {
    trControl <- caret::trainControl(method = 'none')
  }
  caretOut <- caret::train(method = 'earth', trControl = trControl, x = data[,xSelected,drop = FALSE], 
                           y = factor(data[,y], levels = factorLabels), 
                           tuneGrid = data.frame(degree = 1, nprune = 100))
  return(caretOut)
}

marsPredictProb <- function(model, data, factorLabels)
{
  predictions = stats::predict(model, data, type='response')[,1]
  return(predictions)
}
MARSRegression <- function(data, y, factorLabels)
{
  data_y = as.numeric(factor(data[,y], levels=factorLabels))
  data_x = data[,setdiff(colnames(data), y),drop=FALSE]
  return(giveRankMARSBasic(data_x, data_y))
}

LARSRegression <- function(data, y, factorLabels)
{
  data_y = as.numeric(factor(data[,y], levels=factorLabels))
  data_x = as.matrix(data[,setdiff(colnames(data), y),drop=FALSE])
  return(giveRankLARSBasic(data_x, data_y))
}

giveRankLARSBasic <- function(x, y)
{
  larsObject = lars::lars(as.matrix(x), y, trace = T, type = "lar")
  index <- which(larsObject$Cp  %in% min(larsObject$Cp))
  tempLarsObj <- larsObject
  tempLarsObj$beta <- tempLarsObj$beta[1:index, , drop = F]
  larsObject <- tempLarsObj
  remove(tempLarsObj)
  beta = abs(larsObject$beta[nrow(larsObject$beta),])
  beta = as.data.frame(sort(beta, decreasing=T), stringsAsFactors = F)
  beta = as.data.frame(cbind(rownames(beta), beta[,1]), stringsAsFactors = F)
  beta[,2] = as.numeric(beta[,2])
  rownames(beta) <- beta[,1]
  colnames(beta) <- c("Importance")
  remove(larsObject)
  return(beta)
}
#Random forest function
RandomForestVariableImportance <- function(data, xSelected, y, numTrees, mTry, minNodeSize, 
                                           Importance, replace)
{
  caretOut <- rfTrain(data, xSelected, y, 1, numTrees, mTry, minNodeSize, Importance, replace)
  model <- caretOut$finalModel
  vImp <- ranger::importance(model)
  remove(model)
  vImp <- as.data.frame(vImp)
  vImp <- vImp*100 / max(vImp)
  colnames(vImp) <- "Importance"
  return(vImp)
}
rfTrain <- function(data, xSelected, y, cvK, ntree, mtry, nodesize, RFImportance, RFReplace)
{
  set.seed(1000)
  if(cvK > 1) {
    trControl <- caret::trainControl(method = 'cv', number = cvK, classProbs = TRUE)
  } else {
    trControl <- caret::trainControl(method = 'none', classProbs = TRUE)
  }
  
  data[,y] <- factor(data[,y], labels = make.names(levels(data[,y])))
  caretOut <- caret::train(method = 'ranger', x = data[,xSelected, drop = FALSE], y = data[,y], trControl = trControl, 
                           num.trees = ntree, importance = RFImportance, replace = RFReplace, classification = TRUE,
                           tuneGrid = data.frame(mtry = mtry, min.node.size = nodesize, splitrule = 'gini'))
  
  return(caretOut)
}

rfErrorVsTreePlot <- function(data, xSelected, y, ntree, mtry, nodesize, RFImportance, RFReplace, primaryColor)
{
  trees = as.numeric(ntree)
  if(trees!=0 && !(trees%%2))
  {
    trees = trees + 1
  }
  maxLimit <- 20
  interval <- max(5, floor(trees / maxLimit))
  interval <- ifelse(trees<=interval, 1, interval)
  xAxis = seq(0,trees,interval)
  # print(xAxis)
  plotData <- rbind.data.frame(lapply(xAxis[2:length(xAxis)], function(x){
    # print(paste0(x, " trees"))
    caretOut <- rfTrain(data, xSelected, y, 1, x, mtry, nodesize, RFImportance, RFReplace)
    model <- caretOut$finalModel
    c(model$num.trees, model$prediction.error) 
  }))
  plotData = matrix(unlist(plotData), ncol = 2, byrow = T)
  plotData = data.frame(plotData)
  colnames(plotData) <- c("Number of trees", "Prediction Error")
  g = ggplot2::ggplot(data=plotData, ggplot2::aes(x=`Number of trees`, y=`Prediction Error`, group=1)) +
    ggplot2::geom_line(color=primaryColor) +
    # geom_point() +
    ggplot2::labs(title= "Prediction error vs Number of trees") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
  return(g)
}

rfPredictProb <- function(model, data, factorLabels)
{
  pred <- ranger::predictions(stats::predict(model, data, type='response'))
  colnames(pred) <- factorLabels
  return(pred)
}

xgbTrain <- function(data, y, factorLabels, cvK, depth, learningRate, iterations)
{
  set.seed(1000)
  if(cvK > 1) {
    trControl <- caret::trainControl(method = 'cv', number = cvK, classProbs = TRUE)
  } else {
    trControl <- caret::trainControl(method = 'none', classProbs = TRUE)
  }
  xSelected <- setdiff(colnames(data), y)
  data[,y] <- factor(data[,y], labels = make.names(levels(data[,y])))
  caretOut <- caret::train(method = 'xgbLinear', trControl = trControl, x = data[,xSelected, drop = FALSE], 
                           y = data[,y], max.depth = depth, booster = 'gbtree',
                           tuneGrid = data.frame(eta = learningRate, nrounds = iterations, lambda = 0, alpha = 1))
  
  # # Xgboost doesn't work if the column type is integer
  # bst <- xgboost::xgboost(data = as.matrix(sapply(data[,setdiff(names(data),y),drop=F], function(x) as.numeric(x))),
  #                         label = outputVector,
  #                         max.depth = depth,
  #                         eta = learningRate,
  #                         nround = iterations,
  #                         objective = "binary:logistic",
  #                         verbose = 0,
  #                         save_period = NULL)
  return(caretOut)
}

xgbPredictProb <- function(model, data)
{
  pred <- stats::predict(model, data.matrix(data), probability = T)
  # pred <- 1-pred
  return(pred)
}

gamPredictProb <- function(model, data, predType)
{
  pred <- stats::predict(model, data, type=predType)
  return(pred)
}

gamCreateFormula<-function(train,y,col_to_smooth,splines)
{
  linear_col<- setdiff(colnames(train), col_to_smooth)
  linear_col<- setdiff(linear_col, y)
  linear_col <- `linear_col`
  col_ <- c()
  if(length(col_to_smooth)!=0)
  {
    col_ <- paste("s(",col_to_smooth," , ","bs=","\"",as.character(splines),"\"",")+",sep = '',collapse = '')
  }else
  {
    col_<-""
  }
  col2_ <- c()
  if (length(linear_col)!=0)
  {
    col_2 <- paste0(linear_col, "+", sep = '',collapse = '')
    col_to_smooth<-paste0(col_2,col_,collapse='')
  }else
  {
    col_to_smooth<-col_
  }
  col_to_smooth <- substr(col_to_smooth,1,nchar(col_to_smooth)-1)
  col_to_smooth <- paste0(y, "~", col_to_smooth)
  f = as.formula(col_to_smooth)
  return(f)
}

## Training with CV for GAM
gamTrainWithCV <- function(data, y, cvK, formula, ...) {
  
  resample <- NULL
  resampledCM <- NULL
  
  if(cvK > 1) {
    # create samples
    folds <- caret::createFolds(y = data[,y], k = cvK, list = TRUE)
    
    # variables required for metrics
    fill <- rep(NA, length(folds))
    resample <- data.frame(Resample = names(folds), Accuracy = fill)
    resampledCM <- data.frame(Resample = names(folds), cell1 = fill, cell2 = fill, cell3 = fill, cell4 = fill)
    
    ## I know you're going to judge me for using 'for', but I am doing this to save up on memory.
    ## Holding 'n' number of huge models in memory in a shiny brick is going to suck.
    for(i in 1:length(folds)) {
      
      # build model
      model <- tryCatch(
        mgcv::gam(formula = formula, data=data[-folds[[i]],, drop = FALSE], ...),
        error = function(e) NA
      )
      
      # if model did not throw an error, get metrics on validation data
      if(class(model)[1] == 'gam') {
        validation <- data[folds[[i]],,drop = FALSE]
        preds <- stats::predict(model, validation, type = 'response')
        preds <- factor(as.numeric(preds > 0.5))
        preds <- factor(preds, labels = levels(validation[,y]))
        cm <- caret::confusionMatrix(preds, validation[,y])
        
        # adding metrics to their global variables
        resample[i, 'Accuracy'] <- cm$overall[1]
        resampledCM[i, 2:5] <- as.vector(cm$table)
      } else {
        warning('Model could not be built, so returning NAs')
      }
      # removing pointless variables
      remove(model, cm, validation, preds)
    }
  }
  
  # building final model on the whole training data
  finalModel <- mgcv::gam(formula = formula,  data=data, ...)
  
  # returning a caret-like result for conformation with other models
  return(list(finalModel = finalModel, resample = resample, resampledCM = resampledCM))
}

lvqTrain <- function(data, xSelected, y, cvK, factorLabels, k=1, size=10)
{
  set.seed(1000)
  if(cvK > 1) {
    trControl <- caret::trainControl(method = 'cv', number = cvK)
  } else {
    trControl <- caret::trainControl(method = 'none')
  }
  caretOut <- caret::train(method = 'lvq', trControl = trControl, x = data[,xSelected, drop = FALSE], 
                           y = data[,y], tuneGrid = data.frame(size = size, k = k))
  return(caretOut)
}

# native function for prediction
lvqPredict <- function(codebook, testX, k=1, factorLabels = NULL)
{
  # getting the positive class to find the probability for
  positive <- ifelse(is.null(factorLabels), levels(codebook$cl)[2], factorLabels[2])
  # filtering out only necessary columns from test data
  testX <- testX[, codebook$xNames, drop = FALSE]
  tempFunc <- function(a, codebook, k)
  {
    d = apply(codebook$x, 1, function(x,y){return(dist(rbind(x,y), method='euclidean')[[1]])}, y=a)
    index = sort(d, index.return=T)$ix
    t = table(codebook$cl[index[1:k]])
    return(c(positive, t[positive]/sum(t)))
  }
  predictions = t(apply(testX, 1, tempFunc, codebook=codebook, k=k))
  predictions = as.data.frame(predictions, stringsAsFactors = FALSE)
  predictions[,2] <- as.numeric(predictions[,2])
  predictions[,1] <- 1-predictions[,2]
  colnames(predictions) <- c(setdiff(levels(codebook$cl), positive), positive)
  predictions[,'max.class'] <- colnames(predictions)[apply(predictions, 1, which.max)]
  predictions[,'max.class'] <- factor(predictions[,'max.class'], levels = factorLabels)
  return(predictions)
}

lvqPredictProb <- function(model, data, factorLabels)
{
  predictions = lvqPredict(model, data, model$tuneValue$k, factorLabels)
  return(predictions)
}
svmTrainWithCV <- function(data, y, cvK, svmKernel, svmGamma = NULL, svmDegree = NULL, svmCoef0 = NULL)
{
  # Setting defaults for svm params
  svmGamma <- ifelse(is.null(svmGamma), 0.01, svmGamma)
  svmDegree <- ifelse(is.null(svmDegree), 3, svmDegree)
  svmCoef0 <- ifelse(is.null(svmCoef0), 0, svmCoef0)
  
  resample <- NULL
  resampledCM <- NULL
  xSelected <- setdiff(colnames(data),y)
  
  if(cvK > 1) {
    # create samples
    folds <- caret::createFolds(y = data[,y], k = cvK, list = TRUE)
    
    # variables required for metrics
    fill <- rep(NA, length(folds))
    resample <- data.frame(Resample = names(folds), Accuracy = fill)
    resampledCM <- data.frame(Resample = names(folds), cell1 = fill, cell2 = fill, cell3 = fill, cell4 = fill)
    
    ## I know you're going to judge me for using 'for', but I am doing this to save up on memory.
    ## Holding 'n' number of huge models in memory in a shiny brick is going to suck.
    for(i in 1:length(folds)) {
      
      # build model
      model <- tryCatch(
        parallelSVM2(x = data[-folds[[i]],xSelected,drop=F], y = data[-folds[[i]],y], 
                     type='C-classification', probability=TRUE,
                     kernel=svmKernel, gamma = svmGamma, degree = svmDegree, coef0 = svmCoef0),
        error = function(e) NA
      )
      
      # if model did not throw an error, get metrics on validation data
      if(class(model)[1] == 'parallelSVM') {
        validation <- data[folds[[i]],,drop = FALSE]
        # custom predict function because of parallelization
        preds <- svmPredictProb(model, validation[,xSelected,drop=F], levels(data[,y]))
        preds <- factor(as.numeric(preds$p > 0.5))
        preds <- factor(preds, levels = levels(validation[,y]))
        # preds <- factor(preds, labels = levels(validation[,y]))
        cm <- caret::confusionMatrix(preds, validation[,y])
        # adding metrics to their global variables
        resample[i, 'Accuracy'] <- cm$overall[1]
        resampledCM[i, 2:5] <- as.vector(cm$table)
      } else {
        warning('Model could not be built, so returning NAs')
      }
      # removing pointless variables
      remove(model, cm, validation, preds)
    }
  }
  
  # building final model on the whole training data
  finalModel <- parallelSVM2(x = data[,xSelected,drop=F], y = data[,y], type='C-classification', probability=TRUE,
                             kernel=svmKernel, gamma = svmGamma, degree = svmDegree, coef0 = svmCoef0)
  
  # returning a caret-like result for conformation with other models
  return(list(finalModel = finalModel, resample = resample, resampledCM = resampledCM))
}

# modified function from https://github.com/cran/parallelSVM/blob/master/R/parallelSVM.R
parallelSVM2 <- function(x, y = NULL, numberCores = parallel::detectCores()-1, samplingSize = 0.2, 
                         scale = TRUE, type = NULL, kernel = "linear", degree = 3, 
                         gamma = if (is.vector(x)) 1 else 1 / ncol(x), coef0 = 0, cost = 1, nu = 0.5,
                         class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
                         shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, seed = 1L,
                         ..., subset, na.action = na.omit){
  # Default declaration
  
  # to clear parallel cores
  unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }
  
  # Use the amount of cores provided and create a cluster that will kill itself upon exit
  parallelSVM::registerCores(numberCores)
  on.exit(unregister())
  
  # Create random bootstrap training samples (with replacement) in parallel
  trainSamples <- parallelSVM::trainSample(x,y,numberCores,samplingSize)
  
  # Get the function call and its arguments
  fcall <- match.call()
  
  # remember the call used
  call <- gsub('.default','',deparse(fcall))
  call <- gsub('    ','',call)
  
  # Construct the new call expression
  fcall[[1]] <- e1071::svm
  
  # Filter out numberCores and samplingSize
  fcall$numberCores  <- NULL
  fcall$samplingSize <- NULL
  
  # Create copies with the correct data
  function_call <- list()
  for (i in 1:numberCores){
    function_call[[i]]   <- fcall
    function_call[[i]]$x <- trainSamples[[i]]$x
    function_call[[i]]$y <- trainSamples[[i]]$y
    ###############################
    ### Next 4 lines were added ###
    ###############################
    function_call[[i]]$degree <- degree
    function_call[[i]]$coef0 <- coef0
    function_call[[i]]$kernel <- kernel
    function_call[[i]]$gamma <- gamma
  }
  
  #The following package is beign loaded because the binary operator '%dopar%' is being called from package foreach
  library(foreach)
  # parallel SVM creation
  modelDataSvm <- foreach(i = 1:numberCores) %dopar% {
    # Do the call
    eval(function_call[[i]])
  }
  
  unregister()
  closeAllConnections()
  
  # Set a correct class
  class(modelDataSvm) <- "parallelSVM"
  attr(modelDataSvm,"call") <- call
  detach('package:foreach')
  return(modelDataSvm)
}

svmPredictProb <- function(model, data, factorLabels)
{
  p = c()
  ncore = length(model)
  p = lapply(c(1:ncore),function(x){
    pred = stats::predict(model[[x]], data, probability = TRUE)
    pred = attr(pred, 'probabilities')
    pred = pred[,factorLabels]
    return(pred[,2])
  })
  p = rowSums(cbind.data.frame(p)) / ncore
  dum <- rep(0,length(p))
  p = cbind.data.frame(dum,p)
  return(p)
}
knnTrain <- function(data, xSelected, y, cvK, k, factorLabels)
{
  if(cvK > 1) {
    trControl <- caret::trainControl(method = 'cv', number = cvK)
  } else {
    trControl <- caret::trainControl(method = 'none')
  }
  caretOut <- caret::train(method = 'kknn', trControl = trControl, x = data[,xSelected,drop = FALSE], y = data[,y], 
                           tuneGrid = data.frame(kmax = k, distance = 2, kernel = 'optimal'))
  return(caretOut)
}

knnPredictProb <- function(model, data, factorLabels)
{
  predictions <- stats::predict(model, data, type = 'prob')[,2]
  return(predictions)
}

# native function for plot
knnPlots <- function(train, test, xSelected, y, kMax, factorLabels, primaryColor, secondaryColor)
{
  graphData <- lapply(1:kMax, function(k)
  {
    pred = class::knn(as.matrix(train[,xSelected,drop=F]),as.matrix(test[,xSelected,drop=F]),
                      factor(train[,y],levels = factorLabels), k)
    cm = caret::confusionMatrix(pred, factor(test[,y], labels=factorLabels), positive = factorLabels[2])  
    accuracy = as.numeric(cm$overall[[1]])
    return(accuracy)
  })
  graphData <- cbind.data.frame(k=c(1:kMax), accuracy=unlist(graphData))
  g = ggplot2::ggplot(data=graphData, ggplot2::aes(x=k, y=accuracy, group=1)) +
    ggplot2::geom_line(color=primaryColor) +
    ggplot2::geom_point() +
    ggplot2::coord_cartesian(xlim = c(1:nrow(graphData)), ylim = c(0,1)) +
    ggplot2::labs(title= "Accuracy Vs K") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
    ggplot2::scale_x_continuous(breaks = seq(1,kMax,1))
  return(g)
}


predictLoadedData <-
  function(loadedData,
           workingModel,
           modelSelected,
           loadY,
           groundTruth,
           factorLabels,
           binaryCatData,
           y,
           loadedCatData,
           xSelected)
  {
    print("Entering prediction")
    loadedData <- loadedData[, workingModel$featuresSelected, drop = F]
    loadErrorMsg <- invisible(NULL)
    cm <- NULL
    
    
    
    if (modelSelected == "Ensemble")
    {
      print("Inside ensemble")
      predictions <-
        workingModel$predFunc(
          workingModel$modelSuite,
          loadedData,
          workingModel$type,
          workingModel$weights,
          workingModel$ensembleModel,
          workingModel$threshold,
          loadedCatData,
          workingModel$featuresSelected
        ) #xSelected)
      
    } else if ((modelSelected == "RF" ||
                modelSelected == "PDT") && workingModel$catCols == "cat") {
      print("inside categorical RF/PDT")
      newData <-
        getCategoricalData(loadedData, loadedCatData, workingModel$featuresSelected)
      loadedData <- newData$data
      xSelected <- newData$x
      remove(newData)
      tryCatch(
        workingModel$predFunc(
          workingModel$model,
          loadedData,
          workingModel$factorLabels,
          workingModel$threshold,
          loadedCatData,
          xSelected
        ),
        error = function(e) {
          print(e)
        }
      )
      predictions <-
        workingModel$predFunc(
          workingModel$model,
          loadedData,
          workingModel$factorLabels,
          workingModel$threshold,
          loadedCatData,
          xSelected
        )
    } else {
      print("inside normal predict")
      tryCatch(
        workingModel$predFunc(
          workingModel$model,
          loadedData,
          workingModel$factorLabels,
          workingModel$threshold
        ),
        error = function(e) {
          print(e)
        }
      )
      predictions <-
        workingModel$predFunc(
          workingModel$model,
          loadedData,
          workingModel$factorLabels,
          workingModel$threshold
        )
    }
    predictions <- factor(predictions, levels = factorLabels)
    if (loadY)
    {
      if (!is.null(groundTruth))
      {
        if (!is.null(binaryCatData) &&
            (y %in% colnames(binaryCatData)) && !is.null(groundTruth) &&
            sort(levels(as.factor(binaryCatData[, y]))) == sort(levels(as.factor(groundTruth))))
        {
          loadedData <- cbind.data.frame(loadedData, groundTruth, predictions)
          colnames(loadedData)[(ncol(loadedData) - 1):ncol(loadedData)] <-
            c(y, "predictedClass")
          
          cm = caret::confusionMatrix(predictions, groundTruth, positive = factorLabels[2])
        } else
        {
          loadedData <- cbind.data.frame(loadedData, predictions)
          colnames(loadedData)[ncol(loadedData)] <- "predictedClass"
          loadErrorMsg <- paste0(
            "The factor levels of the '",
            y,
            "' column in the uploaded dataset do not match with that of the original dataset's dependent variable"
          )
        } # end of block to assess if factor level of uploaded ground truth batches
      } else
      {
        loadedData <- cbind.data.frame(loadedData, predictions)
        colnames(loadedData)[ncol(loadedData)] <- "predictedClass"
        loadErrorMsg <-
          paste0(
            "The uploaded dataset has no column matching with the selected
            dependent variable '",
            y,
            "'. Therefore, no ground truth analysis is unavailable."
          )
      }
    } else
    {
      loadedData <- cbind.data.frame(loadedData, predictions)
      colnames(loadedData)[ncol(loadedData)] <- "predictedClass"
    }
    return(list(
      data = loadedData,
      errorMssg = loadErrorMsg,
      cm = cm
    ))
  }
