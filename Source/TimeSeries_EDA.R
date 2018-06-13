dateColumnSel <- function(input, output, session, reactData){
  output$dateTimeSelUI <- shiny::renderUI({
    
    shiny::fluidRow(class='box',
             shiny::column(3,shiny::selectInput('TimeVar','Select DateTime Column',choices=colnames(reactData$processed_dataset),multiple=FALSE)),
             shiny::column(3,shiny::textInput("DTformat", "Datetime format (in data)",value = "y-m-d H:M:S")),
             shiny::column(3,shiny::selectInput("TimeZone", "Select Time Zone", choices = OlsonNames(), multiple = FALSE, selected = "GMT")),
             # shiny::column(3,shiny::selectInput("AnomalyVar", "Select variable for analysis", choices = reactData$numeric_cols, multiple = TRUE,
             #             selected = "-None-")),
             shiny::column(class='button_pad',2,shiny::actionButton('variable_select_submit','Submit')),
             shiny::column(12,shiny::verbatimTextOutput("preprocessing")))
    
    
  })
  
  
  TimeVar <<- DTformat <<- AnomalyVar <<- TimeZone <<- NULL
  
  
  
  shiny::observeEvent(input$variable_select_submit, {
    tryCatch({
      varsList <- c(input$TimeVar, input$DTformat, input$AnomalyVar)
      TimeVar <<- input$TimeVar
      reactData$TimeVar <- input$TimeVar
      DTformat <<- input$DTformat
      reactData$DTformat <- input$DTformat
      #AnomalyVar <<- input$AnomalyVar
      TimeZone <<- input$TimeZone
      reactData$TimeZone <- input$TimeZone
      
      # Check if the selected column in already in date time format
      if(class(reactData$processed_dataset[,TimeVar])[1] %in% c("POSIXct","POSIXt")){
        shiny::showNotification('Selected column is already in Date-Time format',type='warning')
        return()
      }
      
      output$datetime_warning_text <- shiny::renderText("")
      output$datetime_error_text <- shiny::renderText("")
      Sys.setenv(TZ = TimeZone)
      
      reactData$processed_dataset[[TimeVar]] <<- lubridate::parse_date_time(reactData$processed_dataset[[TimeVar]],orders = DTformat,tz = TimeZone)
      # split format to get without spaces and special characters
      DTformat <- unlist(strsplit(gsub("[[:blank:][:punct:]]","",DTformat),split = ""))
      format_dict <- c(y="years",m="months",d="days",H="hours",M="minutes",S="seconds")
      reactData$format_list <- lapply(DTformat,FUN = function(x) format_dict[[x]])
      shiny::updateSelectInput(session,'roll_level',choices=reactData$format_list)
      
      # xts_df <- reactData$processed_dataset[,c(TimeVar,AnomalyVar)]
      # colnames(globalData$Data)[1] <<- 'TimeVar'
      xts_data <<-xts:: as.xts(reactData$processed_dataset,order.by = reactData$processed_dataset[[TimeVar]])
      
      min_time <- min(reactData$processed_dataset[[TimeVar]])
      max_time <- max(reactData$processed_dataset[[TimeVar]])
      
      reactData$preprocessing_text <- paste0(
        "* Time Column Selected: ", TimeVar, "\n"
        ," Start Time: ", min_time, "\n"
        ," End Time: ", max_time, "\n"
      )
      output$preprocessing <- shiny::renderText(
        reactData$preprocessing_text
      )
      
      # Update the select inputs to the original selection.
      # Original selection goes away due to reactivity dependency on dataset which changes on convering to time column
      shiny::updateSelectInput(session,'TimeVar',selected=reactData$TimeVar)
      shiny::updateSelectInput(session,'DTformat',selected=gsub(",","",DTformat))
      shiny::updateSelectInput(session,'TimeZone',selected=TimeZone)
      
    },error = function(e) {
      output$datetime_error_text <- shiny::renderText(e$message)
      print(e)
    },warning = function(w){
      output$datetime_warning_text <- shiny::renderText(w$message)
    }
    )
  })
  shiny::div(style = "color:red",textOutput("datetime_warning_text"))
  shiny::div(style = "color:red",textOutput("datetime_error_text"))
  
  AnomalyVar <- NULL
  shiny::observeEvent(input$variable_select_submit,{
    cols <- colnames(reactData$processed_dataset)
    numCols <- cols[sapply(reactData$processed_dataset,is.numeric)]
    AnomalyVar <<- numCols
    reactData$AnomalyVar <-  numCols
    
    hierarchy_columns <- input$hierarchy_columns
    if(isHierarchyGiven())
      reactData$eda_dataset <- reactData$processed_dataset[,c(hierarchy_columns,reactData$TimeVar,AnomalyVar)]
    else reactData$eda_dataset <- reactData$processed_dataset[,c(reactData$TimeVar,AnomalyVar)]
    
  })
  isHierarchyGiven <- function(){
    # hierarchy_columns <- input$hierarchy_columns
    # if(is.null(hierarchy_columns)) 
    #   return(FALSE)
    return(
      # length(hierarchy_columns)==0 | 
      input$hierarchy_checkbox)
  }
  
}




hierarchySel <- function(input, output, session, reactData){
  output$hierchacyUI <- shiny::renderUI({
    shiny::fluidRow(class='box',
             shiny::column(12,checkboxInput("hierarchy_checkbox","Check this option if the dataset has hierarchy or panels and then select the appropriate columns."),style = "background-color: #F5F5F5;"
             ))
  })
  
  output$HierarchySelUI <- shiny::renderUI({
    if(!is.null(input$hierarchy_checkbox) && input$hierarchy_checkbox){
      shiny::fluidRow(class='box',
               shiny::column(3,shiny::selectInput('hierarchy_columns', 'Select columns that define the hierarchy',choices=setdiff(colnames(reactData$processed_dataset),TimeVar),multiple=TRUE)),
               shiny::column(class='button_pad',2,shiny::actionButton("hierarchy_submit","Submit")),
               shiny::column(12,tags$p('Note: Select the columns in a top-down manner. E.g. Country, State, District')),
               shiny::column(12,shiny::verbatimTextOutput("hierarchy_columns_show"))
      )
    }
  })
  
  
  
  HierarchyColumns <- NULL
  reactData$HierarchyColumns <- NULL
  #reactData$UniqueHierachyValues <- NULL
  
  shiny::observeEvent(input$hierarchy_submit,{
    hCols <- input$hierarchy_columns
    if(length(hCols)==0) {
      reactData$HierarchyColumns <- c()
      return()
    }
    
    HierarchyColumns <<- hCols
    reactData$HierarchyColumns <<- hCols ## Add hierarhcy columns to reactData
    hData <- reactData$processed_dataset[,hCols,drop=FALSE]
    #reactData$UniqueHierachyValues <<- apply(hData,1,function(x){ paste0(x,collapse='-')})
    reactData$hierarchy_columns_show_text <- paste0("Hierarchy variables selected :\n","  ", paste(HierarchyColumns,collapse=', '),"\n")
    output$hierarchy_columns_show <- shiny::renderText(reactData$hierarchy_columns_show_text)
    
    
  })
  
}



visualizeHierarchy <- function(input, output, session, reactData){
  output$visualiseButton <- shiny::renderUI({conditionalPanel('input.hierarchy_submit!=0 & input.hierarchy_checkbox==1',shiny::actionButton(inputId = "tree_button",
                                                                                                                              label = "Visualize hierarchy"))})
  
  output$visualiseHieratchyUI <- shiny::renderUI({ conditionalPanel('input.hierarchy_checkbox==1',              
                                                             shiny::verbatimTextOutput("level_cut_text"),
                                                             tags$div(id="div_tree",style='border: 1px solid #e3e3e3;background-color: #fbfbfb;')
  )})
  
  
  #create div referring to div in the d3script                 
  
  shiny::observeEvent(input$tree_button,{
    tryCatch({
      
      h_level <- reactData$HierarchyColumns
      level_1 <-h_level[1]
      data <- reactData$processed_dataset
      if(length(unique(data[,level_1]))>1){
        data$LEVEL1 <- 'All'
        h_level <- c('LEVEL1',h_level)
      }
      
      x <- recur(data,h_level)
      var_json <- jsonlite::toJSON(x[[1]],pretty = TRUE, auto_unbox = TRUE)
      
      if(level_cut){
        output$level_cut_text <- shiny::renderText("Note:- Only 10 panels per level are shown")
      }
      
      session$sendCustomMessage(type="jsondata",var_json)
    },error=function(e) shiny::showNotification(e[1]))
  })
  
  
  recur <- function(data,levelCols){
    
    data <- data[,levelCols,drop=F]
    values <- unique(data[,levelCols[1]])
    if(length(values >5)) {
      level_cut <<- TRUE
    }
    print(length(values))
    ret <- lapply(values,function(x){
      if(length(levelCols)>1){
        str <- paste0(levelCols[1],'==',shQuote(x))
        data <- data %>% dplyr::filter_(str)
        list(
          name = x,
          children = recur(data,levelCols[-1]))
        
      }
      else {
        list(name=x,
             size= 1)
      }
    })
    
  }
}



timeSeriesEDA <- function(input, output, session, reactData){
  
  smoothing_functions_list <- c("Moving Average","Simple Exponential Smoothing")
  
  smoothing_text <- 'Use smoothing techniques to remove noise from the data'
  
  auto_correlation_text <- '</br><b> Auto-Correlation</b> : Autocorrelation is the correlation of a signal with a delayed copy of itself as a function of delay. Informally, it is the similarity between observations as a function of the time lag between them. The analysis of autocorrelation is a mathematical tool for finding repeating patterns, such as the presence of a periodic signal obscured by noise. </br>    
  
  </br><b>Partial autocorrelation function (PACF)</b> : PACF gives the partial correlation of a time series with its own lagged values, controlling for the values of the time series at all shorter lags. It contrasts with the autocorrelation function, which does not control for other lags.</br>
  
  </br>This function plays an important role in data analyses aimed at identifying the extent of the lag in an autoregressive model. By plotting the partial autocorrelative functions one could determine the appropriate lags p in an AR (p) model or in an extended ARIMA (p,d,q) model.</br>
  '
  
  time_series_decomposition_text <- '</br> The decomposition of time series is a statistical method that deconstructs a time series into several components, each representing one of the underlying categories of patterns. Time Series are generally decomposed into Trend, Cyclical, Seasonal and Random(Remainder) components.</br>
  
  </br>Seasonal Cycle length is the number of observations per "cycle"(normally a year, but sometimes a week,a day, an hour, etc).</br>
  
  
  </br><table>
  <tr>
  <th>Data</th>
  <th>Frequency</th>
  </tr>
  <tr>
  <td>Annual</td>
  <td>1</td>
  </tr>
  <tr>
  <td>Quarterly</td>
  <td>4</td>
  </tr>
  <tr>
  <td>Monthly</td>
  <td>12</td>
  </tr>
  <tr>
  <td>Weekly</td>
  <td>52</td>
  </tr>
  </table></br>
  
  </br>If the frequency of observations is greater than once per week, then there is usually more than one way of handling the frequency. For example, data with daily observations might have a weekly seasonality (frequency=7) or an annual seasonality (frequency=365). Similarly, data that are observed every minute might have an hourly seasonality (frequency=60), a daily seasonality (frequency=24x60=1440), a weekly seasonality (frequency=24x60x7=10080) and an annual seasonality (frequency=24x60x365.25=525960)</br>
  
  '
  
  stationarity_text <- '</br> Stationarity refers to time invariance of some, or all, of the statistics of a random process, such as mean, autocorrelation, and n-th order distribution. Stationarity is an assumption underlying many statistical procedures used in time series analysis, non-stationary data is often transformed to become stationary. The most common cause of violation of stationarity is due to trend in mean, which can be either due to the presence of a unit root or of a deterministic trend.</br> 
  
  </br> A unit root test tests whether a time series variable is non-stationary and possesses a unit root. The null hypothesis is generally defined as the presence of a unit root and the alternative hypothesis is either stationarity, trend stationarity or explosive root depending on the test used.</br>
  
  </br><b>ADF (Augmented Dickey-Fuller test)</b> :  An augmented Dickey–Fuller test (ADF) tests the null hypothesis that a unit root is present in a time series sample. The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. It is an augmented version of the Dickey–Fuller test for a larger and more complicated set of time series models.The augmented Dickey–Fuller (ADF) statistic, used in the test, is a negative number. The more negative it is, the stronger the rejection of the hypothesis that there is a unit root at some level of confidence.</br>
  
  </br><b>KPSS (Kwiatkowski–Phillips–Schmidt–Shin test)</b> : KPSS tests are used for testing a null hypothesis that an observable time series is stationary around a deterministic trend (i.e. trend-stationary) against the alternative of a unit root.Contrary to most unit root tests, the presence of a unit root is not the null hypothesis but the alternative. Additionally, in the KPSS test, the absence of a unit root is not a proof of stationarity but, by design, of trend-stationarity. This is an important distinction since it is possible for a time series to be non-stationary, have no unit root yet be trend-stationary. In both unit root and trend-stationary processes, the mean can be growing or decreasing over time; however, in the presence of a shock, trend-stationary processes are mean-reverting (i.e. transitory, the time series will converge again towards the growing mean, which was not affected by the shock) while unit-root processes have a permanent impact on the mean (i.e. no convergence over time). </br>
  
  </br><b>PP (Phillips-Perron test)</b> : PP test is a unit root test. It is used in time series analysis to test the null hypothesis that a time series is integrated of order 1. </br>
  
  </br><b>Note</b> : On testing for stationarity, if you find any non-stationary columns, the Data Differencing option in the Data Wrangling section can be used to difference the series and then recheck for stationarity.
  
  '
  
  cointegration_text <- '</br>Two or more set of time series are said to be Cointegrated if a linear combination of these series results in a series which is Integrated of Order 0. Order of Integration denotes the minimum number of differences required to obtain a stationary series.</br>
  
  </br>Note:- Cointegration test requires atleast two columns</br>
  
  '
  
  spectral_analysis_text <- ''
  
  cross_correlation_text <- '</br>Cross-correlation is a measure of similarity of two series as a function of the displacement of one relative to the other.</br>
  
  </br>In the relationship between two time series ( x(t) and y(t) ), the series y(t) may be related to past lags of the x-series.  The sample cross correlation function (CCF) is helpful for identifying lags of the x-variable that might be useful predictors of y(t).</br>
  
  </br>In R, the sample CCF is defined as the set of sample correlations between x(t+h) and y(t) for h = 0, ±1, ±2, ±3, and so on.  A negative value for h is a correlation between the x-variable at a time before t and the y-variable at time t.   For instance, consider h = −2.  The CCF value would give the correlation between x(t-2) and y(t).</br>
  
  </br><ul><li>When one or more x(t+h) , with h negative, are predictors of y(t), it is sometimes said that x leads y.</li>
  <li>When one or more x(t+h), with h positive, are predictors of y(t),  it is sometimes said that x lags y.</li></ul></br>
  
  '
  
  
  output$tabEDA <- shiny::renderUI({
    tagList(
      #shiny::fluidRow(style='padding-top:10px',shiny::column(12, shiny::selectInput('eda_panel','Select Panel',choices= NULL))),
      shiny::fluidRow(style='padding:10px 0px 10px',
               conditionalPanel('input.hierarchy_checkbox==1',uiOutput('eda_panel_ui'))
               # conditionalPanel('output.eda_panel_ui!=null'
               #                  ,shiny::column(12,shiny::actionButton('eda_panel_submit','Submit'))
               ,tags$hr()
      ),
      
      shiny::fluidRow(shiny::column(12,tags$h3('Univariate Analysis'))),
      shiny::fluidRow(shiny::column(12,
                      # tags$hr(),
                      tags$h4('Time Series Decomposition'),
                      tags$p(HTML(time_series_decomposition_text)),
                      shiny::actionButton("tsdecomp_go","Press this for Time Series Decomposition"),
                      conditionalPanel("input.tsdecomp_go%2",
                                       shiny::renderUI({
                                         list(tagList(shiny::fluidRow(class='box',
                                                               shiny::column(4,shiny::selectInput('tsColSel1','Select Time Series Column',choices=setdiff(colnames(reactData$eda_dataset),c(TimeVar,reactData$HierarchyColumns)),multiple=TRUE)),
                                                               shiny::column(2,shiny::textInput('freqInput','Seasonal Cycle Length',value=12)),
                                                               shiny::column(class='button_pad',2,shiny::actionButton('tsColSubmit1','Submit')))),
                                              shiny::fluidRow(class='row_box row_pad',uiOutput('tspui1')))
                                       })
                      ),
                      tags$hr()
      )),
      shiny::fluidRow(shiny::column(12,
                      tags$h4('Stationarity Test'),
                      tags$p(HTML(stationarity_text)),
                      shiny::actionButton("stationarity_go","Press this for checking stationarity"),
                      conditionalPanel("input.stationarity_go%2",
                                       shiny::renderUI(tagList(shiny::fluidRow(class='box',
                                                                 shiny::column(4,shiny::selectInput('stationarity_column','Time Series Variable',choices = setdiff(colnames(reactData$eda_dataset),c(TimeVar,reactData$HierarchyColumns)),multiple=TRUE)),
                                                                 
                                                                 shiny::column(2,shiny::selectInput('stationarity_func_name','Test Method',choices=c('ADF','KPSS','PP'))),
                                                                 shiny::column(2,uiOutput('stationarity_func_params_ui')),
                                                                 shiny::column(2,class='button_pad',shiny::actionButton('stationarity_test_submit','Submit'))),
                                                        shiny::fluidRow(class='row_box row_pad',uiOutput('stationarity_table_output'))
                                       ))
                      ),
                      tags$hr()
      )),
      shiny::fluidRow(shiny::column(12,
                      tags$h4('Auto-Correlation & Partial Auto-Correlation'),
                      tags$p(HTML(auto_correlation_text)),
                      shiny::actionButton("autocorrelation_go","Press this for checking Correlation"),
                      conditionalPanel("input.autocorrelation_go%2",
                                       shiny::renderUI(tagList(shiny::fluidRow(class='box',
                                                                 shiny::column(4,shiny::selectInput('autocorrelation_columns','Time Series Variable',choices = setdiff(colnames(reactData$eda_dataset),c(TimeVar,reactData$HierarchyColumns)))),
                                                                 shiny::column(2,class='button_pad',shiny::actionButton('autocorrelation_submit','Submit'))),
                                                        shiny::fluidRow(class='row_box row_pad',uiOutput('autocorrelation_output'))
                                       ))
                      ),
                      tags$hr()
      )),
      shiny::fluidRow(shiny::column(12,
                      tags$h4('Spectral Analysis'),
                      tags$p(HTML(spectral_analysis_text)),
                      shiny::actionButton("spectral_go","Press this for spectral analysis"),
                      conditionalPanel("input.spectral_go%2",
                                       shiny::renderUI(tagList(shiny::fluidRow(class='box',
                                                                 shiny::column(4,shiny::selectInput('spectral_columns','Time Series Variable',choices = setdiff(colnames(reactData$eda_dataset),c(TimeVar,reactData$HierarchyColumns)),multiple=FALSE)),
                                                                 shiny::column(2,class='button_pad',shiny::actionButton('spectral_test_submit','Submit'))),
                                                        shiny::fluidRow(class='row_box row_pad',plotOutput('spectral_output'))
                                       ))
                      ),
                      tags$hr()
      )),
      shiny::fluidRow(shiny::column(12,tags$h3('Multivariate Analysis'))),
      shiny::fluidRow(shiny::column(12,
                      tags$h4('Cointegration Test'),
                      tags$p(HTML(cointegration_text)),
                      shiny::actionButton("cointegration_go","Press this for checking cointegration"),
                      conditionalPanel("input.cointegration_go%2",
                                       shiny::renderUI(tagList(shiny::fluidRow(class='box',
                                                                 shiny::column(4,shiny::selectInput('cointegration_func_name','Conintegration Test Method',choices=c('Engle-Granger Method','Johansen Procedure'))),
                                                                 shiny::column(4,shiny::selectInput('cointegration_columns','Time Series Variable',choices = setdiff(colnames(reactData$eda_dataset),c(TimeVar,reactData$reactData$HierarchyColumns)),multiple=TRUE)),
                                                                 shiny::column(2,class='button_pad',shiny::actionButton('cointegration_test_submit','Submit'))),
                                                        shiny::verbatimTextOutput('cointegration_output')
                                       ))),
                      tags$hr()
      )),
      shiny::fluidRow(shiny::column(12,
                      tags$h4('Cross-Correlation'),
                      tags$p(HTML(cross_correlation_text)),
                      shiny::actionButton("correlation_go","Press this for checking Correlation"),
                      conditionalPanel("input.correlation_go%2",
                                       shiny::renderUI(tagList(shiny::fluidRow(class='box',
                                                                 shiny::column(4,shiny::selectInput('correlation_columns','Time Series Variable',choices = setdiff(colnames(reactData$eda_dataset),c(TimeVar,reactData$reactData$HierarchyColumns)),multiple=TRUE)),
                                                                 shiny::column(2,class='button_pad',shiny::actionButton('correlation_test_submit','Submit'))),
                                                        shiny::fluidRow(class='row_box row_pad',uiOutput('correlation_output'))
                                       ))
                      )
      ))
      
    )
  })
  
  
  shiny::observeEvent(input$tsColSubmit1,{
    tryCatch({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing.....")
      
      tsColSel <- input$tsColSel1
      freq <- as.numeric(input$freqInput)
      reactData$tsDecomp_tsColSel <- input$tsColSel1
      reactData$tsDecomp_freq <- as.numeric(input$freqInput)
      if(freq >= nrow(reactData$eda_dataset)){
        shiny::showNotification('Freqency should be greater than 1 and less than #rows',type='error')
        return()
      }
      
      tsObjList <- lapply(tsColSel,function(x) {
        xtObj <- xts::as.xts(reactData$eda_dataset[,x],order.by=reactData$eda_dataset[,TimeVar])
        attr(xtObj,'frequency') <-freq
        stats::decompose(as.ts(xtObj))
      })
      print("2")
      names(tsObjList) <- tsColSel
      attr(tsObjList,'TimeVar') <- sort(reactData$eda_dataset[,TimeVar])
      reactData[['tsp1']] <- tsObjList
      print("3")
    },error=function(e) print(e))
  })   
  
  
  
  shinyjs::useShinyjs(rmd=T)
  
  shiny::observeEvent(input$stationarity_func_name,{
    testName <- input$stationarity_func_name
    paramsUI <- switch(testName,
                       'ADF'= {
                         list(shiny::textInput('adf_lag_order','Lag Order',placeholder='Auto'))
                       },{})
    
    output$stationarity_func_params_ui <- shiny::renderUI({
      paramsUI
    })
  })
  
  shiny::observeEvent(input$stationarity_test_submit,{
    tryCatch({
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing.....")
      
      testCol <- input$stationarity_column
      testFunc <- input$stationarity_func_name
      stationarity_test_tables <- list()
      
      reactData$stationarity_testCol <- testCol
      reactData$stationarity_testFunc <- testFunc
      
      # Read other parameters
      adf_lag_order <- input$adf_lag_order
      reactData$adf_lag_order <- adf_lag_order
      
      # Collate params
      params <- switch(testFunc,
                       'ADF'=list( k = as.numeric(adf_lag_order)),NA)
      
      for(i in 1:length(testCol)){
        testOutput <- stationaryTest(testFunc, c(list(reactData$eda_dataset[,testCol[i]]),if(!(is.null(params) | is.na(params))) params))
        j <- 2*i - 1
        stationarity_test_tables[[j]] <- tags$h5(paste0('Column: ',testCol[i]))
        stationarity_test_tables[[j+1]] <- DT::datatable(testOutput, options = list(paging = FALSE, escape = FALSE, scrollX = TRUE, searching = FALSE),rownames=FALSE) 
      }
      
      reactData$stationarity_table_output <- htmltools::browsable(stationarity_test_tables)
      output$stationarity_table_output <- shiny::renderUI({ htmltools::browsable(stationarity_test_tables) })
      
    },error=function(e)shiny::showNotification(e[1]))
  })
  
  
  stationaryTest <- function(testType,params){
    ##functions are used in switch case
    library(tseries)
    testFuncName <- switch(testType,
                           'ADF'= c('adf.test','Data is not stationary','Data is stationary'),
                           'KPSS'= c('kpss.test','Data is trend stationary','Data is not trend stationary'),
                           'PP'= c('PP.test','Data is not stationary','Data is stationary'))
    
    results <- do.call(testFuncName[1],params)
    
    # p-value check is different for differnt functions.Significance level set to 5%.
    pValueCheck <- switch(testType,
                          'ADF'=,'PP'= results$p.value > 0.05,
                          'KPSS'= results$p.value < 0.05)
    
    
    finding <-  ifelse(pValueCheck, "The time-series is not stationary", "The time-series is stationary")
    
    Attribute <- c(results$method,
                   "Null hypothesis",
                   "Alternate hypothesis",
                   paste0('Test Statistic (',attr(results$statistic,'names'),")"),
                   paste0('Parameter (',attr(results$parameter,'names'),')'),
                   "p-value",
                   "Result")
    
    Description <- c("Unit root test used to check the stationarity of the series",
                     testFuncName[2],
                     testFuncName[3],
                     round(results$statistic,6),
                     results$parameter,
                     round(results$p.value, 2),
                     finding)
    
    data.frame(Attribute,Description,stringsAsFactors = FALSE)
  }
  
  
  
  shiny::observeEvent(input$autocorrelation_submit,{
    tryCatch({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing.....")
      
      testCol <- input$autocorrelation_columns
      
      acf_data <- reactData$eda_dataset[,testCol]
      bacf <- acf(na.omit(acf_data))
      bacfdf <- with(bacf, data.frame(lag, acf))
      
      acf_plot <- ggplot2::ggplot(data = bacfdf, mapping = ggplot2::aes(x = lag, y = acf)) +
        ggplot2::geom_segment(lineend = "butt", mapping = ggplot2::aes(xend = lag, yend = 0),colour = 'blue') +
        ggplot2::geom_hline(yintercept = 0, color ='blue') +  
        ggplot2::geom_hline(
          yintercept = c(
            stats::qnorm((1 + 0.95)/2)/sqrt(length(na.omit(acf_data))), 
            -(stats::qnorm((1 + .95)/2)/sqrt(length(na.omit(acf_data))))
          ),
          color = "purple", linetype = "dashed"
        ) +
        ggplot2::ggtitle(paste("Auto-Correlation Plot:",testCol)) + 
        ggplot2::xlab("Lag") +
        ggplot2::ylab("Auto-Correlation") +  
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
      
      
      
      pacf_data <- reactData$eda_dataset[,testCol]
      bacf <- stats::pacf(na.omit(pacf_data))
      bacfdf <- with(bacf, data.frame(lag, acf))
      
      pacf_plot <- ggplot2::ggplot(data = bacfdf, mapping = ggplot2::aes(x = lag, y = acf)) +
        ggplot2::geom_segment(lineend = "butt",mapping=ggplot2::aes(xend=lag,yend = 0), colour = 'blue') +
        ggplot2::geom_hline(yintercept = 0, color = 'blue') +
        ggplot2::geom_hline(
          yintercept = c(
            qnorm((1+.95)/2)/sqrt(length(na.omit(pacf_data))),
            -(qnorm((1 + .95)/2)/sqrt(length(na.omit(pacf_data))))
          ),
          color = "purple", linetype = "dashed"
        ) +
        ggplot2::ggtitle(paste("Partial Auto-Correlation Plot:",testCol)) + 
        ggplot2::xlab("Lag") +
        ggplot2::ylab("Partial Auto-Correlation") +
        ggplot2::scale_color_manual(values = c("orange", "purple")) +
        ggplot2::scale_fill_manual(values = c("#F8766D", "#00BFC4")) + 
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
      
      reactData$autocorrelation_output <- list(acf_plot,pacf_plot)
      
      output$autocorrelation_output <- shiny::renderUI({
        list(shiny::column(6,shiny::renderPlot(acf_plot)),shiny::column(6,shiny::renderPlot(pacf_plot)))
      })    
    },error=function(e)shiny::showNotification(e[1]))        
  })
  
  
  shiny::observeEvent(input$spectral_test_submit,{
    tryCatch({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing.....")
      
      testCols <- input$spectral_columns
      
      if(length(testCols)==0){
        shiny::showNotification('Add columns',type='warning')
        return()
      }
      
      ret_spec <- stats::spectrum(ts(na.omit(reactData$eda_dataset[,testCols])))
      ret_spec_df <- with(ret_spec, data.frame(freq, spec))
      
      #### Changed the plot to line from bar #####
      spectrum <- ggplot2::ggplot(data = ret_spec_df, mapping = ggplot2::aes(x = freq, y = spec)) +
        ggplot2::geom_segment(lineend = "butt",mapping = ggplot2::aes(xend=freq,yend=0),color='blue') +
        ggplot2::geom_hline(yintercept = 0,  color = "blue") + 
        
        # geom_col(color='blue') +
        ggplot2::ggtitle(paste0("Spectral Analysis of ",testCols)) +
        ggplot2::xlab("Frequency") + 
        ggplot2::ylab("Spectral Intensity") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
      
      
      reactData$spectral_output <- spectrum
      
      output$spectral_output <- shiny::renderPlot({
        spectrum
      })
    },error=function(e)shiny::showNotification(e[1]))
  })
  
  
  shiny::observeEvent(input$cointegration_test_submit,{
    tryCatch({ 
      testFunc <- input$cointegration_func_name
      testCols <- input$cointegration_columns
      reactData$cointegration_func_name <- testFunc
      reactData$cointegration_columns <- testCols
      
      if(length(testCols)<2){
        shiny::showNotification('Cointegration test requires atleast 2 columns',type='warning')
        return()
      }
      
      if(testFunc=='Engle-Granger Method'){
        require("egcm")
        eg <- egcm::egcm(reactData$eda_dataset[,testCols])
        reactData$cointegration_test_output <- eg
        
        output$cointegration_output <- shiny::renderPrint({
          print('##########################',quote=FALSE)
          print('   Engle-Granger Method   ',quote=FALSE)
          print('##########################',quote=FALSE)
          print('',quote=FALSE)
          print(eg)
        })
      }
      else if(testFunc=='Johansen Procedure')
      {
        require("urca")
        coRes <- urca::ca.jo(reactData$eda_dataset[,testCols],type="trace",K=2, ecdet="none", spec="longrun")
        reactData$cointegration_test_output <- coRes
        
        output$cointegration_output <- shiny::renderPrint({
          print('##########################',quote=FALSE)
          print('    Johansen-Procedure    ',quote=FALSE)
          print('##########################',quote=FALSE)
          print('',quote=FALSE)
          print(paste0('Test type : ',coRes@type,' ,',coRes@model),quote=FALSE)
          print(paste0('Eigenvalues (lambda):'),quote=FALSE)
          print(coRes@lambda,quote=FALSE)
          print('Values of test statistic and critical values of test:',quote=FALSE)
          print(cbind(coRes@teststat,coRes@cval),quote=FALSE)
        })
      }
      
    },error=function(e)shiny::showNotification(e[1]))
  })
  
  
  shiny::observeEvent(input$correlation_test_submit,{
    tryCatch({
      testCols <- input$correlation_columns
      
      if(length(testCols)<2){
        shiny::showNotification('Correlation check requires atleast 2 columns',type='warning')
        return()
      }
      
      # getCorrMat <- function(dataset, methodused = "everything"){
      #   cormat <-base::round(cor(dataset, use = methodused),3)
      #   return(cormat)
      # }
      
      computePairwiseCCF <- function(dataset){
        
        combinations.of.columns <- combn(colnames(dataset), m=2)
        
        pairwise.ccfs <- apply(combinations.of.columns, 2,
                               FUN = function(x){ccf(dataset[,x[1]], dataset[,x[2]], plot = FALSE)})
        
        lag.values <- pairwise.ccfs[[1]]$lag
        pairwise.ccfs <- data.frame(matrix(unlist(lapply(pairwise.ccfs, FUN = function(x){x$acf})),
                                           ncol = length(pairwise.ccfs),
                                           byrow = F))
        rownames(pairwise.ccfs) <- lag.values
        colnames(pairwise.ccfs) <- unlist(apply(combinations.of.columns, 2, FUN = function(x){paste(x[1],",",x[2])}))
        
        return(pairwise.ccfs)
        
      }
      
      getCorrPlot <- function(cross.correlation){
        
        plot <- ggplot2::ggplot(data = cross.correlation$cross.corr, mapping = ggplot2::aes(x = lags, y = corr)) +
          ggplot2::geom_segment(lineend = "butt", mapping = ggplot2::aes(xend = lags, yend = 0),colour = 'blue') +
          ggplot2::geom_hline(yintercept = 0, color ='blue') +  
          ggplot2::geom_hline(
            yintercept = c(
              stats::qnorm((1 + 0.95)/2)/sqrt(cross.correlation$nrow), 
              -(stats::qnorm((1 + .95)/2)/sqrt(cross.correlation$nrow))
            ),
            color = "purple", linetype = "dashed"
          ) +
          ggplot2::ggtitle(paste("Cross-Correlation Plot: ",cross.correlation$columns)) + 
          ggplot2::xlab("Lag") +
          ggplot2::ylab("Cross-Correlation") +  
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
        
        return(plot)
      }
      
      numCols <- unlist(sapply(reactData$eda_dataset[,testCols,drop=F],is.numeric))
      numCols <- testCols[numCols]
      
      # corMat <- getCorrMat(reactData$eda_dataset[,numCols],"pairwise.complete.obs")
      corMat <- computePairwiseCCF(reactData$eda_dataset[,numCols])
      
      cross.corr.list <- lapply(1:ncol(corMat), FUN = function(i){list(cross.corr = data.frame(lags = as.numeric(rownames(corMat)), corr = as.vector(corMat[,i])),
                                                                       columns = colnames(corMat)[i], nrow = nrow(reactData$eda_dataset[,numCols])
      )})
      print(cross.corr.list)
      cross.corr.plot.list <- lapply(cross.corr.list, FUN =  getCorrPlot)
      
      plotObj <- do.call(gridExtra::arrangeGrob, c(cross.corr.plot.list,ncol=1))
      
      
      reactData$correlation_output <- plotObj
      reactData$cross_corr_plot_height <- 200*length(cross.corr.plot.list)
      print(plotObj)
      
      output$corr_output <- shiny::renderPlot({
        gridExtra::grid.arrange(plotObj,ncol=1)
      })
      
      output$correlation_output <- shiny::renderUI({
        plotOutput("corr_output",height = 200*length(cross.corr.plot.list))})
    },error=function(e)shiny::showNotification(e[1]))
  })
  
  output$eda_panel_ui <- shiny::renderUI({
    tryCatch({
      addHieObs <- function(suffix){
        # level1obs <- paste0('level1obs',suffix)
        
        if(exists(paste0(suffix,'1obs'),where=1)) get(paste0(suffix,'1obs'))[['destroy']] # view_level1obs$destroy()
        if(exists(paste0(suffix,'2obs'),where=1)) get(paste0(suffix,'2obs'))[['destroy']] # view_level2obs$destroy() 
        if(exists(paste0(suffix,'3obs'),where=1)) get(paste0(suffix,'3obs'))[['destroy']] # view_level3obs$destroy()
        if(exists(paste0(suffix,'4obs'),where=1)) get(paste0(suffix,'4obs'))[['destroy']] # view_level4obs$destroy()
        if(exists(paste0(suffix,'5obs'),where=1)) get(paste0(suffix,'5obs'))[['destroy']] # view_level4obs$destroy()
        
        n <- length(reactData$HierarchyColumns)-1
        if(n==0) return()
        
        # Return a list of observe events for selection inputs for each level of data
        obsList <- lapply(c(1:n),function(i){  
          thisLevel <- i
          assign(paste0(suffix,thisLevel,'obs'),
                 shiny::observeEvent(input[[paste0(suffix,thisLevel)]],{
                   tryCatch({
                     nextLevel <- thisLevel + 1
                     nextLevelStr <- paste0(suffix,thisLevel+1)
                     req(input[[nextLevelStr]])
                     if(is.null(input[[nextLevelStr]])) return()
                     
                     hierarchy_columns <- reactData$HierarchyColumns
                     selectColumns <- hierarchy_columns[1:(thisLevel)]
                     inputValues <- c()
                     
                     for(j in 1:length(selectColumns))
                       inputValues[j] <- input[[paste0(suffix,j)]]
                     
                     filterStr <- paste(paste0(selectColumns,'==',shQuote(inputValues)),collapse=' & ')
                     
                     if(length(hierarchy_columns)==1)
                       return()
                     
                     changeColumn <- hierarchy_columns[thisLevel+1]
                     dat <-data.frame(reactData$processed_dataset,stringsAsFactors = F)
                     
                     cmd <- paste0("unique(subset(dat,",filterStr,",select = ",changeColumn,"))")
                     
                     filterDat <-eval(parse(text=cmd))
                     shiny::updateSelectInput(session,nextLevelStr,choices=filterDat[,1],selected=filterDat[,1][1])
                     shiny::updateCheckboxInput(session,'normalize_checkbox',value=0)
                     
                   },error=function(e) shiny::showNotification(e[1],type='error'))
                 }, ignoreInit = TRUE),pos=1)
        })
        
        return(obsList)
        
      }
      HierarchyColumns <- reactData$HierarchyColumns
      if(is.null(HierarchyColumns) | length(HierarchyColumns)==0 | input$hierarchy_checkbox==0) return()
      
      nHie <- length(HierarchyColumns)
      
      uiList <- lapply(c(1:nHie),function(i){
        hCol <- HierarchyColumns[i]
        choices <- unique(reactData$processed_dataset[,hCol])
        shiny::column(3,shiny::selectInput(paste0('level_',i),paste0('Select ',hCol,' Value'),choices= choices,selected=choices[1]))
      })
      
      addHieObs('level_')
      
      uiList <- tagList(tags$h4('Panel Selection'),shiny::fluidRow(class='box',uiList,shiny::column(2,class='button_pad',shiny::actionButton('eda_panel_submit','Submit'))))
    },error=function(e)shiny::showNotification(e[1],type='error'))
  })
  
  
  
  shiny::observeEvent(input$eda_panel_submit,{
    tryCatch({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      hierarchy_columns <- reactData$HierarchyColumns
      level_inputs <- unlist(lapply(c(1:length(hierarchy_columns)),function(i) input[[paste0('level_',i)]]))
      filter_string  <- paste(paste(hierarchy_columns,'==',shQuote(level_inputs)),collapse=' & ')
      reactData$eda_dataset <- reactData$processed_dataset %>% dplyr::filter_(filter_string)
    },error=function(e) shiny::showNotification(paste0('EDA panel selection : ',e[1]),type='error'))
    shiny::showNotification('Dataset filtered based on provided inputs')
  })
  
  
  
  addHieObservers <- function(suffix){
    # level1obs <- paste0('level1obs',suffix)
    
    if(exists('level1obs',where=1)) level1obs$destroy()
    if(exists('level2obs',where=1)) level2obs$destroy()
    if(exists('level3obs',where=1)) level3obs$destroy()
    if(exists('level4obs',where=1)) level4obs$destroy()
    
    n <- length(reactData$HierarchyColumns)-1
    
    # Return a list of observe events for selection inputs for each level of data
    obsList <- lapply(c(1:n),function(i){  
      thisLevel <- i
      assign(paste0('level',thisLevel,'obs'),
             shiny::observeEvent(input[[paste0('level_',thisLevel)]],{
               tryCatch({
                 nextLevel <- thisLevel + 1
                 nextLevelStr <- paste0('level_',thisLevel+1)
                 req(input[[nextLevelStr]])
                 if(is.null(input[[nextLevelStr]])) return()
                 
                 hierarchy_columns <- reactData$HierarchyColumns
                 selectColumns <- hierarchy_columns[1:(thisLevel)]
                 inputValues <- c()
                 
                 for(j in 1:length(selectColumns))
                   inputValues[j] <- input[[paste0('level_',j)]]
                 
                 filterStr <- paste(paste0(selectColumns,'==',shQuote(inputValues)),collapse=' & ')
                 changeColumn <- hierarchy_columns[thisLevel+1]
                 dat <-data.frame(reactData$processed_dataset,stringsAsFactors = F)
                 
                 cmd <- paste0("unique(subset(dat,",filterStr,",select = ",changeColumn,"))")
                 filterDat <-eval(parse(text=cmd))
                 
                 shiny::updateSelectInput(session,nextLevelStr,choices=filterDat[,1])
                 
               },error=function(e)shiny::showNotification(e[1]))
             }, ignoreInit = TRUE),pos=1)
    })
    
    return(obsList)
    
  }
  
  output$tspui1 <- shiny::renderUI({
    fit <- reactData[['tsp1']]
    if(is.null(fit)) return()
    
    TimeVar <- attr(fit,'TimeVar')
    x <- do.call(cbind,lapply(fit,function(item){item$x}))
    trend <- do.call(cbind,lapply(fit,function(item){item$trend}))
    seasonal <- do.call(cbind,lapply(fit,function(item){item$seasonal}))
    random <- do.call(cbind,lapply(fit,function(item){item$random}))
    names(x) <- names(trend) <- names(seasonal) <- names(random) <- names(fit)
    
    x <- xts::as.xts(data.frame(x),order.by=TimeVar)
    trend <- xts::as.xts(data.frame(trend),order.by=TimeVar)
    seasonal <- xts::as.xts(data.frame(seasonal),order.by=TimeVar)
    random <- xts::as.xts(data.frame(random),order.by=TimeVar)
    
    reactData$tsDecompCharts <- htmltools::browsable(
      tagList(
        dygraphs::dygraph(x,group='a',main='Actual Series',height = '200px',width = '90%') %>% dygraphs::dyOptions(gridLineColor = "#E7E7E7"),
        dygraphs::dygraph(trend,group='a',main='Trend Component',height = '200px',width = '90%') %>% dygraphs::dyOptions(gridLineColor = "#E7E7E7"),
        dygraphs::dygraph(seasonal,group='a',main='Seasonal Component',height = '200px',width = '90%') %>% dygraphs::dyOptions(gridLineColor = "#E7E7E7"),
        dygraphs::dygraph(random,group='a',main='Remainder Component',height = '200px',width = '90%') %>% dygraphs::dyOptions(gridLineColor = "#E7E7E7")
      ))
    
  })
}


viewTimeSeries <- function(input, output, session, reactData){
  output$ViewTimeSeries <- shiny::renderUI({
    if (is.null(reactData$AnomalyVar)){
      choice_list <- setdiff(colnames(reactData$processed_dataset),c(reactData$TimeVar,reactData$HierarchyColumns))
    } else {
      choice_list <- reactData$AnomalyVar
    }
    list(tagList(#shiny::fluidRow(style='padding:10px 0px 10px',
      shiny::fluidRow(class='box',
               conditionalPanel('input.hierarchy_checkbox',uiOutput('panel_ui')),
               shiny::column(3,shiny::selectInput('viewtsColSel0','Select Time Series Column',choices=choice_list,multiple=F)),
               shiny::column(class='button_pad',2,shiny::actionButton('viewtsColSubmit0','Submit'))
      )
      ,shiny::fluidRow(class='row_box row_pad',shiny::column(12,dygraphs::dygraphOutput('last_level_plots')))))
  })
  
  output$panel_ui <- shiny::renderUI({
    tryCatch({
      addHieObs <- function(suffix){
        # level1obs <- paste0('level1obs',suffix)
        
        if(exists(paste0(suffix,'1obs'),where=1)) get(paste0(suffix,'1obs'))[['destroy']] # view_level1obs$destroy()
        if(exists(paste0(suffix,'2obs'),where=1)) get(paste0(suffix,'2obs'))[['destroy']] # view_level2obs$destroy() 
        if(exists(paste0(suffix,'3obs'),where=1)) get(paste0(suffix,'3obs'))[['destroy']] # view_level3obs$destroy()
        if(exists(paste0(suffix,'4obs'),where=1)) get(paste0(suffix,'4obs'))[['destroy']] # view_level4obs$destroy()
        if(exists(paste0(suffix,'5obs'),where=1)) get(paste0(suffix,'5obs'))[['destroy']] # view_level4obs$destroy()
        
        n <- length(reactData$HierarchyColumns)-1
        if(n==0) return()
        
        # Return a list of observe events for selection inputs for each level of data
        obsList <- lapply(c(1:n),function(i){  
          thisLevel <- i
          assign(paste0(suffix,thisLevel,'obs'),
                 shiny::observeEvent(input[[paste0(suffix,thisLevel)]],{
                   tryCatch({
                     nextLevel <- thisLevel + 1
                     nextLevelStr <- paste0(suffix,thisLevel+1)
                     req(input[[nextLevelStr]])
                     if(is.null(input[[nextLevelStr]])) return()
                     
                     hierarchy_columns <- reactData$HierarchyColumns
                     selectColumns <- hierarchy_columns[1:(thisLevel)]
                     inputValues <- c()
                     
                     for(j in 1:length(selectColumns))
                       inputValues[j] <- input[[paste0(suffix,j)]]
                     
                     filterStr <- paste(paste0(selectColumns,'==',shQuote(inputValues)),collapse=' & ')
                     
                     if(length(hierarchy_columns)==1)
                       return()
                     
                     changeColumn <- hierarchy_columns[thisLevel+1]
                     dat <-data.frame(reactData$processed_dataset,stringsAsFactors = F)
                     
                     cmd <- paste0("unique(subset(dat,",filterStr,",select = ",changeColumn,"))")
                     
                     filterDat <-eval(parse(text=cmd))
                     shiny::updateSelectInput(session,nextLevelStr,choices=filterDat[,1],selected=filterDat[,1][1])
                     updateCheckboxInput(session,'normalize_checkbox',value=0)
                     
                   },error=function(e) shiny::showNotification(e[1],type='error'))
                 }, ignoreInit = TRUE),pos=1)
        })
        
        return(obsList)
        
      }
      HierarchyColumns <- reactData$HierarchyColumns
      if(is.null(HierarchyColumns) | length(HierarchyColumns)==0) return()
      
      nHie <- length(HierarchyColumns)
      
      uiList <- lapply(c(1:nHie),function(i){
        hCol <- HierarchyColumns[i]
        choices <- unique(isolate(reactData$processed_dataset)[,hCol]) # Dependency only on reactData$HierarchyColumns
        shiny::column(3,shiny::selectInput(paste0('view_level_',i),paste0('Select ',hCol,' Value'),choices= choices,selected=choices[1],multiple = ifelse(i==nHie,TRUE,FALSE)))
      })
      
      addHieObs('view_level_')
      
      uiList <- tagList(uiList,shiny::column(3,style='padding-top:10px',checkboxInput('show_all_ts_check',paste0("Check this box to select all sub-series under ",shQuote(HierarchyColumns[nHie])," "))),
                        shiny::column(12,checkboxInput('normalize_checkbox','Normalize values (z-scores)')))
      uiList
    },error=function(e)shiny::showNotification(paste0('EDA panel creation : ',e[1]),type='error'))
  })
  
  
  
  
  shiny::observeEvent(input$show_all_ts_check,ignoreNULL = TRUE,{
    tryCatch({
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      print('Selecting all lower nodes .....')
      #last_but_level_data <- get_viewts_data()
      TimeVar <- reactData$TimeVar
      selected_time_series <- input$viewtsColSel0
      all_hierarchy_columns <- reactData$HierarchyColumns
      nHie <- length(all_hierarchy_columns)
      hierarchy_columns <- all_hierarchy_columns[-nHie]
      last_level_selection <- input[[paste0('view_level_',nHie)]]  
      last_level <- all_hierarchy_columns[nHie]
      normalize <- input$normalize_checkbox
      level_inputs <- unlist(lapply(c(1:length(hierarchy_columns)),function(i) input[[paste0('view_level_',i)]]))
      
      # dplyr::filter_string  <- paste(paste(hierarchy_columns,'==',shQuote(level_inputs)),collapse=' & ')
      # last_but_level_data <- reactData$processed_dataset %>% dplyr::filter_(dplyr::filter_string)  %>% dplyr::select_(last_level,TimeVar,selected_time_series)
      if(length(hierarchy_columns)!=0){
        filter_string  <- paste(paste(hierarchy_columns,'==',shQuote(level_inputs)),collapse=' & ')
        last_but_level_data <- reactData$processed_dataset %>% dplyr::filter_(filter_string)  %>% dplyr::select_(last_level,TimeVar,selected_time_series)
      } else {
        last_but_level_data <- reactData$processed_dataset  %>% dplyr::select_(last_level,TimeVar,selected_time_series)
      } 
      
      last_level_values <- unique(last_but_level_data[,last_level])
      
      if(input$show_all_ts_check){
        if(length(last_level_values)>100){
          shiny::showNotification('More than 100 values selected. Restricting to 100 selections. Updating the UI might take some time.',type='warning')
          shiny::updateSelectInput(session,paste0('view_level_',nHie),selected = last_level_values[1:100])
          return()
        }
        shiny::updateSelectInput(session,paste0('view_level_',nHie),selected = last_level_values)
      } else {
        shiny::updateSelectInput(session,paste0('view_level_',nHie),selected = last_level_values[1])
      }
      
      
    },error=function(e)shiny::showNotification(e[1]))
  })
  
  
  shiny::observeEvent(input$viewtsColSubmit0,{
    tryCatch({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing...")
      
      all_hierarchy_columns <- reactData$HierarchyColumns
      TimeVar <- reactData$TimeVar
      selected_time_series <- input$viewtsColSel0
      reactData$viewtsColSel0 <- input$viewtsColSel0
      
      
      if(is.null(all_hierarchy_columns) | length(all_hierarchy_columns)==0 | input$hierarchy_checkbox==0){
        ## UI for Normal non-hierarchical time series data ---------
        
        tryCatch({
          data <- reactData$processed_dataset[,c(reactData$TimeVar,selected_time_series)] 
          xtsObj <- xts::as.xts(data[,c(selected_time_series),drop=FALSE],order.by=data[,reactData$TimeVar])
          
          output$last_level_plots <- dygraphs::renderDygraph({
            reactData$viewtsChart <- dygraphs::dygraph(xtsObj,main=selected_time_series) %>% dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.3,hideOnMouseOut = T) %>% dygraphs::dyOptions(gridLineColor = "#E7E7E7")
          })
          
        },error=function(e)shiny::showNotification(e[1],type='error'))
        
        
      } else {
        ## UI for Hierarchical time series data ----------------------
        nHie <- length(all_hierarchy_columns)
        hierarchy_columns <- all_hierarchy_columns[-nHie]
        last_level_selection <- input[[paste0('view_level_',nHie)]]  
        last_level <- all_hierarchy_columns[nHie]
        normalize <- input$normalize_checkbox
        level_inputs <- unlist(lapply(c(1:length(hierarchy_columns)),function(i) input[[paste0('view_level_',i)]]))
        
        reactData$normalize <- input$normalize_checkbox
        reactData$level_inputs <- level_inputs
        
        
        tryCatch({
          if(length(hierarchy_columns)!=0){
            filter_string  <- paste(paste(hierarchy_columns,'==',shQuote(level_inputs)),collapse=' & ')
            last_but_level_data <- reactData$processed_dataset %>% dplyr::filter_(filter_string)  %>% dplyr::select_(last_level,TimeVar,selected_time_series)
          } else {
            last_but_level_data <- reactData$processed_dataset  %>% dplyr::select_(last_level,TimeVar,selected_time_series)
          } 
          
          data_for_plot <- reshape2::dcast(formula=paste(TimeVar,'~',last_level),data=last_but_level_data,value.var = selected_time_series)
          time_data_for_plot <- data_for_plot[,TimeVar]
          data_for_plot <- data_for_plot[,-1][,last_level_selection]
          
          if(normalize)
            data_for_plot <- scale(data_for_plot,center = TRUE,scale = TRUE)
          
          xtsObj <- xts::as.xts(data_for_plot,order.by=time_data_for_plot)
          
          output$last_level_plots <- dygraphs::renderDygraph({
            reactData$viewtsChart <- dygraphs::dygraph(xtsObj,main=selected_time_series)  %>% dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.3,hideOnMouseOut = T) %>% dygraphs::dyOptions(gridLineColor = "#E7E7E7")
          })
          
        },error=function(e) shiny::showNotification(e[1],type='error'))
        
      }
    },error=function(e)shiny::showNotification(e[1],type='error'))
  })
  
  
  
  
  
 
  
  
}