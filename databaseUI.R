
databaseUI<- function(input,output,session){
  connectionObj <<- NULL
  dbChoices <<- c(SQLite = "sqlite_db", MySQL = "mysql_db", 
                  PostgreSQL = "postgres", Oracle = "oracle", `SQL server` = "sql_server", 
                  Teradata = "teradata", Netezza = "netezza", Hive = "hive", 
                  Other = "other")
  odbcDbChoices <<- c(Hive = "hive",Teradata = "teradata", `SQL server` = "sql_server", Other = "other")
  
  dbPackages <<- list(  'rJava'
                        ,'RPostgreSQL'
                        ,'RMySQL'
                        ,'RSQLite'
                        ,'DBI'
                        ,'RJDBC'
                        , 'odbc')
  
  output$dbUI<- shiny::renderUI({
    shiny::fluidRow(
      
      shiny::column(4,shiny::radioButtons("connectionMethod", "Choose connection method",choices=c("JDBC"="jdbc","ODBC"="odbc"),selected="jdbc",inline=TRUE))
      ,shiny::column(4,shiny::radioButtons("credMethod", "Choose login method",choices=c("Credentials"="cred","JDBC String"="jdbcString"),selected="cred",inline=TRUE))
      ,shiny::column(3,align="right",shiny::tags$div(shiny::HTML('<b>Config:</b>')),shiny::actionButton('loadConfig','Load'),shiny::actionButton('saveConfig','Save'))
      ,shiny::tags$br()
      ,shiny::div(id="jdbcSelect",shiny::column(12,shiny::selectInput('dbButton1','Select Database',choices=dbChoices,selected = 'postgres')))
      # ,shiny::div(id="odbcSelect",shiny::column(12,shiny::selectInput('dbButton2','Select Database',choices=odbcDbChoices,selected = 'postgres')))
      ,shiny::div(id='sqlitePath',shiny::column(12,shiny::textInput('dbName1','Enter Complete Path to Database')))
      ,shiny::div(id='dsnString',shiny::column(12,shiny::textInput('connectionString1','DSN')))
      ,shiny::div(id="loadSettings",shiny::column(4,shiny::textInput('dbHost','Host',value='', placeholder= 'example@server.com [OR] IP Address'))
           ,shiny::column(2,shiny::textInput('dbPort','Port'))
           ,shiny::column(5, shiny::textInput('dbName','DB Name',value='', placeholder = 'Database Name')))
      ,shiny::div(id='JDBCConnString',style="display:none;",shiny::column(12,shiny::tagList(shiny::textInput('connectionString','Enter JDBC Connection String'),shiny::tags$p(shiny::HTML('E.g. jdbc:mysql://127.0.0.1:5432/mydatabase')))))
      ,shiny::div(id='loadSettings2',shiny::column(6,shiny::textInput('dbUsername','Username',value='', placeholder = 'Username')),shiny::column(6,shiny::passwordInput('dbPassword','Password',value='')))
      ,shiny::div(id='driverOptionsDiv',shiny::column(12,shiny::checkboxInput('driverOptions','Load JDBC drivers manually')))
      ,shiny::div(id='manualDrivers',shiny::column(6,shiny::textInput('driverLocation','JDBC Driver Location')
                                     ,shiny::tags$div(shiny::HTML("E.g. C:/Users/Admin/Desktop/Folder_containing_jars/<br/> Use '/' for specifying location.")))
           ,shiny::column(6,shiny::textInput('driverClass','JDBC Driver Class')
                   ,shiny::tags$div(shiny::HTML('E.g. oracle.jdbc.OracleDriver'))))
      ,shiny::column(12,shiny::checkboxInput(inputId = 'useSsl',label='Use SSL Encryption',F))
      ,shiny::div(id='sslContent',shiny::column(6,shiny::fileInput('certification','Choose certification',multiple = F)),shiny::column(6,shiny::passwordInput('sslPassword','SSL Password',value='')))
      ,shiny::column(12,align='left',shiny::actionButton('srcSubmitButton2','Connect'))
      ,shiny::column(12,align='left',shiny::uiOutput('UIconn'))
      ,shiny::column(12,align='left',shiny::uiOutput('connIndicatorUI'))
      ,shiny::column(12,shiny::textInput('tableName1','Enter Primary Table Name/Query (Write table name or query space seperated by table name)',value='', placeholder="Primary Table Name"))
      ,shiny::column(12,shiny::actionButton('submitTable','Submit'))  
      )
  })
  
  shiny::observeEvent(input$srcSubmitButton2,{
    
    connectionParams <<- list( #---Store the connection paramters in global variable
      dbButton1 = input$dbButton1,
      dbName = input$dbName,
      dbName1 = input$dbName1,
      dbHost = input$dbHost,
      dbPort = as.integer(input$dbPort),
      dbUsername = input$dbUsername,
      dbPassword = input$dbPassword,
      dbTable = input$dbTable,
      dbDriverLocation = input$driverLocation,
      dbDriverClass = trimws(input$driverClass),
      driverOptions = input$driverOptions,
      certification = input$certification$datapath,
      useSsl=input$useSsl,
      sslPassword = input$sslPassword,
      connectionString = trimws(input$connectionString),
      connectionString1 = trimws(input$connectionString1),
      connectionMethod = trimws(input$connectionMethod),
      credMethod = input$credMethod)
    
    
    
    
    
    
    #print('Sourcing from Database')
    
    con <- connect(connectionParams)
    connFlag <<- TRUE
    
    
    
    
    if(is.null(con[[1]])){
      output$connIndicatorUI <- shiny::renderUI({shiny::tags$p(paste0('Error in Connection: ',con[[2]]),style="color:red;")})
      shiny::showNotification(con[[2]],type='error')
      return()
    }
    output$connIndicatorUI <- shiny::renderUI({shiny::tags$p('Connection Established',style="color:green;")})
    shiny::showNotification("Connection Established")
    connectionObj <<- con[[1]]
    
  })
  
  output$connIndicatorUI <- shiny::renderUI({})
  
  #--Close db connection on closing window--#
  shiny::observe({  
    js_string <- 'confirm("Are You Sure?");'
    session$sendCustomMessage(type='jsCode', list(value = js_string))
    
    cancel.onSessionEnded <- session$onSessionEnded(function() {
      
      if(!is.null(connectionObj)){
        #print('Dropping qcTempTable and qcTempTableManipulated Tables')
        
        #print('Closing DB connections')
        DBI::dbDisconnect(conList$con)
      }
    })
  })
  
  ##-- Update dbName input for oracle 
  shiny::observeEvent(input$dbButton1,{
    dbName <- input$dbButton1
    if(dbName=='oracle'){
      shiny::updateTextInput(session,'dbName','Service Name Or SID')
    } else {
      shiny::updateTextInput(session,'dbName','DB Name')
    }
  })
  shiny::observeEvent(input$connectionMethod,{
    connectionMethod <- input$connectionMethod
    if(connectionMethod=='jdbc'){
      shiny::updateSelectInput(session,'dbButton1','Select Database',choices=dbChoices,selected = 'postgres')
    } else {
      shiny::updateSelectInput(session,'dbButton1','Select Database',choices=odbcDbChoices,selected = 'hive')
    }
  })
  shiny::observeEvent(input$saveConfig,{
    
    configList <- list(
      dbButton1=input$dbButton1,
      dbName = input$dbName,
      dbName1 = input$dbName1,
      dbHost = input$dbHost,
      dbPort = as.integer(input$dbPort),
      dbUsername = input$dbUsername,
      dbDriverLocation = input$driverLocation,
      dbDriverClass = trimws(input$driverClass),
      driverOptions = input$driverOptions,
      connectionString = trimws(input$connectionString),
      connectionString1 = trimws(input$connectionString1),
      connectionMethod = trimws(input$connectionMethod)
      
    )
    saveRDS(configList,'Downloads/DBconfig.rds')
    shiny::showNotification('Configuration Saved')
  })
  
  #____Load saved config____#
  shiny::observeEvent(input$loadConfig,{
    #print("Loading last saved connection configuration")
    
    if(!file.exists('Downloads/DBconfig.rds')){
      shiny::showNotification('No Existing Configuration File Found!',type='error')
      return()
    }
    config <- readRDS('Downloads/DBconfig.rds')
    #print(c("Config: ", config))
    #print(input$loadConfig)
    # print("##############################################")
    # print(input$connectionMethod)
    # updateRadioButtons(session, 'connectionMethod', "Choose a connection method", 
    # choices = c("JDBC", "ODBC"), selected=toupper(config$connectionMethod))
    #print(input$connectionMethod)
    shiny::updateSelectInput(session,'dbButton1','Select Database',choices=showChoices(config$connectionMethod),selected=config$dbButton1)
    shiny::updateTextInput(session,'dbHost','Host',value=config$dbHost)
    shiny::updateTextInput(session,'dbPort','Port',value=config$dbPort)
    if(config$dbName=='oracle'){
      shiny::updateTextInput(session,'dbName','Serive Name Or SID',value=config$dbName)
    }
    else{
      shiny::updateTextInput(session,'dbName','DB Name',value=config$dbName)
    }
    shiny::updateTextInput(session,'dbUsername','Username',value=config$dbUsername)
    shiny::updateTextInput(session,'dbName1','Enter Complete Path to Database',value=config$dbName1)
    shiny::updateTextInput(session,'connectionString','Enter JDBC Connection String',value=config$connectionString)
    shiny::updateTextInput(session,'driverLocation','JDBC Driver Location',value=config$dbDriverLocation)
    shiny::updateTextInput(session,'connectionString1','DSN',value=config$connectionString1)
    shiny::updateTextInput(session,'driverClass','JDBC Driver Class',value=config$dbDriverClass)
    
    
    if(length(config$dbDriverClass)!=0)
      shiny::updateCheckboxInput(session,'driverOptions',value=1)
    shiny::updateTextInput(session,'dbPassword','Password',value='')
    shiny::showNotification('Configuration Loaded')
  })
  
}




dbExistsTableWrapper <- function(connectionObj, name, connDb,i){
  flag <- FALSE
  if(name=='') return(flag)
  alias <- LETTERS[i]
  conList <<- list(con=connectionObj,table=name,db=connDb)
  
  path <- NULL
  data <- name
  
  head <- dbFetchTopWrapper(conList)
  print(head)
  if(!is.null(head))
    flag <- TRUE
  return(flag)
}

dbFetchTopWrapper <- function(conList){
  
  fn <- function(){
    tryCatch(dbFetchTop(conList)
             ,error=function(e){message(e[1])})
  }
  dat <- fn()
}

dbTableSelectQuery <- function(conList){
  
  if(grepl(" ",conList$table)){
    
    return(gsub("\\s*\\w*$", "",conList$table))
  }
  else
  {
    query <- switch(conList$db#connectionParams$dbButton1
                    ,'oracle'={
                      paste('select * from',conList$table)
                    }
                    ,'mysql_db' = paste('select * from', conList$table)
                    ,'postgres' = paste('select * from',conList$table)
                    ,'sql_server' = paste('select * from', conList$table)
                    ,'teradata' = paste('select * from', conList$table)
                    ,paste('select * from', conList$table)
    )
    print(query)
    return(query)}
}

dbTableAll <- function(conList){
  query <- dbTableSelectQuery(conList)
  DBI::dbSendQuery(conList$con,query)
}

dbFetchAll <- function(conList){
  con <- conList$con
  if(!class(con)=="SQLiteConnection") clearResults(conList$con)
  res <- dbTableAll(conList)
  dat <- DBI::dbFetch(res)
  if(!class(con)=="SQLiteConnection") clearResults(conList$con)
  dat
}

dbTableTopQuery <- function(conList){
  
  n=10
  if(grepl(" ",conList$table)){
    return(gsub("\\s*\\w*$", "",conList$table))
  }
  else
  {
    query <- switch(conList$db#connectionParams$dbButton1
                    ,'oracle'={
                      paste('select * from',conList$table,' where rownum <=',n)
                    }
                    ,'mysql_db' = paste('select * from', conList$table,'limit',n)
                    ,'postgres' = paste('select * from',conList$table,'limit',n)
                    ,'sql_server' = paste('select * from', conList$table,'limit',n)
                    ,'teradata' = paste('select top',n,' * from', conList$table)
                    ,paste('select top',n,' * from', conList$table)
    )
    print("query")
    return(query)}
}


dbTableTop <- function(conList){
  
  query <- dbTableTopQuery(conList)
  
  DBI::dbSendQuery(conList$con,query)
}

dbFetchTop <- function(conList){
  
  con <- conList$con
  if(!class(con)=="SQLiteConnection") clearResults(conList$con)
  res <- dbTableTop(conList)
  dat <- DBI::dbFetch(res)
  if(!class(con)=="SQLiteConnection") clearResults(conList$con)
  dat
}
showChoices <- function(connectionMethod){
  if(tolower(connectionMethod == "jdbc")){
    return(dbChoices)
  }else{
    return(odbcDbChoices)
  }
} 

connect <- function(cParams){
  
  dbButton1 <- cParams$dbButton1
  dbName <- cParams$dbName
  dbName1 <- cParams$dbName1
  dbHost <- cParams$dbHost
  dbPort <- as.integer(cParams$dbPort)
  dbUsername <- cParams$dbUsername
  dbPassword <- cParams$dbPassword
  dbTable <- cParams$dbTable
  dbDriverLocation <- cParams$dbDriverLocation
  dbDriverClass <- trimws(cParams$dbDriverClass)
  driverOptions <- cParams$driverOptions
  certification <- cParams$certification
  sslPassword <- cParams$sslPassword
  useSsl <- cParams$useSsl
  connectionString <- trimws(cParams$connectionString)
  connectionString1 <- trimws(cParams$connectionString1)
  connectionMethod <- trimws(cParams$connectionMethod)
  credMethod <- cParams$credMethod
  
  if(tolower(connectionMethod) == "jdbc"){
    
    #print("Trying to establish JDBC connection...")
    
    tryCatch({
      if(driverOptions==1){
        cp <- list.files(dbDriverLocation, pattern = "[.]jar", full.names=TRUE, recursive=TRUE)
        
        if(useSsl && dbButton1=='hive'){
          .jaddClassPath(cp)
          driver <- RJDBC::JDBC("org.apache.hive.jdbc.HiveDriver", dbDriverClass)
        }else{
          driver <- RJDBC::JDBC(dbDriverClass,cp,identifier.quote='`')
        }
      } else {
        driver <-  switch(dbButton1
                          ,'sqlite_db'= RSQLite::SQLite()
                          ,'mysql_db'= RMySQL::MySQL()
                          ,'postgres'= RPostgreSQL::PostgreSQL()
                          ,NULL
        )
      }
      
      if(is.null(driver)){
        #shiny::showNotification('JDBC driver not available. Please provide the location of JDBC driver and Class manually')
        return(list(NULL,'JDBC driver not available. Please provide the location of JDBC driver and Class manually'))
      }
      
      #print('Loading DB driver')
      
      my_db <- switch(dbButton1
                      ,'sqlite_db' = DBI::dbConnect(driver,dbName1)
                      ,'mysql_db' = {
                        if(credMethod=='jdbcString'){
                          str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                          
                          con <- eval(parse(text=str))
                          con
                        }
                        else
                          DBI::dbConnect(driver,url='jdbc:mysql://',
                                    port=dbPort,host=dbHost,dbname=dbName,password=dbPassword,user=dbUsername)}
                      ,'postgres' = {
                        if(credMethod=='jdbcString'){
                          str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                          
                          con <- eval(parse(text=str))
                          con
                        }
                        else
                          DBI::dbConnect(driver,paste0("jdbc:postgresql://",''),
                                    port=dbPort,host=dbHost,dbname=dbName,password=dbPassword,user=dbUsername)}
                      ,'sql_server' = {
                        if(credMethod=='jdbcString'){
                          str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                          
                          con <- eval(parse(text=str))
                          con
                        }
                        else
                          DBI::dbConnect(drv=driver,url=paste0('jdbc:sqlserver://',dbHost,":",dbPort),user=dbUsername,password=dbPassword)
                        
                      }
                      ,'teradata' = {
                        if(credMethod=='jdbcString'){
                          str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                          
                          con <- eval(parse(text=str))
                          con
                        }
                        else
                          DBI::dbConnect(driver,paste0('jdbc:teradata://',dbHost,',DATABASE=',dbName), password=dbPassword,user=dbUsername)}
                      ,'oracle'= {
                        #require(ROracle)
                        # connect.string <- paste("(DESCRIPTION=","(ADDRESS=(PROTOCOL=tcp)(HOST=", dbHost, ")(PORT=", dbPort, "))"
                        #                       ,"(CONNECT_DATA=(SID=", sid, ")))", sep = "")
                        #con1 <- DBI::dbConnect(driver, user = dbUsername, password = dbPassword, dbname=connect.string)
                        print(connectionString)
                        if(credMethod=='jdbcString'){
                          str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                          print(str)
                          con <- eval(parse(text=str))
                          con
                        }
                        else{
                          con1 <- DBI::dbConnect(driver, url=paste0("jdbc:oracle:thin:@//",dbHost,":",dbPort,"/",dbName), dbUsername, dbPassword)
                          
                          con1}
                      }
                      ,'netezza'= {
                        if(credMethod=='jdbcString'){
                          str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                          
                          con <- eval(parse(text=str))
                          con
                        }
                        else
                          DBI::dbConnect(driver, paste0("jdbc:netezza://",dbHost,":",dbPort,"//",dbName), user=dbUsername, password=dbPassword)
                      }
                      ,'hive'= {
                        
                        if(credMethod=='jdbcString'){
                          str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                          
                          con <- eval(parse(text=str))
                          con
                        }
                        else{
                          if(useSsl){
                            connectionhost <- paste0('jdbc:hive2://',dbHost,':',dbPort,"/",dbName)
                            
                            SSLPath <- paste0(certification)
                            connectionString = paste0(connectionhost,';ssl=true;sslTrustStore=',SSLPath,';trustStorePassword=', sslPassword)
                            
                            #Final Connection String
                            con <- DBI::dbConnect(driver,connectionString ,dbUsername, dbPassword)
                            
                          }
                          else{
                            connectionhost <- paste0('jdbc:hive2://',dbHost,':',dbPort,"/",dbName)
                            str <- paste0("DBI::dbConnect(driver,url='",connectionhost,"',user='",dbUsername,"',password='",dbPassword,"')")
                            con <- eval(parse(text=str))
                          }
                          
                          con}
                      }
                      ,'other'= {
                        str <- paste0("DBI::dbConnect(driver,url='",connectionString,"',user='",dbUsername,"',password='",dbPassword,"')")
                        
                        con <- eval(parse(text=str))
                        con
                      }
      )
      
      #print("Success!")
      return(list(my_db,''))
    }
    ,error = function(e){
      #shiny::showNotification(paste('DB error:',e[1]),type='error');
      message(e[1]);
      return(list(NULL,paste('DB error:',e[1])))
    })
  }else{
    tryCatch({
      
      #print('Trying to establish ODBC connection...')
      
      my_db <- switch(dbButton1,
                      'hive'= {
                        con <- DBI::dbConnect(odbc::odbc(),
                                              dsn = connectionString1,
                                              Host   = dbHost,
                                              Schema = dbName,
                                              UID    = dbUsername,
                                              PWD    = dbPassword,
                                              Port   = dbPort)
                        con
                      }
                      ,'teradata'= {
                        # print(connectionString1)
                        con <- DBI::dbConnect(odbc::odbc(),
                                              dsn = connectionString1,
                                              Host   = dbHost,
                                              Schema = dbName,
                                              Port   = dbPort)
                        
                        con
                      }
                      ,'sql_server'= {
                        con <- DBI::dbConnect(odbc::odbc(),
                                              dsn = connectionString1,
                                              Host   = dbHost,
                                              Schema = dbName,
                                              UID    = dbUsername,
                                              PWD    = dbPassword)
                        con
                      }
                      ,'other'= {
                        str <- paste0("DBI::dbConnect(odbc::odbc(), dsn =\"",as.character(connectionString1),"\", uid =\"",dbUsername, "\", pwd =\"",dbPassword,"\")")
                        
                        con <- eval(parse(text=str))
                        con
                      }
      )
      #print("Success!")
      return(list(my_db,''))
    }
    ,error = function(e){
      #shiny::showNotification(paste('DB error:',e[1]),type='error');
      message(e[1]);
      return(list(NULL,paste('DB error:',e[1])))
    }
    )
  }
}


clearResults <- function(con){
  if(class(con)=='SQLiteConnection') return()
  if(is.null(DBI::dbGetInfo(con)$odbcdriver)){
    if(dbResultLen(con)){
      DBI::dbClearResult(DBI::dbListResults(con)[[1]])
    }
  }else{
    return()
  }
}


dbResultLen <- function(con){
  if(is.null(DBI::dbGetInfo(con)$odbcdriver)){
    return(length(DBI::dbListResults(con)))
  }
  else{
    return()
  }
}

