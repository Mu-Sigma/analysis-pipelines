expired <- FALSE

fireAlert <- function(input, output)
{
  output$popStartup_2<-renderUI({tags$div(class="whiteBox_2","")})
  output$popStartup<-renderUI({
    tagList(tags$div(class="whiteBox",
                     tags$p(id="headPopup","Caution! Files tampered or brick has expired!",style="color:red;"),
                     tags$hr(),
                     tags$p(id="bodyPopup", "Kindly re-download the brick for use!", style="color:red;font-size=18px"),
                     tags$hr(),
                     actionButton("closePopup","Close"))
    )
  })
  observeEvent(input$closePopup,{
    output$popStartup<-renderUI({})
    output$popStartup_2<-renderUI({})
  })
}


loadPackageInstall <- function()
{
  if (file.exists("Source/PackageInstallationUtils.R")) {
    source("Source/PackageInstallationUtils.R")
  }else if (file.exists("Source/PackageInstallationUtils.RData")) {
    load("Source/PackageInstallationUtils.RData", envir = .GlobalEnv)
  }else {
    stop("Files tampered or brick has expired!\nKindly re-download the brick for use!")
  }
}


loadFunctions <- function(input, output, session) 
{
  Rfiles <- 0
  RDatafiles <- 0
  # expiryMssg <- NULL
  tryCatch({
    filesToLoad <- c("EDAUtils", "databaseUI", "TimeSeries_EDA", "brickUsage", "reportIssue", "oldVersionPopup", "customPrecision","warningHeader")
    invisible(lapply(filesToLoad, function(x){
      if (file.exists(paste0("Source/", x, ".R"))) {
        source(paste0("Source/", x, ".R"))
        Rfiles <<- Rfiles + 1
      }
      if (file.exists(paste0("Source/", x, ".RData"))) {
        load(paste0("Source/", x, ".RData"), envir = .GlobalEnv)
        RDatafiles <<- RDatafiles + 1
      }
    }))
    loadGlobals()
    databaseUI(input,output,session)
    
    if(RDatafiles == length(filesToLoad)) {
      if (file.exists("Source/expiry.R")){
        source("Source/expiry.R")
      }else if (file.exists("Source/expiry.RData")) {
        load("Source/expiry.RData", envir = .GlobalEnv)
      }else{
        stop("Files tampered!\nKindly re-download the brick for use!")
      }
      defaultDeclarations()
      expiryMssg <<- checkExpiry()
      if (expiryMssg == "FIRST" || expiryMssg == "PASS") {
        expired <<- FALSE
      }
      else {
        expired <<- TRUE
      }
    } else if(RDatafiles>0 && RDatafiles<length(filesToLoad)) {
      expired <<- TRUE
      fireAlert(input, output)
    } else if(RDatafiles == 0 && Rfiles > 0 && Rfiles < length(filesToLoad)) {
      stop("Important files are missing from Source.\nKindly re-download the brick for use!")
    }
    else expired <<- FALSE
  }, error = function(e) {
    if(RDatafiles == 0 && Rfiles == 0){
      expired <<- TRUE
      fireAlert(input, output)
    }else if(RDatafiles < length(filesToLoad) && Rfiles < length(filesToLoad)){
      stop("Files tampered or brick has expired!\nKindly re-download the brick for use!")
    }else stop(e$message)
  })
}
