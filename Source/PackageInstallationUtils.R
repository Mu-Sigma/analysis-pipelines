#######################################################################################################################
# Package Installation Script
# Souvik 
# Version 1.04
# Script for easy installation of Foundation Bricks
# * Added script for perfroming package dependency check
#######################################################################################################################


#######################################################################################################################
# Function to check if the installed version of Java is 1.8 to match system requirements of 'rJava' Package
# Inputs :
#     verbose: Set to FALSE by default. Else, if set as TRUE, returns additional details about 
#              version of Java and R which has been called inside the function
#     minJavaVersion: minimum Java version required
# 
# Output : Logical TRUE/FALSE output
#     TRUE: If the installed version of Java is 1.8
#     FALSE: If the installed version of Java is not 1.8     
#######################################################################################################################
CheckJavaVersion <- function(minJavaVersion, verbose = F){
  tryCatch({
  OSType <- Sys.info()['sysname']
  
  # Extracts java -version on a Linux environment
  if(OSType == 'Linux'){
    #Extracting java -version
    sysinfo <- system("java -version 2> tmp.txt")
    sysinfo <- readLines("tmp.txt")
    sysinfo <- substr(strsplit(sysinfo, " ")[[1]][3], 2, 4)
    
    # Extract java -version on a Windows environment
  }else if(OSType == 'Windows'){
    #Extracting java -version 
    sysinfo <- system2("java", "-version", stdout = 'stdout.txt', stderr = 'stderr.txt')
    sysinfo <- readLines('stderr.txt')
    sysinfo <- substr(strsplit(sysinfo, " ")[[1]][3], 2, 4)
    
    # Extracting java -version on a Macintosh
  }else if(OSType == 'Darwin'){
    # Extracting java -version
    sysinfo <-  system("java -version 2> tmp.txt")
    sysinfo <- readLines("tmp.txt")
    sysinfo <- substr(strsplit(sysinfo, " ")[[1]][3], 2, 4)
    
  }else{
    return(FALSE)
  }
  if(verbose == T){
    base::system("java -version")
  }
  if(file.exists('tmp.txt')){
    file.remove("tmp.txt")
  }
  if(file.exists('text.txt')){
    file.remove("text.txt")
  }
  if(file.exists('stderr.txt')){
    file.remove("stderr.txt")
  }
  if(file.exists('stdout.txt')){
    file.remove("stdout.txt")
  }
  
  # Check if the version of Java is 1.8
  if(sysinfo >= minJavaVersion){
    return(TRUE)
  }else{
    warning('minimum version requirements for Java not met')
    return(FALSE)
  }
  }, error = function(e){
    warning(e) 
    return(FALSE)
  })

}

javaHomeExists <- function(){
  if(length(Sys.getenv("JAVA_HOME")) == 1){
    return(dir.exists(Sys.getenv("JAVA_HOME")))
  }else{
    return(TRUE)
  }
}

#######################################################################################################################
# Function to check if the installed version of R is 3.4.3 to match requirements for Foundation Bricks
# Inputs : 
#     minVersion: Minimum R's version  
# 
# Output : 
#     returns R version if it's 3.4.3
#######################################################################################################################


RVersionCheck <- function(minVersion) {
  result <- strsplit(R.Version()$version.string, " ")[[1]][3]
  
  if(result < minVersion){
    warning("minimum version requirement of r not met")
    return(FALSE)
  }else{
    return(TRUE)
  }
}

#######################################################################################################################
# Function to match architecture build of R and java installled on a system(32/64-bit) 
# Inputs: 
#     verbose: Set to FALSE by default. Else, if set as TRUE, returns additional details about 
#              version of Java and R which has been called inside the function
# 
# Output: Logical TRUE/FALSE output
#     TRUE: If the architecture of both R and Java matches(32/64-bit)
#     FALSE: If the architecture of both R and Java doesn't match(32/64-bit)     
#######################################################################################################################

isJavaRMatch <- function(){
  OSType <- Sys.info()['sysname']
  rArch <- R.Version()
  rArch <- substr(rArch$arch, nchar(rArch$arch)-1, nchar(rArch$arch))

  if(OSType == 'Windows'){
    tryCatch({
      javaArch <- system(paste0('java -d', rArch, ' -version'), intern = T)
      if(!is.null(attr(javaArch, 'status')) && attr(javaArch, 'status') == 1){
        warning("architecture for installed versions of Java and R doesn't match")
        print(FALSE)
      }else{
        print(TRUE) 
      }
    },error = function(e){
      warning("Architecture for the installed Versions of R and java don't match") 
      print(FALSE)
    })}
  else if(OSType == 'Linux'){
    tryCatch({
      javaArch <- system(paste0('java -d', rArch, ' -version'))
      if(javaArch == 0){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }, error = function(e){
      warning("Architecture for the installed Versions of R and java don't match") 
      return(FALSE)
    })
  }else{
    tryCatch({
      javaArch <- system(paste0('java -d', rArch, ' -version'))
      if(javaArch == 0){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }, error = function(e){
      warning("Architecture for the installed Versions of R and java don't match") 
      return(FALSE)
    })
  }
}


#######################################################################################################################
# Function which gives out a list of packages to be installed as dependencies during package instalation
# Inputs :
#     packageName: The package of interest
#     reqVersion: The required version of the package of interest 
#
# Output : 
#     A logical TRUE/FALSE output
#     TRUE: If version requirements have been met
#     FALSE: If version requirement are not met
#######################################################################################################################

checkPackageversion <- function(packageName, reqVersion){
  if(packageVersion(packageName) != reqVersion){
    warning(paste0("Version requirement of '", packageName, "' package not met. Version '", reqVersion, "' is required."))
    return(FALSE)
  }else{
    return(TRUE)
  }
}

#######################################################################################################################
# Function which gives out a list of packages to be installed as dependencies during package instalation
# Inputs :
#     packageList: A list of packages
#
# Output : 
#     The list of dependent packages which is required to be installed before the list of input packages
#######################################################################################################################

PackageDependencyList <- function(packageList){
  
  if(class(packageList) != "character"){
    packageList <- as.character(packageList)
  }
  
  if(length(packageList) == 0){
    stop('input parameter is empty')
  }
  
  # Create a graph object to containing package dependency hierarchy
  dependency.check <- miniCRAN::makeDepGraph(packageList, suggests = FALSE, includeBasePkgs = FALSE)
  
  # Iterates over each element of the object packageList and gives out the individual package dependencies for each of these
  # Elements of packageList 
  result <- list()
  for(i in 1:length(packageList)){
    result[[i]] <- (igraph::topo_sort(dependency.check))}
  
  # For each element in the list, numbers are replaced with package names, remove NAs and reverse the order to get the correct order
  result <- lapply(result, function(x){
    x <- igraph::as_ids(x)
  })
  result <- Reduce(c, result) #append all the lists into one list
  result <- result[!duplicated(result)] #remove duplicates
  
  return(result) 
}


#######################################################################################################################
# Function to load and install packages from a list of packages
# Ref: https://stackoverflow.com/questions/8175912/load-multiple-packages-at-once 
# Inputs: 
#     packageList: Variable to where a list of packages have been saved
# 
# Output: Logical TRUE/FALSE output
#     TRUE: If all the packages' from the list was installed and loaded successfully against each package's name
#     FALSE: If all/one of the packages' from the list couldn't be instaled and loaded successfully 
#            against each one's name   
#######################################################################################################################

loadpackage <- function(packageList){
  # #Extracts the names of all the packages from the installed.packages
  # toInstallPackages <- packageDepList[!(packageDepList %in% installed.packages()[, "Package"])]
  failedPackages <- c()
  if(is.null(packageList) || length(packageList)==0)
    return(NULL)
  # tryCatch({
  #   packageDepList <<- PackageDependencyList(packageList)
  # }, error = function(e){
  #   packageDepList <<- packageList
  # }, warning = function(w){
  #   packageDepList <<- packageList
  # })
  #Installs and loads packages not there in installed.packages
  
  
  for(i in 1:length(packageList)){
    if(!(packageList[i] %in% installed.packages()[,1]) == TRUE){
      install.packages(packageList[i], dependencies = FALSE, repos = "http://cloud.r-project.org/")
    }
    if(!(packageList[i] %in% installed.packages()[,1]) == TRUE){
      warning(paste0(packageList[i], ": installation failed"))
      failedPackages <- c(failedPackages, packageList[i])
    }
  }
  if(length(failedPackages)>0){
  return(failedPackages)
  }
  else{
    return(TRUE)
  }
}
#######################################################################################################################
