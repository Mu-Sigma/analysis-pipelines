#' @name setPythonEnvir
#' @export
setPythonEnvir <- function(type = 'conda', pathOrEnvirName = 'base'){
  tryCatch({
    if(type == 'conda'){
      reticulate::use_condaenv(pathOrEnvirName, required = T)
      futile.logger::flog.info("||  Using conda environment of name '%s'  ||", pathOrEnvirName, 
                               name = "logger.base")
    }else if(type == 'virtualenv'){
      reticulate::use_virtualenv(pathOrEnvirName, required = T)
      futile.logger::flog.info("||  Using virtual environment of name '%s'  ||", pathOrEnvirName, 
                               name = "logger.base")
    }else if (type == 'python'){
      reticulate::use_python(pathOrEnvirName, required = T)
      futile.logger::flog.info("||  Using python at path: '%s'  ||", pathOrEnvirName, 
                               name = "logger.base")
    }else{
      futile.logger::flog.error("||  Invalid type - Should be one of 'conda', 'virtualenv', or 'python'  ||")
    }
  }, error = function(e){
    futile.logger::flog.error("||  %s  ||", e, name = 'logger.base')
  })
}

#' @name getTargetForPyClassification
#' @export
getTargetForPyClassification <- function(dataset, targetVarName, positiveClass){
  dataset %>% dplyr::mutate(target = ifelse(!!rlang::sym(targetVarName) == !!(positiveClass) , 1, 0)) %>%     dplyr::select(target) %>%
    as.list() %>% unlist -> targetList
  names(targetList) <- NULL
  targetList %>% as.factor %>% reticulate::r_to_py() -> target
  return(target)
}

#' @name getFeaturesForPyClassification
#' @export
getFeaturesForPyClassification <- function(dataset, featureNames){
  dataset %>% dplyr::select(!!featureNames) %>% as.matrix %>% reticulate::r_to_py() -> featureMatrix
  return(featureMatrix)
}


# setPythonEnvir('python', '/Users/naren/anaconda3/bin/python')
# os <- reticulate::import("os")
# numpy <- reticulate::import("numpy")
# pandas <- reticulate::import("pandas")
# sklearn <- reticulate::import("sklearn")
# 
# reticulate::source_python("python/sampleFunctions.py")
