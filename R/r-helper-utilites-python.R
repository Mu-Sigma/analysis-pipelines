#' @name setPythonEnvir
#' @title Sets the python environment to be used
#' @details Wrapper function over reticulate functions to set a python environment to be used
#' @param type Type of python environment. Takes three possible vales - 'conda' for Anaconda environments,
#' 'virtualenv' for Virtual environments, and 'python' to manually set the python path to use
#' @param pathOrEnvirName Name of the environment for Anaconda and Virtual environments,
#' or the Python path when type is 'python'
#' @family R helper utilities for Python
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


#' @name getFeaturesForPyClassification
#' @title Extracts selected columns from a data frame as a Python array
#' @details Helper function, which when provided an R data frame and a set of column/ feature names,
#' extracts them from the R data frame as a matrix and converts them to the equivalent Python array.
#' @details Typically this function can be used when providing a feature matrix to a Python machine learning function
#' @param dataset an R data frame
#' @param featureNames Column names to be extracted from the R data frames. A character vector.
#' @family R helper utilities for Python
#' @export
getFeaturesForPyClassification <- function(dataset, featureNames){
  dataset %>% dplyr::select(!!featureNames) %>% as.matrix %>% reticulate::r_to_py() -> featureMatrix
  return(featureMatrix)
}

#' @name getTargetForPyClassification
#' @title Extracts selected column from a data frame a binary class Python array
#' @details Helper function, which when provided an R dataframe and a binary categorical column,
#' extracts it from the R data frame, converts it to 1/0 class coding, and converts it to a Python array
#' @details Typically this function can be used to extract a target variable for a classifier to be provided to a
#' Python machine learning function
#' @export
getTargetForPyClassification <- function(dataset, targetVarName, positiveClass){
  dataset %>% dplyr::mutate(target = ifelse(!!rlang::sym(targetVarName) == !!(positiveClass) , 1, 0)) %>%     dplyr::select(target) %>%
    as.list() %>% unlist -> targetList
  names(targetList) <- NULL
  targetList %>% as.factor %>% reticulate::r_to_py() -> target
  return(target)
}

