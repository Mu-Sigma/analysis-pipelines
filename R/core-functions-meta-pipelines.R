#' @name MetaAnalysisPipeline
#' @title TBW
#' @details TBW
#' @slot pipeline TBW
#' @family Package core functions
#' @exportClass MetaAnalysisPipeline
#' @export MetaAnalysisPipeline

MetaAnalysisPipeline <- setClass("MetaAnalysisPipeline",
                                 slots = c(
                                   pipeline = "tbl",
                                   pipelinePrototype = "proto",
                                   type = "character"
                                 ))

#' @name initializeMetaAnalysisPipeline
#' @title This is the constructor for the \link{MetaAnalysisPipeline} class
#' @param .Object The \code{MetaAnalysisPipeline} object
#' @details TBW
#' @return an object of class "\code{MetaAnalysisPipeline}"
#' @family Package core functions
#' @export

setMethod(
  f = "initialize",
  signature = "MetaAnalysisPipeline",
  definition = function(.Object, type = "batch")
  {
    tryCatch({
      .Object@pipeline <- tibble(
        id = character(),
        operation = character(),
        heading = character(),
        parameters = list(),
        outAsIn = logical(),
        storeOutput = F
      )

      .Object@type <- "batch"

      return(.Object)
    }, error = function(e){
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    })
  }
)

#' @name exportAsMetaPipeline
#' @title Method to export a Meta pipeline
#' @param .Object A Pipeline object
#' @details TBW
#' @return an object of class "\code{MetaAnalysisPipeline}"
#' @family Package core functions
#' @export
setGeneric(
  name = "exportAsMetaPipeline",
  def = function(object){
    standardGeneric("exportAsMetaPipeline")
  }
)

.exportAsMetaPipeline <- function(object){
  metaPipeline <- MetaAnalysisPipeline()
  pipelineProto <- proto::proto()
  if(class(object) == "AnalysisPipeline"){
    metaPipeline@type <- "batch"
  }else if(class(object) == "StreamingAnalysisPipeline"){
    metaPipeline@type <- "streaming"
  }

  object@pipeline -> pipeline
  pipeline %>>% purrr::pmap(function(id, operation, heading,
                                     parameters, outAsIn, storeOutput){
    # print(operation)
    fnName <- paste0("fn_", operation)
    assign(x = fnName, value = proto::proto(), envir = pipelineProto)
    paramNames <- names(parameters)
    if(is.null(paramNames)){
      names(parameters) <- paste0("param", 1:length(parameters))
    }

    purrr::imap(parameters, function(p, n){
      assign(x = paste0("arg_", n),
             value = p,
             envir = pipelineProto[[fnName]])
      return(NULL)
    })
    return(NULL)
  })
  metaPipeline@pipeline <- pipeline
  metaPipeline@pipelinePrototype <- pipelineProto
  return(metaPipeline)
}

setMethod(
  f = "exportAsMetaPipeline",
  signature = "BaseAnalysisPipeline",
  definition = .exportAsMetaPipeline
)

#' @name getPipelinePrototype
#' @title TBW
#' @param object  TBW
#' @details TBW
#' @return TBW
#' @family Package core functions
#' @export
setGeneric(
  name = "getPipelinePrototype",
  def = function(metaPipelineObj){
    standardGeneric("getPipelinePrototype")
  }
)

.getPipelinePrototype <- function(metaPipelineObj){
  return(metaPipelineObj@pipelinePrototype)
}

setMethod(
  f = "getPipelinePrototype",
  signature = "MetaAnalysisPipeline",
  definition = .getPipelinePrototype
)


#' @name createPipelineInstance
#' @title TBW
#' @param object  TBW
#' @details TBW
#' @return TBW
#' @family Package core functions
#' @export
setGeneric(
  name = "createPipelineInstance",
  def = function(metaPipelineObj, newParams){
    standardGeneric("createPipelineInstance")
  }
)

.createPipelineInstance <- function(metaPipelineObj, newParams){

  ## deal with formulas
  ## get named arguments when creating pipeline
  ## provide a template to fill
  ## error handling

  if(metaPipelineObj@type == "batch"){
    pipelineObj <- AnalysisPipeline()
  }else if(metaPipelineObj@type == "streaming"){
    pipelineObj <- StreamingAnalysisPipeline()
  }

  pipelineObj@pipeline <- metaPipelineObj@pipeline

  newParamList <- newParams
  if(any(class(newParams) == "proto")){
    names(newParams) %>>% grepl(x = ., pattern = "^fn_*" ) -> fnNameInd
    names(newParams)[fnNameInd] -> fnNames
    newParamList <- purrr::imap(fnNames, function(fn, n){
      fnEnvir <- get(fn, envir = newParams)
      fnEnvir %>>% ls %>>% grepl(x = ., pattern = "^arg_*" ) %>>% which -> argNameInd
      ls(fnEnvir)[argNameInd] -> argNames
      params <- mget(x = argNames, envir = newParams[[fn]])

      #TODO: Remove this once names are set
      names(params) <- NULL
      return(params)
    })
    names(newParamList) <- unlist(lapply(fnNames, function(f) unlist(strsplit(f, "_"))[2]))
  }

  tblOrder <- match(pipelineObj@pipeline$operation, names(newParamList))
  newParamList <- newParamList[tblOrder]

  pipelineObj@pipeline %>>% dplyr::mutate(parameters = newParamList) -> pipelineObj@pipeline

  return(pipelineObj)
}

setMethod(
  f = "createPipelineInstance",
  signature = "MetaAnalysisPipeline",
  definition = .createPipelineInstance
)


.visualizeMetaPipeline <- function(object){
  object %>>% createPipelineInstance(object@pipelinePrototype) -> sampleObj
  vis <- NULL
  sampleObj %>>% prepExecution -> sampleObj
  sampleObj %>>% visualizePipeline -> vis
  return(vis)
}

setMethod(
  f = "visualizePipeline",
  signature = "MetaAnalysisPipeline",
  definition = .visualizeMetaPipeline
)


.saveMetaPipeline <- function(object, path){
  tryCatch({
    .registry <- getRegistry()
    listToBeSaved <- c("object", ".registry", getRegistry()$functionName, getRegistry()$exceptionHandlingFunction)
    save(list = listToBeSaved,file = path)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

setMethod(
  f = "savePipeline",
  signature = "MetaAnalysisPipeline",
  definition = .saveMetaPipeline
)

#' @name loadMetaPipeline
#' @title TBW
#' @param path TBW
#' @details TBW
#'@return TBW
#' @family Package core functions
#' @export
loadMetaPipeline <- function(path){
  tryCatch({

    futile.logger::flog.warn("||  The existing registry will be overwritten with the registry from the RDS file  ||",
                               name = "logger.base")
    load(path, envir = environment())
    functionNames = setdiff(ls(envir = environment()), c("path", "object", ".registry"))

    lapply(functionNames, function(x){
      assign(x, get(x, environment()), globalenv())
    })
    return(object)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}


