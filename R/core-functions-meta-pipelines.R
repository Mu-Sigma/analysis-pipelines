#' @name MetaAnalysisPipeline
#' @title TBW
#' @details TBW
#' @slot pipeline TBW
#' @family Package core functions
#' @exportClass MetaAnalysisPipeline
#' @export MetaAnalysisPipeline

setOldClass("proto")
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
  object %>>% setLoggerDetails(target = "none") -> object
  metaPipeline <- MetaAnalysisPipeline()
  pipelineProto <- proto::proto()
  if(class(object) == "AnalysisPipeline"){
    metaPipeline@type <- "batch"
  }else if(class(object) == "StreamingAnalysisPipeline"){
    metaPipeline@type <- "streaming"
  }

  if(nrow(object@pipelineExecutor$topologicalOrdering) == 0){
    object %>>% prepExecution -> object
  }

  object@pipeline -> pipeline
  pipeline %>>% purrr::pmap(function(id, operation, heading,
                                     parameters, outAsIn, storeOutput, dependencies){
    # fnName <- paste0("fn_", operation)
    fnName <- operation
    assign(x = fnName, value = proto::proto(), envir = pipelineProto)

    purrr::imap(parameters, function(p, np){
      # n <- names(p)
      if(class(p) == "formula"){
        if(analysisPipelines:::isDependencyParam(p)){
          n <- analysisPipelines:::getResponse(p)
          p <- paste0("~", analysisPipelines:::getTerm(p)) %>>% as.formula
        }
      }
      assign(x = paste0(np),
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
    names(newParams) %>>% grep(x = ., pattern = "^[.]", value = T, invert = T ) -> fnNames

    newParamList <- purrr::imap(fnNames, function(fn, nfn){
      fnEnvir <- get(fn, envir = newParams)
      fnEnvir %>>% names %>>% grep(x = ., pattern = "^[.]", invert = T, value = T ) -> argNames
      params <- mget(x = argNames, envir = newParams[[fn]])
      params <- purrr::imap(params, function(p, np){
        if(class(p) == "formula"){
          if(analysisPipelines:::isDependencyParam(p)){
            p <- paste(np, "~", analysisPipelines:::getTerm(p)) %>>% as.formula
            # names(p) <- NULL
          }
          #TODO: Deal with normal formula parameters
        } #else{
          # names(p) <- np
        # }
        return(p)
      })
      # names(params) <- NULL
      return(params)
    })
    names(newParamList) <- fnNames
  }

  tblOrder <- match(pipelineObj@pipeline$operation, names(newParamList))
  newParamList <- newParamList[tblOrder]
  names(newParamList) <- NULL

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
  sampleObj %>>% setLoggerDetails(target = "none") -> sampleObj
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
    .setRegistry(.registry)
    lapply(functionNames, function(x){
      assign(x, get(x, environment()), globalenv())
    })
    return(object)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}


