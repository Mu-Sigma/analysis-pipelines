##################################################################################################
# Title: Reusable pipelines for generating analyses outputs and reports
# Author: Naren Srinivasan
# Created on: July 12, 2018
# Description: An R package version which works both on R data frames, and a Spark environment i.e.
#              Spark DataFrames including Structured Streaming
##################################################################################################

#' @importFrom pipeR %>>%
#' @importFrom rlang .data
#' @importFrom graphics image
#' @importFrom methods getClass new removeMethod setClassUnion setGeneric setOldClass
#' @importFrom stats as.formula lm reorder terms
#' @importFrom utils installed.packages read.csv
NULL

pos <- 1
globEnv = as.environment(pos)

try({
  if(!("SparkR" %in% installed.packages())){
    futile.logger::flog.warn(paste0("||  The SparkR package is not installed. Please ensure the right SparkR version compatible",
                                  "compatible with the Spark distribution you plan to use is installed. You can use the 'devtools'",
                                  "package to do the same using 'devtools::install_github('apache/spark@v2.x.x', subdir='R/pkg')'  ||"),
                             name = "logger.base")
  }
}, silent = TRUE)


#' This section defines the environment which the package uses for maintaining the registry and an outputCache
#' @keywords internal
.analysisPipelinesEnvir <- new.env(parent = emptyenv())

.analysisPipelinesEnvir$.functionRegistry <- dplyr::tibble(
  functionName = character(),
  heading = character(),
  engine = character(),
  exceptionHandlingFunction = character(),
  userDefined = logical(),
  isDataFunction = logical(),
  firstArgClass = character()
)
.analysisPipelinesEnvir$.outputCache <- new.env()

#' @name BaseAnalysisPipeline-class
#' @rdname BaseAnalysisPipeline-class
#' @title Base class for \code{AnalysisPipeline} and \code{StreamingAnalysisPipeline} objects
#' @details The class which holds the metadata including the registry of available functions,
#' the data on which the pipeline is to be applied, as well as the pipeline itself, and serves
#' as the base class for various types of Pipeline objects such as Batch and Streaming.
#' @details This base class which contains the slots related to the registry, pipeline and output can be extended
#' to create custom class for specific scenarios if required.
#' @details In the documentation, objects of classes which are subclasses of this class are referred to as 'Pipeline' objects
#' @slot pipeline A tibble which holds functions to be called
#' @slot pipelineExecutor A list containing details of the execution, such as topological ordering of functions to be executed,
#' dependency map of functions, as well as logger configuration
#' @slot output A list which holds all the functions output
#' @family Package core functions
#' @exportClass BaseAnalysisPipeline
#' @export BaseAnalysisPipeline

BaseAnalysisPipeline <- setClass("BaseAnalysisPipeline",
                             slots = c(
                               pipeline = "tbl",
                               pipelineExecutor = "list",
                               output = "list"
                             ))

#' BaseAnalysisPipeline constructor
#' @docType methods
#' @rdname initialize-methods
#' @title This is the constructor for the \link{BaseAnalysisPipeline} class
#' @family Package core functions
#' @keywords internal

setMethod(
  f = "initialize",
  signature = "BaseAnalysisPipeline",
  definition = function(.Object)
  {
    tryCatch({
      .Object@pipeline <- dplyr::tibble(
        id = character(),
        operation = character(),
        heading = character(),
        parameters = list(),
        outAsIn = logical(),
        storeOutput = F
      )

      .Object@pipelineExecutor <- list(
        topologicalOrdering = dplyr::tibble(id = character(),
                                     level = character()),
        dependencyLinks = dplyr::tibble(from = character(),
                                 to = character()),
        loggerDetails <- list()
      )
      .Object@output <- list()

      .Object %>>% setLoggerDetails -> .Object
      initializeLoggers(.Object)

      return(.Object)
    }, error = function(e){
      futile.logger::flog.error(e, name = "logger.base")
    stop()
    }, warning = function(w){
      futile.logger::flog.warn(w, name = "logger.base")
    })
  }
)

#' @name registerFunction
#' @title Register a user-defined function to be used with a \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details The specified operation along with the heading and engine details is stored in the registry, after which it can be added to a pipeline.
#' @details If the function already exists in the registry, registration will be skipped. In order to change the definition, the function needs
#' to be reassigned in the Global Environment and then the \code{registerFunction} called again.
#' @param functionName name of function to be registered
#' @param heading heading of that section in report
#' @param functionType type of function - 'batch' for \code{AnalysisPipeline} objects, 'streaming' for \code{StreamingAnalysisPipeline} objects
#' @param engine specifies which engine the function is to be run on. Available engines include "r", "spark", and "python"
#' @param isDataFunction logical parameter which defines whether the function to be registered operates on data i.e. the first parameter is a dataframe
#' @param exceptionFunction R object corresponding to the exception function
#' @param firstArgClass character string with the class of the first argument to the function, if it is a non-data function
#' @param loadPipeline logical parameter to see if function is being used in loadPipeline or not. This is for internal working
#' @param userDefined logical parameter defining whether the function is user defined. By default, set to true
#' @family Package core functions
#' @examples
#' \dontrun{
#'   library(analysisPipelines)
#'   getNumRows <- function(dataset){
#'    return(nrow(dataset))
#'   }
#'
#'   registerFunction("getNumRows")
#'}
#' @export
registerFunction <- function( functionName, heading = "",
                              functionType = "batch", # batch, streaming
                               engine = "r",
                               exceptionFunction = as.character(substitute(genericPipelineException)),
                               isDataFunction = T, firstArgClass = "",
                               loadPipeline = F, userDefined = T
                               ){
    tryCatch({

      #NULL checks
      if(is.null(functionName) || functionName == ""){
        functionName <- paste("userDefinedFunction_", Sys.time())
        futile.logger::flog.warn(paste("||  NULL or empty name provided. Setting function name automatically as '%s'   ||"),
                                 functionName, name = "logger.base")
      }

      if(!functionType %in% c("batch", "streaming")){
        futile.logger::flog.error(paste("||  The provided 'functionType' must be 'batch' or 'streaming'   ||"),
                                  name = "logger.base")
        stop()
      }

      if(!engine %in% c("r", "spark", "spark-structured-streaming", "python")){
        futile.logger::flog.error(paste("|| 'engine' must be one of the supported engines. Please check ",
                                        "with this version of the packge in the documentation  ||"),
                                  name = "logger.base")
        stop()
      }

      if(is.null(exceptionFunction)){
        futile.logger::flog.warn(paste("|| 'exceptionFunction' must not be NULL. Setting automatically to default.  "),
                                  name = "logger.base")
      }

      if(!is.logical(isDataFunction)){
        futile.logger::flog.error(paste("|| 'isDataFunction' must be a logical value.  "),
                                 name = "logger.base")
        stop()
      }


      #Define data frame class according to engine type

      childClass <- "AnalysisPipeline"
      if(functionType == "streaming"){
        childClass <- "StreamingAnalysisPipeline"
      }

      dataFrameClass <- "data.frame"
      if(engine == "spark" || engine == 'spark-structured-streaming'){
        # dataFrameClass <- "SparkDataFrame"
        dataFrameClass <- "ANY"
      }else if(engine == 'python'){
        dataFrameClass <- "pandas.core.frame.DataFrame"
      }


      ## Checking if already registered
      doesMethodExist <- c()
      tryCatch({
        doesMethodExist <- methods::findMethod(functionName, signature = childClass, where = .GlobalEnv)

          if(length(doesMethodExist) > 0){

            futile.logger::flog.error(paste("||  A function of name '%s' has already been registered.",
                                            "If you'd like to change the definition, please re-assign the function definition",
                                             "and then call 'registerFunction' again.  ||"),
                                      functionName, name = "logger.base")
            stop()
          }
      }, warning = function(w){
        tryCatch({
          func <- methods::getFunction(name = functionName, where = .GlobalEnv)
        }, error = function(e){
          futile.logger::flog.error(paste("||  The provided function name does not exist in the Global environment  ||"),
                                    name = "logger.base")
          stop()
        })
      })

      ## Checking if class of first argument provided is valid
      if(!isDataFunction){
        tryCatch({
          getClass(firstArgClass)
        }, error = function(e){
          setOldClass(firstArgClass, where = .GlobalEnv)
          futile.logger::flog.error(paste("||  The provided class of the first argument is not a defined S4 class.',
                                          ' Assuming that it is an S3 class and setting the definition in the ',
                                          environment  ||"),
                                    name = "logger.base")
        })

      }

      #Skip registration if already exists
      if(length(doesMethodExist) == 0){

        parametersName <- ""
        paramsToBeParsed <- ""
        methodParams <- ""
        genericSignature <- c()
        objectName <- "object"

        packageMethodSignature <- c()
        origMethodSignature <- c()

        objectArg <- alist(object = )
        commonArgs <- alist(outAsIn = F, storeOutput = F)
        newArgs <- alist()

        f <- get(functionName, .GlobalEnv)
        origF <- f
        originalArgs <- list()

        argEnv <- NULL
        #Checking for direct python functions
        if(any(class(f) == "python.builtin.function")){
           inspect <- reticulate::import("inspect")
           argEnv <- inspect$getargspec(f)
           originalArgs <- argEnv$args %>>% lapply(function(x){
             a <- eval(parse(text = paste0("alist(", x, " = )")))
             return(a)
           }) %>>% unlist %>>% as.list
        }else{
          originalArgs <- formals(f) %>>% as.list
        }
        firstArg <- names(originalArgs)[1]
          if(isDataFunction){
            firstArgClass <- dataFrameClass
            originalArgs[[1]] <- rlang::.data
          }
          newArgs <- append(objectArg, originalArgs)
          paramsToBeParsed <- paste0(originalArgs %>>% names, collapse = ", ")
          # genericSignature <- c("object", names(originalArgs)[1])
          genericSignature <- c("object", names(originalArgs))

          ## Adding missing signature to method
          if(isDataFunction){
            firstArgClassName <- paste0("formulaOR", firstArgClass, "ORmissing")
            setClassUnion(name = firstArgClassName,
                          c("formula", firstArgClass, "missing"),
                          where = .GlobalEnv)
          }else{
            firstArgClassName <- paste0("formulaOR", firstArgClass)
            setClassUnion(name = firstArgClassName,
                          c("formula", firstArgClass),
                          where = .GlobalEnv)
          }

          packageMethodSignature <- c(childClass, firstArgClassName,
                                      rep("ANY", length(names(originalArgs)) - 1))
          origMethodSignature <- c("missing", firstArgClass,
                                   rep("ANY", length(names(originalArgs)) - 1))

          #Converting to string
          packageMethodSignature <- paste0('c("', paste(packageMethodSignature, collapse = '", "'), '")')
          origMethodSignature <- paste0('c("', paste(origMethodSignature, collapse = '", "'), '")')

        parametersName <- paste0(newArgs %>>% names, collapse = ", ")
        methodParams <- paste0(originalArgs %>>% names, collapse = ", ")

        methodBody <- paste0(utils::capture.output(body(eval(parse(text=functionName)))),collapse="\n")

        ##Assigning the exception function to the global Environment
        assign(exceptionFunction, get(x = exceptionFunction,
                                      envir = environment()),
               envir = globEnv)



        genericArgs <- append(newArgs, commonArgs)
        formals(f) <- genericArgs
        body(f) <- paste('standardGeneric("', functionName,'")')

        ## Suffix for python & Spark functions
        if(engine == 'python'){
          methodBody <- paste0('{',
                             'val <- ', functionName, '(', methodParams, ');',
                               'return(val);}')
          functionName <- paste0(functionName, "_py")
        }else if(engine == "spark"){
          functionName <- paste0(functionName, "_spark")
        }else if(engine == "spark-structured-streaming"){
          functionName <- paste0(functionName, "_sparkSS")
        }

        registerFunText <-
          paste0(
                 # Adding to pipeline when run on a Analysis Pipeline object
                 'setMethod(f = "', functionName,'",',
                     'signature = ', packageMethodSignature, ',',
                     'definition = function(',
                     parametersName, ',',
                     'outAsIn, storeOutput){',
                         'parametersList <- unlist(strsplit(x = "', paramsToBeParsed, '", split = ","' ,'));',
                          'parametersList <- trimws(parametersList);',
                         'parametersPassed <- lapply(parametersList, function(x){',
                                                                              'val <- eval(parse(text = x));',
                                                                              'if(class(val) == "formula"){',
                                                                                'if(analysisPipelines::isDependencyParam(val)){',
                                                                                   'val <- as.formula(paste(x,"~",analysisPipelines::getTerm(val)))',
                                                                                '};',#else{',
                                                                                  # 'names(val) <- x',
                                                                                # '};',
                                                                              '};', #else{',
                                                                                # 'names(val) <- x',
                                                                              # '};',
                                                                               'return(val);}',
                                                    ');',
                          'names(parametersPassed) <- parametersList;',
                         'return(updateObject(', objectName, ',',
                              'operation = "', functionName, '",',
                             'heading = "', heading, '",',
                             'parameters = parametersPassed, outAsIn = outAsIn, storeOutput = storeOutput));});',

                 #Executing the actual function when pipeline is executed
                 'setMethod(f = "',functionName,'",',
                     'signature = ', origMethodSignature, ',',
                     'definition = function( ', methodParams,')',
                     methodBody, ')'
          )

        #Register function
        # Generic
       setGeneric(name = functionName,
                  signature = genericSignature,
                  def = f,
                  where = .GlobalEnv)
        try({
          removeMethod(f = get(functionName, .GlobalEnv), signature = "ANY", where = .GlobalEnv)
        }, silent = T)

        # Methods for pipeline object, and first argument
        eval(parse(text = registerFunText), envir=.GlobalEnv)

        # Updating registry
        if(loadPipeline==F){
          fn <- paste0(functionName)

            .updateRegistry(functionName = fn,
                            heading = heading,
                            engine = engine,
                            exceptionHandlingFunction = exceptionFunction,
                            userDefined = userDefined,
                            isDataFunction = isDataFunction,
                            firstArgClass = firstArgClass)
            invisible("Registration Successful")
            futile.logger::flog.info("||  Function '%s' was registered successfully  ||", fn, name = "logger.base")
        }
      }
    }, error = function(e){
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    })
  }

#' This is an internal function used to update the registry, in order to override existing function registrations
#' @name .updateRegistry
#' @keywords internal
.updateRegistry <- function(functionName,
                            heading = "",
                            engine = "r",
                            exceptionHandlingFunction = as.character(substitute(genericPipelineException)),
                            userDefined = F,
                            isDataFunction = T,
                            firstArgClass = ""){
  fn <- paste(functionName)
  if(nrow(.analysisPipelinesEnvir$.functionRegistry  %>>% dplyr::filter(functionName == fn)) == 1){
    .analysisPipelinesEnvir$.functionRegistry %>>%
                              dplyr::filter(functionName != fn) -> .analysisPipelinesEnvir$.functionRegistry
  }
  .analysisPipelinesEnvir$.functionRegistry %>>% dplyr::add_row(functionName = functionName,
                                                                heading = heading,
                                                                engine = engine,
                                                                exceptionHandlingFunction = exceptionHandlingFunction,
                                                                userDefined = userDefined,
                                                                isDataFunction = isDataFunction,
                                                                firstArgClass = firstArgClass) -> .analysisPipelinesEnvir$.functionRegistry

}

#' This is an internal function which returns the cache from the package namespace
#' @name .getCache
#' @keywords internal
.getCache <- function(){
  return(.analysisPipelinesEnvir$.outputCache)
}

#' Internal function used to set the registry object in case of loading pipelines or meta-pipelines
#' @name .setRegistry
#' @keywords internal
.setRegistry <- function(.registry){
  .analysisPipelinesEnvir$.functionRegistry <- .registry
}

#' @name loadPredefinedFunctionRegistry
#' @title Loading the registry of predefined functions
#' @details Loads the registry of predefined functions
#' @family Package core functions
#' @examples
#' \dontrun{
#'   library(analysisPipelines)
#'   loadPredefinedFunctionRegistry()
#' }
#' @export
loadPredefinedFunctionRegistry <- function(){
  tryCatch({

    for(rowNo in 1:nrow(.batchPredefFunctions)){
      registerFunction(functionType = "batch",
                       functionName = .batchPredefFunctions[['functionName']][[rowNo]],
                       heading =  .batchPredefFunctions[['heading']][[rowNo]],
                       engine = .batchPredefFunctions[['engine']][[rowNo]],
                       exceptionFunction = .batchPredefFunctions[['exceptionHandlingFunction']][[rowNo]],
                       isDataFunction = .batchPredefFunctions[['isDataFunction']][[rowNo]],
                       firstArgClass = .batchPredefFunctions[['firstArgClass']][[rowNo]],
                       userDefined = F, loadPipeline = F )
    }

    for(rowNo in 1:nrow(.streamingPredefFunctions)){

      registerFunction( functionType = "streaming",
                        functionName = .streamingPredefFunctions[['functionName']][[rowNo]],
                        heading =  .streamingPredefFunctions[['heading']][[rowNo]],
                        engine = .streamingPredefFunctions[['engine']][[rowNo]],
                        exceptionFunction = .streamingPredefFunctions[['exceptionHandlingFunction']][[rowNo]],
                        isDataFunction = .streamingPredefFunctions[['isDataFunction']][[rowNo]],
                        firstArgClass = .streamingPredefFunctions[['firstArgClass']][[rowNo]],
                        userDefined = F, loadPipeline = F)
    }
    futile.logger::flog.info('||  Predefined utility functions registered  ||', name = "logger.base")
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  })
}

#' @name setInput
#' @rdname setInput
#' @title Sets the input for an \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details
#'      Assigns the input to the pipeline for an  \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @param input the input data frame
#' @param filePath path to the file which needs to be read (currently supports .csv files)
#' @return Updated \code{AnalysisPipeline} \code{StreamingAnalysisPipeline} object
#' @family Package core functions
#' @examples
#'   library(analysisPipelines)
#'   pipelineObj <- AnalysisPipeline()
#'   pipelineObj %>>% setInput(input = iris) -> pipelineObj
#' @export

setGeneric(
  name = "setInput",
  def = function(object,
                input,
                filePath = "")
  {
    standardGeneric("setInput")
  }
)

.setInput = function(object, input, filePath = "")
{
  tryCatch({
    input <- initDfBasedOnType(input, filePath)
    object@input <- input
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname setInput
setMethod(
  f = "setInput",
  signature = "BaseAnalysisPipeline",
  definition = .setInput
)

#' @name updateObject
#' @rdname updateObject
#' @title Update the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object by adding an operation to the pipeline
#' @details
#'       The specified operation along with the heading and parameters is updated in the pipeline slot
#'       of the  \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object, where the sequence of operations
#'      to be performed is stored
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @param operation function name to be updated in tibble
#' @param heading heading of that section in report
#' @param parameters parameters passed to that function
#' @param outAsIn whether to use original input or output from previous function
#' @param storeOutput whether the output of this operation is to be stored
#' @return Updated \code{AnalysisPipeline} \code{StreamingAnalysisPipeline} object
#' @family Package core functions
#' @export
setGeneric(
  name = "updateObject",
  def = function(object,
                 operation,
                 heading = "",
                 parameters,
                 outAsIn = F,
                 storeOutput = F)
  {
    standardGeneric("updateObject")
  }
)

.updateObject = function(object, operation, heading = "", parameters, outAsIn = F,  storeOutput = F)
{
  tryCatch({
    if(nrow(object@pipeline) == 0){
      id = "1"
    }else{
      id = max(as.numeric(object@pipeline$id)) + 1
      id <- as.character(id)
    }
    object@pipeline %>>% dplyr::add_row(id = id,
                                 operation = operation,
                                 heading = heading,
                                 parameters = list(parameters),
                                 outAsIn = outAsIn,
                                 storeOutput = storeOutput) -> object@pipeline
    return(object)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname updateObject
setMethod(
  f = "updateObject",
  signature = "BaseAnalysisPipeline",
  definition = .updateObject
)

#' @name assessEngineSetUp
#' @rdname assessEngineSetUp
#' @title Assesses engine (R, Spark, Python, Spark Structured Streaming) set up
#' @details
#'       Assesses whether engines required for executing functions in an \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline}
#'       object have been set up
#' @details This method is implemented on the base class as it is a shared functionality across Pipeline objects
#' @param object A Pipeline object
#' @return Tibble containing the details of available engines, whether they are required for a pipeline, a logical value
#'         reporting whether the engine has been set up, and comments.
#' @family Package core functions
#' @examples
#' \dontrun{
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' pipelineObj %>>% univarCatDistPlots(uniCol = "Species", priColor = "blue",
#'  optionalPlots = 0) %>>% assessEngineSetUp
#' }
#' @export

setGeneric(
  name = "assessEngineSetUp",
  def = function(object)
  {
    standardGeneric("assessEngineSetUp")
  }
)

.assessEngineSetUp = function(object)
{
  tryCatch({
    startEngineAssessment <- Sys.time()
    futile.logger::flog.info("||  Engine Assessment for pipeline STARTED  ||" , name='logger.engine.assessment')

    engineAssessment <- dplyr::tibble(engine = character(),
                               requiredForPipeline = logical(),
                               isSetup = logical(),
                               comments = character())
    if(nrow(object@pipeline) == 0){
      m <- "No functions have been added to the pipeline"
      futile.logger::flog.error(m)
      stop(m)
    }else{
      pipelineRegistryJoin = dplyr::left_join(object@pipeline, getRegistry(), by = c("operation" = "functionName"))
      requiredEngines <- unique(pipelineRegistryJoin$engine)

      # R
      isRSetup <- T
      rComments <- ""
      engineAssessment %>>% dplyr::add_row(engine = "r",
                                           requiredForPipeline = ifelse("r" %in% requiredEngines, T, F),
                                           isSetup = isRSetup,
                                           comments = rComments)           -> engineAssessment

      #Spark Batch and Structured Streaming

      isSparkSetup <- T
      sparkComments <- ""
      checkSession <- tryCatch(SparkR::sparkR.conf(), error = function(e) e)
      if("SparkSession not initialized" %in% checkSession){
        isSparkSetup <- F
        sparkComments <- paste0("There does not seem to be a Spark Session initialized through SparkR ",
                                "which is required to execute pipelines containing Spark functions. ",
                                "Please initialize a SparkR session. The analysisPipelines::sparkRSessionCreateIfNotPresent() ",
                                "helper function can be used.")
      }else{
        sparkComments <- paste0("SESSION DETAILS : ", paste0(checkSession, collapse = " "))
      }

      engineAssessment %>>% dplyr::add_row(engine = "spark",
                                           requiredForPipeline = ifelse("spark" %in% requiredEngines, T, F),
                                           isSetup = isSparkSetup,
                                           comments = sparkComments)           -> engineAssessment

      engineAssessment %>>% dplyr::add_row(engine = "spark-structured-streaming",
                                           requiredForPipeline = ifelse("spark-structured-streaming" %in% requiredEngines, T, F),
                                           isSetup = isSparkSetup,
                                           comments = sparkComments)           -> engineAssessment

      #Python
      isPythonSetup <- F
      pythonComments <- ""
      checkSession <- ""
      checkSession <- tryCatch({
        reticulate::py_run_string('a = "Is session running"') %>>% reticulate::py_to_r() -> sess
        if("a" %in% names(sess)){
          isPythonSetup <- T
          pythonComments <- reticulate::py_config() %>% as.character %>>% paste(collapse = "\n")
        }else{
          pythonComments <- paste0("There does not seem to be a Python Session initialized through reticulate ",
                                   "which is required to execute pipelines containing Python functions. ")
        }
      }, error = function(e){
         pythonComments <- paste0("There does not seem to be a Python Session initialized through reticulate ",
                                 "which is required to execute pipelines containing Python functions. ")
      })

      engineAssessment %>>% dplyr::add_row(engine = "python",
                                           requiredForPipeline = ifelse("python" %in% requiredEngines, T, F),
                                           isSetup = isPythonSetup,
                                           comments = pythonComments)           -> engineAssessment

    }


    endEngineAssessment <- Sys.time()
    engineAssessmentTime <- endEngineAssessment - startEngineAssessment
    futile.logger::flog.info("||  Engine Assessment COMPLETE. Time taken : %s seconds||", engineAssessmentTime, name='logger.engine.assessment')

    return(engineAssessment)
  }, error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname assessEngineSetUp
setMethod(
  f = "assessEngineSetUp",
  signature = "BaseAnalysisPipeline",
  definition = .assessEngineSetUp
)

#' @name savePipeline
#' @rdname savePipeline
#' @title Saves the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object to the file system without outputs
#' @details
#'       The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object is saved to the file system in the paths specified
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @param object object that contains input, pipeline, registry and output
#' @param path the path at which the .Rda file containing the pipeline should be stored, along with the name of the file including
#' a .Rda extension
#' @return Does not return a value
#' @family Package core functions
#' @examples
#' \dontrun{
#'   library(analysisPipelines)
#'   pipelineObj <- AnalysisPipeline(input = iris)
#'   pipelineObj %>>% savePipeline(path = "./test.RDS")
#' }
#' @export

setGeneric(
  name = "savePipeline",
  def = function(object, path)
  {
    standardGeneric("savePipeline")
  }
)

.savePipeline = function(object, path){
  tryCatch({
    object@output <- list()
    object@input <- data.frame()
    .registry <- getRegistry()
    listToBeSaved <- c("object", ".registry", getRegistry()$functionName, getRegistry()$exceptionHandlingFunction)
    save(list = listToBeSaved,file = path)
    futile.logger::flog.info("|| Pipeline and associated registry saved successfully at path '%s'  ||", path,
                             name = "logger.base")
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  }, warning = function(w){
    futile.logger::flog.warn(w, name = "logger.base")
  })
}

#' @rdname savePipeline
setMethod(
  f = "savePipeline",
  signature = "BaseAnalysisPipeline",
  definition = .savePipeline
)


#' @name getPipeline
#' @rdname getPipeline
#' @title Obtain the pipeline
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details
#'      Obtains the pipeline from the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object as a tibble
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @return Tibble describing the pipeline
#' @family Package core functions
#' @examples
#' \dontrun{
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' getNumRows <- function(dataset){
#'   return(nrow(dataset))
#' }
#' registerFunction("getNumRows")
#' pipelineObj %>>% getNumRows %>>% getPipeline
#' }
#' @export

setGeneric(
  name = "getPipeline",
  def = function(object)
  {
    standardGeneric("getPipeline")
  }
)

.getPipeline = function(object){
  return(object@pipeline)
}

#' @rdname getPipeline
setMethod(
  f = "getPipeline",
  signature = "BaseAnalysisPipeline",
  definition = .getPipeline
)


#' @name getRegistry
#' @title Obtains the function registry
#' @details
#'      Obtains the function registry as a tibble, including both predefined and user defined functions
#' @return Tibble describing the registry
#' @family Package core functions
#' @examples
#' getRegistry()
#' @export

getRegistry <- function(){
  registry <- .analysisPipelinesEnvir$.functionRegistry
  return(registry)
}

#' @name getInput
#' @rdname getInput
#' @title Obtains the initializedInput
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details
#'      Obtains the input from the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @return Dataframe for an \code{AnalysisPipeline} & SparkDataFrame for a \code{StreamingAnalysisPipeline}
#' @family Package core functions
#' @examples
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' pipelineObj %>>% getInput
#' @export

setGeneric(
  name = "getInput",
  def = function(object)
  {
    standardGeneric("getInput")
  }
)

.getInput = function(object){
  return(object@input)
}

#' @rdname getInput
setMethod(
  f = "getInput",
  signature = "BaseAnalysisPipeline",
  definition = .getInput
)

#' @name getOutputById
#' @rdname getOutputById
#' @title Obtains a specific output
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @param reqId The position of the function for which the output is desired in the sequence of operations in the pipeline.
#' @param includeCall Logical which defines whether the call used to generate the output should be returned. By, default this is false
#' @details
#'      Obtains a specific output from the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object by passing the position
#'      of the function for which the output is desired, in the sequence of operations in the pipeline. This can be obtained by passing the number
#'      under the 'id' column in the pipeline table corresponding to the required function
#' @details This method is implemented on the base class as it is a shared functionality types of Analysis Pipelines
#' which extend this class
#' @return If includeCall = F, the output object generated by the function is returned
#' @return If includeCall = T, it is a list containing to elements
#'         - call: tibble with 1 row containing the function call for the output desired
#'         - output: output generated
#' @family Package core functions
#' @examples
#' \dontrun{
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' getNumRows <- function(dataset){
#'   return(nrow(dataset))
#' }
#' registerFunction("getNumRows")
#' pipelineObj %>>% getNumRows(storeOutput = TRUE) -> pipelineObj
#' pipelineObj %>>% generateOutput %>>% getOutputById("1")
#' }
#' @export

setGeneric(
  name = "getOutputById",
  def = function(object, reqId, includeCall = F)
  {
    standardGeneric("getOutputById")
  }
)

.getOutputById = function(object, reqId, includeCall = F){
  tryCatch({
      op <- list(call = data.frame(),
                 output = list())
      reqId <- as.character(reqId)
      object@pipeline %>>% dplyr::filter(.data$id == reqId) -> call
      if(call$storeOutput){
        object@output[[paste0("f", reqId, ".out")]] -> output
      }else{
        includeCall <- T
        output <- "The output of this function was not configured to be stored"
      }


      if(includeCall){
        op <- list(call = call,
                   output = output)
        return(op)
      }else{
        return(output)
      }

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })
}

#' @rdname getOutputById
setMethod(
  f = "getOutputById",
  signature = "BaseAnalysisPipeline",
  definition = .getOutputById
)

######################## Formula helper functions ############

#' @name getResponse
#' @title Obtains the response term from the formula
#' @param f formula from which term is to be extracted.
#' @details This is a helper function to extract the response variable from a formula
#' @return The response variable in the formula as a string
#' @examples
#' library(analysisPipelines)
#' getResponse(y ~ x1 + x2)
#' @export
getResponse <- function(f){
  resp <- dimnames(attr(stats::terms(f), "factors"))[[1]][1]
  return(resp)
}

#' @name getTerm
#' @title Obtains the dependency term from the formula
#' @param f formula from which term is to be extracted.
#' @details This is a helper function to extract the terms from a formula
#' @return String with the terms
#' @examples
#' library(analysisPipelines)
#' getTerm(y ~ x)
#' @export
getTerm <- function(f){
  t <- attr(stats::terms(f), "term.labels")
  return(t)
}

#' @name isDependencyParam
#' @title Checks if the parameter is the dependency parameter
#' @param f formula from which term is to be extracted.
#' @details This is a helper function to check if the formula provided is a dependency parameter,
#' as per the package's formula semantics, capturing function dependencies
#' @return Logical as to whether it is a dependency parameter
#' @examples
#' library(analysisPipelines)
#' isDependencyParam(~f1)
#' @export
isDependencyParam <- function(f){
  termRegexPattern <- "[f]|[:digit:]"
  t <- NULL
  isDepParam <- F
  numDepTerms <- 0
  t <- getTerm(f)

  if(!is.null(t)){
    if(length(t) == 1){
      depTerms <- grep(termRegexPattern, t)
    }
  }

  if(length(depTerms) > 0){
    isDepParam <- T
  }

  return(isDepParam)
}



######################## Execution helper functions ############

#' @name getUpstreamDependencies
#' @title Obtains upstream dependencies for \code{AnalysisPipeline} objects
#' @keywords internal
getUpstreamDependencies <- function(row){
  tryCatch({
    ## dependencies from parameters

    params <- unlist(row$parameters, recursive = F)
    # params <- row$parameters
    dep <- lapply(params, function(p){
      isDepParam <- F
      t <- NULL
      tId <- NA
      # p <- unlist(p)
      if(class(p) == "formula"){
        isDepParam <- isDependencyParam(p)
      }
      if(isDepParam){
        # Dependency param
        t <- getTerm(p)
        tId <- as.numeric(gsub(pattern = "f", replacement = "", t))
      }
      return(tId)
    })

    ## Dependencies from outAsIn
    if(row$outAsIn && row$id != "1"){
      dep <- c(dep, as.character(as.numeric(row$id) - 1))
    }


    dep <- dep[which(!sapply(dep, is.na))]
    dep <- paste(unique(dep), sep = ",")

    return(dep)

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })
}


#' @name setUpstreamDependencies
#' @title Sets upstream dependencies for the entire pipeline
#' @keywords internal
setUpstreamDependencies <- function(pipeline){
  tryCatch({
    pipeline %>>% apply(MARGIN = 1, FUN = getUpstreamDependencies) -> upstreamDependenciesList
    if(length(upstreamDependenciesList) == 0){
      upstreamDependenciesList <- lapply(pipeline$id, function(x){
        x <- list(NA)
        x <- x[which(!sapply(x, is.na))]
        x <- paste(unique(x), sep = ",")
        return(x)
      })
    }
    pipeline %>>% dplyr::mutate(dependencies = upstreamDependenciesList) -> pipeline
    return(pipeline)

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })
}


### Graph edges
#' @name computeEdges
#' @title Computes edges (dependencies) in a pipeline given the joined tibble of the pipeline and registry
#' @keywords internal
computeEdges <- function(pipelineRegistryJoin){
  tryCatch({
    edgesDf <- dplyr::tibble(from = character(),
                             to = character())
    pipelineRegistryJoin %>>% apply(MARGIN = 1, FUN = function(x, ...){
      edges <- list()

      if(length(x$dependencies) != 0){
        id <- as.character(x$id)
        parents <- unlist(strsplit(x$dependencies, ","))
        edges <- lapply(parents, function(x, ...){
          edge <- list(from = x, to = id)
          return(edge)
        }, id = id)

        edges <- dplyr::bind_rows(edges)
      }

      return(edges)
    }) %>>% dplyr::bind_rows(.data) -> edgesDf

    if(nrow(edgesDf) == 0 && ncol(edgesDf) == 0){
      edgesDf <- dplyr::tibble(from = character(),
                        to = character())
    }else{
      edgesDf %>>% dplyr::distinct(.data$from, .data$to, .keep_all = TRUE) -> edgesDf
    }
    return(edgesDf)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })
}

## Starting points
#' @name getStartingPoints
#' @title Obtains starting nodes in a graph given nodes and edges
#' @keywords internal
getStartingPoints <- function(nodes, edgeDf){
  startingPoints <- setdiff(nodes, unique(edgeDf$to))
  return(startingPoints)
}

## End points
#' @name getEndPoints
#' @title Obtains end nodes in a graph given nodes and edges
#' @keywords internal
getEndPoints <- function(nodes, edgeDf){
  endPoints <- setdiff(unique(edgeDf$to), unique(edgeDf$from))
  return(endPoints)
}

### Topological levels

#' @name identifyTopLevelRecursively
#' @title Recursive function to identify the toplogical levels of the functions in a pipeline
#' @keywords internal
identifyTopLevelRecursively <- function(input = list(topDf = dplyr::tibble(),
                                                     nodes = c(),
                                                     edgeDf = dplyr::tibble(),
                                                     level = 1)){
  topDf <- input$topDf
  nodes <- input$nodes
  edgeDf <- input$edgeDf
  l <- input$level

  if(nrow(edgeDf) == 0){
    topDf %>>% dplyr::bind_rows(dplyr::bind_cols(id = nodes, level = rep(as.character(l), length(nodes)))) -> topDf
    output <- list(topDf = topDf,
                   nodes = nodes,
                   edgeDf = edgeDf,
                   level = l)
    return(output)
  }else{
    startingPoints <- getStartingPoints(nodes, edgeDf)
    topDf %>>% dplyr::bind_rows(dplyr::bind_cols(id = startingPoints, level = rep(as.character(l), length(startingPoints)))) -> topDf
    edgeDf %>>% dplyr::filter(!(.data$from %in% startingPoints)) -> edgeDf
    nodes %>>% setdiff(startingPoints) -> nodes

    output <- list(topDf = topDf,
                   nodes = nodes,
                   edgeDf = edgeDf,
                   level = l + 1)
    return(identifyTopLevelRecursively(output))
  }
}


#' @name identifyTopologicalLevels
#' @title Identifies the topological levels of the functions in a pipeline
#' @keywords internal
identifyTopologicalLevels <- function(
                                      nodes = c(),
                                      edgeDf = dplyr::tibble(),
                                      topDf = dplyr::tibble(id = character(),
                                                            level = character()),
                                      level = 1){

  tryCatch({
    input <- list(topDf = topDf,
                  nodes = nodes,
                  edgeDf = edgeDf,
                  level = level)
    topDf <- identifyTopLevelRecursively(input)$topDf
    return(topDf)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })
}

####################### Execution prep #############################

#' @name prepExecution
#' @rdname prepExecution
#' @title Prepare the pipleline for execution
#' @param object A Pipeline object
#' @details The pipeline is prepared for execution by identifying the graph of the pipeline as well as its topological ordering,
#' and dependency map in order to prepare for execution
#' @return Updated \code{AnalysisPipeline} \code{StreamingAnalysisPipeline} object
#' @family Package core functions
#' @examples
#' \dontrun{
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' pipelineObj %>>% univarCatDistPlots(uniCol = "Species",
#'  priColor = "blue", optionalPlots = 0, storeOutput = T) %>>%
#' prepExecution -> pipelineObj
#' }
#' @export prepExecution

setGeneric(
  name = "prepExecution",
  def = function(object)
  {
    standardGeneric("prepExecution")
  }
)

.prepExecution <- function(object){
  tryCatch({

    object %>>% initializeLoggers
    startPipelinePrep <- Sys.time()
    futile.logger::flog.info(msg = "||  Pipeline Prep. STARTED  ||", name='logger.prep')


    object@pipeline$dependencies <- rep(NA, nrow(object@pipeline))
    object@pipeline %>>% setUpstreamDependencies -> object@pipeline #Parents

    pipelineRegistryJoin <- dplyr::left_join(object@pipeline, getRegistry(), by = c("operation" = "functionName"))

    pipelineRegistryJoin %>>% computeEdges -> edgeDf
    pipelineRegistryJoin$id %>>% as.character %>>% getStartingPoints(edgeDf) -> startingPoints
    nodes <- as.character(pipelineRegistryJoin$id)

    topOrdering <- identifyTopologicalLevels(nodes, edgeDf)
    object@pipelineExecutor$topologicalOrdering <- topOrdering
    object@pipelineExecutor$dependencyLinks <- edgeDf

    endPipelinePrep <- Sys.time()
    prepTime <- endPipelinePrep - startPipelinePrep
    futile.logger::flog.info(msg = "||  Pipeline Prep. COMPLETE. Time taken : %s seconds||", prepTime, name='logger.prep')

    return(object)

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })
}

#' @rdname prepExecution
setMethod(
  f = "prepExecution",
  signature = "BaseAnalysisPipeline",
  definition = .prepExecution
)

#' @name visualizePipeline
#' @rdname visualizePipeline
#' @title Visualizes the pipeline as a graph
#' @details Indicates dependencies amongst functions as well as functions for which output
#' needs to be stored
#' @param object The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object
#' @return A graph object which can be printed (or) plotted to visualize the pipeline
#' @family Package core functions
#' @examples
#' \dontrun{
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' pipelineObj %>>% univarCatDistPlots(uniCol = "Species",
#'  priColor = "blue", optionalPlots = 0, storeOutput = T) %>>%
#' visualizePipeline
#' }
#' @export

setGeneric(
  name = "visualizePipeline",
  def = function(object)
  {
    standardGeneric("visualizePipeline")
  }
)

.visualizePipeline <- function(object){
  tryCatch({
    if(nrow(object@pipelineExecutor$dependencyLinks) == 0){
      object %>>% prepExecution -> object
    }

    #Logos
    rLogo <- paste('data:image/png;base64',
                   RCurl::base64Encode(readBin(system.file("r-logo.png", package = "analysisPipelines"),
                                         'raw',
                                         file.info(system.file("r-logo.png", package = "analysisPipelines"))[1, 'size']),
                                 'txt'), sep = ',')
    pythonLogo <-  paste('data:image/png;base64',
                         RCurl::base64Encode(readBin(system.file("python-logo.png", package = "analysisPipelines"),
                                              'raw',
                                              file.info(system.file("python-logo.png", package = "analysisPipelines"))[1, 'size']),
                                      'txt'), sep = ',')
    sparkLogo <-  paste('data:image/png;base64',
                        RCurl::base64Encode(readBin(system.file("spark-logo.png", package = "analysisPipelines"),
                                             'raw',
                                             file.info(system.file("spark-logo.png", package = "analysisPipelines"))[1, 'size']),
                                     'txt'), sep = ',')

    sparkSsLogo <-  paste('data:image/png;base64',
                          RCurl::base64Encode(readBin(system.file("spark-structured-streaming-logo.png", package = "analysisPipelines"),
                                                      'raw',
                                                      file.info(system.file("spark-structured-streaming-logo.png", package = "analysisPipelines"))[1, 'size']),
                                              'txt'), sep = ',')

    dataLogo <- paste('data:image/png;base64',
                      RCurl::base64Encode(readBin(system.file("data-icon.png", package = "analysisPipelines"),
                                                  'raw',
                                                  file.info(system.file("data-icon.png", package = "analysisPipelines"))[1, 'size']),
                                          'txt'), sep = ',')
    paramLogo <- paste('data:image/png;base64',
                      RCurl::base64Encode(readBin(system.file("param-icon.png", package = "analysisPipelines"),
                                                  'raw',
                                                  file.info(system.file("param-icon.png", package = "analysisPipelines"))[1, 'size']),
                                          'txt'), sep = ',')
    outputLogo <- paste('data:image/png;base64',
                      RCurl::base64Encode(readBin(system.file("output-icon.png", package = "analysisPipelines"),
                                                  'raw',
                                                  file.info(system.file("output-icon.png", package = "analysisPipelines"))[1, 'size']),
                                          'txt'), sep = ',')

    node_df <-dplyr::left_join(object@pipeline, getRegistry(),
                                                       by = c("operation" = "functionName")) %>>%
               dplyr::left_join(object@pipelineExecutor$topologicalOrdering, by = c("id" = "id"))
    edge_df <- object@pipelineExecutor$dependencyLinks


    storedOutputs <- node_df %>>% dplyr::filter(.data$storeOutput == T)
    storedOutputs <- storedOutputs$id

    spData <- node_df %>>% dplyr::filter(.data$isDataFunction == T)
    spData <- spData %>>% dplyr::filter(.data$level == min(as.numeric(.data$level)))
    spDataIds <- spData$id

    spParam <-node_df %>>% dplyr::filter(.data$isDataFunction == F)
    spParam <- spParam %>>% dplyr::filter(.data$level == min(as.numeric(.data$level)))
    spParamIds <- spParam$id

    node_df %>>% dplyr::mutate(image = ifelse(.data$engine == "r", rLogo,
                                              ifelse(.data$engine == "spark", sparkLogo,
                                                     ifelse(.data$engine == 'spark-structured-streaming', sparkSsLogo,
                                                        pythonLogo)))) -> node_df
    node_df$shape <- "image"

    # node_df %>>% dplyr::mutate(group = ifelse(storeOutput == T, "Stored output", "Auxiliary step"))
    node_df$group <- "function"
    node_df %>>%
      dplyr::select(.data$id, .data$operation, .data$group, .data$shape, .data$image) -> node_df

    colnames(node_df) <- c("id", "label", "group","shape", "image")

    node_df %>>% dplyr::add_row(id = "d0", label = "Data", group = "data", shape = "image",
                                image = dataLogo ) -> node_df

    for(o in storedOutputs){
      node_df %>>% dplyr::add_row(id = paste0("o",o), label = paste("Output ID:", o ), group = "output",
                                  shape = "image",
                                  image = outputLogo) -> node_df
    }


    #Starting points
    for(s in spDataIds){
      edge_df %>>% dplyr::add_row(from = "d0", to = s) -> edge_df
    }

    for(s in spParamIds){
      pId <- paste0("p", s)
      node_df %>>% dplyr::add_row( id = pId, label = "Non-data parameter", group = "parameter",
                                  shape = "image",
                                  image = paramLogo ) -> node_df
      edge_df %>>% dplyr::add_row(from = paste0("p", s), to = s) -> edge_df
    }

    for(e in storedOutputs){
      edge_df %>>% dplyr::add_row(from = e , to = paste0("o",e)) -> edge_df
    }

    lnodes <- data.frame(label = c("Stored output", "Auxiliary step"),
                         shape = c("dot"), color = c("#A1AEFF","#ff4d4d"),
                         title = "Pipeline")


    vis <- visNetwork::visNetwork(node_df, edge_df, width = "100%") %>%
      # visNetwork::visGroups(groupname = "Stored output", color = "#A1AEFF", shadow=T) %>%
      # visNetwork::visGroups(groupname = "Auxiliary step", color = "#ff4d4d", shadow=T) %>%
      # visNetwork::visLegend(addNodes = lnodes, position = "right", ncol = 4,
      #                       zoom = F, useGroups = F)%>%
      visNetwork::visNodes(font = list(size =18)) %>%
      visNetwork::visEdges(arrows=list(to=list(enabled = T, scaleFactor = 0.25)),
                           widthConstraint = 1.2,
                           length = c(3)) %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visNetwork::visLayout(randomSeed = 6, improvedLayout = T) %>%
      # to stop auto update and making nodes sticky
      visNetwork::visPhysics(enabled = F) %>%
      visNetwork::visInteraction(navigationButtons = TRUE)

    return(vis)

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })


}

#' @rdname visualizePipeline
setMethod(
  f = "visualizePipeline",
  signature = "BaseAnalysisPipeline",
  definition = .visualizePipeline
)



########### Changing generics ############################################


#' @name generateOutput
#' @rdname generateOutput
#' @title Generate a list of outputs from Pipeline objects
#' @details \code{generateOutput} is a generic function that is implemented for various types of pipeline objects
#' such as \code{AnalysisPipeline} and \code{StreamingAnalysisPipeline}
#' @details
#'       The sequence of operations stored in the pipeline object
#'       are run and outputs generated, stored in a list
#' @param object object that contains input, pipeline, registry and output
#' @return Updated Pipeline object with the outputs at each step stored in the \code{output} slot.
#' @return Specific outputs can be obtained by using the \link{getOutputById} function
#' @family Package core functions
#' @include core-functions.R
#' @exportMethod generateOutput

setGeneric(
  name = "generateOutput",
  def = function(object)
  {
    standardGeneric("generateOutput")
  }
)


#' @name checkSchemaMatch
#' @rdname checkSchemaMatch
#' @title Checks the schema of the input to a Pipeline object against the original
#' @param object A Pipeline object
#' @param newData The newData that the pipeline is to be initialized with
#' @details Checks the schema of the new data frame that the pipeline is to be initialized with against
#'          the original schema that the pipeline was saved with. Provides a detailed comparison
#' @return Returns a list with details on added columns, removed columns, comparison between column classes, and a logical
#'         whether the schema has remained the same from the old dataframe to the new one
#' @family Package core functions
#' @exportMethod checkSchemaMatch
setGeneric(
  name = "checkSchemaMatch",
  def = function(object, newData)
  {
    standardGeneric("checkSchemaMatch")
  }
)

######### Logging functions ###################



#' @name setLoggerDetails
#' @rdname setLoggerDetails
#' @title Sets the logger configuration for the pipeline
#' @details This function sets the logger configuration for the pipeline.
#' @param object A Pipeline object
#' @param target A string value. 'console' for appending to console, 'file' for appending to a file, or 'console&file' for both
#' @param targetFile File name of the log file in case the target is 'file'
#' @param layout Specify the layout according to 'futile.logger' package convention
#' @family Package core functions
#' @examples
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' pipelineObj %>>% setLoggerDetails(target = "file",
#'  targetFile = "pipeline.out") -> pipelineObj
#' @export

setGeneric(
  name = "setLoggerDetails",
  def = function(object, target = 'console',
                          targetFile = 'pipelineExecution.out',
                          layout = 'layout.simple'){
    standardGeneric("setLoggerDetails")
  }
)

.setLoggerDetails <- function(object,target = 'console',
                                      targetFile = 'pipelineExecution.out',
                                      layout = 'layout.simple'){
  loggerDetails <- list( target = target,
                         targetFile = targetFile,
                         layout = layout)
  object@pipelineExecutor$loggerDetails <- loggerDetails
  return(object)
}

#' @rdname setLoggerDetails
setMethod(
  f = "setLoggerDetails",
  signature = "BaseAnalysisPipeline",
  definition = .setLoggerDetails
)


#' @name getLoggerDetails
#' @rdname getLoggerDetails
#' @title Obtains the logger configuration for the pipeline
#' @details This function obtains the logger configuration for the pipeline.
#' @param object A Pipeline object
#' @return Logger configuration as a list
#' @family Package core functions
#' @examples
#' library(analysisPipelines)
#' pipelineObj <- AnalysisPipeline(input = iris)
#' pipelineObj %>>% getLoggerDetails
#' @export
setGeneric(
  name = "getLoggerDetails",
  def = function(object){
    standardGeneric("getLoggerDetails")
  }
)

.getLoggerDetails <- function(object){
  return(object@pipelineExecutor$loggerDetails )
}

#' @rdname getLoggerDetails
setMethod(
  f = "getLoggerDetails",
  signature = "BaseAnalysisPipeline",
  definition = .getLoggerDetails
)

#' @name initializeLoggers
#' @title intializes the loggers with the required appenders and layout based on the provided configuration
#' @keywords internal
initializeLoggers <- function(object){

  appender.fn <- futile.logger::appender.console()
  # Define target
  futile.logger::flog.threshold(futile.logger::INFO)
  fileName <- object@pipelineExecutor$loggerDetails$targetFile
  if(object@pipelineExecutor$loggerDetails$target == 'file'){
    appender.fn <- futile.logger::appender.file(fileName)
  }else if(object@pipelineExecutor$loggerDetails$target == 'console&file'){
    appender.fn <- futile.logger::appender.tee(fileName)
  }else if(object@pipelineExecutor$loggerDetails$target == 'none'){
    futile.logger::flog.threshold(futile.logger::FATAL)
  }

  #TODO: pass layout as parameter
  # layout <- layout.simple()

  futile.logger::flog.appender(appender.fn)

}

######## Auxiliary functions ############################################

#' @name genericPipelineException
#' @title Default exception for pipeline functions
#' @details This functions defines the default function which will be called in case of an exception occurring while
#' executing any of the pipeline functions. While a function is registered, a custom function to deal with exceptions
#' incurred during the call of the function being registered can be passed by the user. If passed, the custom function
#' will be called instead of this function
#' @param error Error encountered during the execution of a particular pipeline function
#' @family Package core functions
#' @export
genericPipelineException <- function(error){
  message <- error$message
  m <- paste0("||  EXCEPTION OCCURED WHILE RUNNING THE PIPELINE FUNCTION WITH PROVIDED PARAMETERS: ", message, "  ||")
  futile.logger::flog.error(m, name = 'logger.func')
  stop(m)
}

#' @name loadPipeline
#' @title Loads the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object from the file system
#' @details
#'       The \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object is loaded into the file system from the file system
#'       based on the path specified.
#' @details Optionally, the \code{input} parameter can be provided to
#'       initialize the \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object with an R data frame
#'       or Streaming Spark DataFrame (in case of \code{StreamingAnalysisPipeline} object) present in the R session.
#' @details Another provided option, is to specify a filePath where the input dataset is present (in a .CSV format)
#'       and the object will be initialized with this data frame. The \code{filePath} parameter takes precedence over
#'       \code{input} parameter. This is applicable only from \code{AnalysisPipeline} objects
#' @details Note - When a pipeline is loaded, the existing registry is overwritten with the registry saved with the
#' pipeline
#' @param path the path at which the .Rds file containing the pipeline is located
#' @param input (optional) data frame with which the pipeline object should be initialized
#' @param filePath (optional) path where a dataset in .CSV format is present which is to be loaded
#' @return An \code{AnalysisPipeline} or \code{StreamingAnalysisPipeline} object, optinally initialized with the data frame provided
#' @family Package core functions
#' @examples
#' \dontrun{
#'   library(analysisPipelines)
#'   loadPipeline(path = "./pipeline.RDS")
#' }
#' @export

loadPipeline <- function(path, input = data.frame() , filePath = ""){
  tryCatch({

    futile.logger::flog.warn("||  The existing registry will be overwritten with the registry from the RDS file  ||",
                             name = "logger.base")
    load(path, envir = environment())
    #load(path, envir = testEnv)
    functionNames = setdiff(ls(envir = environment()), c("path", "object", "input", "filePath", ".registry"))
    #functionNames = setdiff(ls(envir = testEnv), c("path", "object", "input", "filePath", ".registry"))

    lapply(functionNames, function(x){
      assign(x, get(x, environment()), globEnv)
      #assign(x, get(x, testEnv), .GlobalEnv)
    })

    eval(parse(text = paste0(".setRegistry(.registry)")))

    futile.logger::flog.info("||  Registry loaded succesfully  ||",
                             name = "logger.base")


    input <- initDfBasedOnType(input, filePath)
    schemaCheck <- object %>>% checkSchemaMatch(input)
    if(!schemaCheck$isSchemaSame){
      if(length(schemaCheck$removedColumns) > 0){
        m <- paste0("||  Some columns which were present in the original schema ",
                    "for the pipeline, ",
                    "are not present in the new data frame. Some pipeline functions ",
                    "may not execute as expected. Use the checkSchemaMatch function to obtain ",
                    "a detailed comparison  ||")
        futile.logger::flog.warn(m, name = 'logger.pipeline')
        warning(m)
      }

      if(length(schemaCheck$addedColumns) > 0){
        m <- paste0("||  Some new columns have been added to the new data frame ",
                    "as compared to the original schema for the pipeline. ",
                    "Use the checkSchemaMatch function to obtain ",
                    "a detailed comparison  ||")
        futile.logger::flog.warn(m, name = 'logger.pipeline')
        warning(m)
      }

      if(length(schemaCheck$addedColumns) == 0 && length(schemaCheck$removedColumns) == 0){
        m <- paste0("||  Colummn names are the same but types have changed",
                    "Some pipeline functions may not execute as expected. ",
                    "Use the checkSchemaMatch function to obtain ",
                    "a detailed comparison  ||")
        futile.logger::flog.warn(m, name = 'logger.pipeline')
        warning(m)
      }

    }

    object@input <- input
    return(object)
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}


#' @name initDfBasedOnType
#' @title initializes the \code{AnalysisPipeline} object with the input based on the provided type
#' @details
#'      Transforms provided inputs into R data frame regardless of the input provided, be it Spark DataFrames
#'      or Python data frames
#' @param input Input dataframe
#' @param filePath File path where the .csv file is stored
#' @return \code{AnalysisPipeline} object initialized with input
#' @family Package core functions
#' @keywords internal

initDfBasedOnType <- function(input, filePath){

  tryCatch({
    if(filePath == ""){
      if(!all(dim(input) == c(0,0))){
        #Check for R, Spark, Python data frame
        if(class(input) == "SparkDataFrame"){
          if("SparkR" %in% installed.packages()){
            input <- SparkR::as.data.frame(input)
          }else{
            futile.logger::flog.error(paste0("||  'SparkR' is not installed. Please install before initializing the pipeline",
                                      " with a SparkDataFrame  ||"),
                                      name = 'logger.pipeline')
            stop()
          }
        }else if(any(class(input) == "pandas.core.frame.DataFrame")){
          if("reticulate" %in% installed.packages()){
            input <- reticulate::py_to_r(input)
          }else{
           futile.logger::flog.error(paste0("||  'reticulate' is not installed. Please install before initializing the pipeline",
                                           " with a Pandas DataFrame  ||"),
                                    name = 'logger.pipeline')
            stop()
          }
        }else if(any(class(input) %in% c("data.frame", "tibble"))){
          # do nothing for R - Check is required so that the exception is not thrown
        } else{
          m <- "||  The provided input is not of class - data.frame, SparkDataFrame or Pandas DataFrame  ||"
          futile.logger::flog.error(m, name = 'logger.pipeline')
          stop()
        }
      }
    }
    else{
      input <- utils::read.csv(filePath)
    }

    return(input)

  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
  stop()
  })
}


#' @name saveRegistry
#' @title Saves the registry to the file system
#' @param path path on the file system, where the registry is to be saved to
#' @details This function saves  the existing function registry and associated function definition loaded in the
#' environment into a file.
#' @family Package core functions
#' @examples
#' \dontrun{
#'   library(analysisPipelines)
#'   saveRegistry(path = "./registry.RDS")
#' }
#' @export
saveRegistry <- function(path){
    tryCatch({
      .registry <- getRegistry()
      listToBeSaved <- c(".registry", getRegistry()$functionName, getRegistry()$exceptionHandlingFunction)
      save(list = listToBeSaved,file = path)
    },error = function(e){
      futile.logger::flog.error(e, name = "logger.base")
      stop()
    })
}


#' @name loadRegistry
#' @title Loads a function registry from a file
#' @param path path on the file system, where the registry is to be loaded from
#' @details This function loads a function registry and associated function definition stored in an RDS file into the
#' environment. The existing registry is overwritten with the newly loaded registry
#' @family Package core functions
#' @examples
#' \dontrun{
#'   library(analysisPipelines)
#'   loadRegistry(path = "./registry.RDS")
#' }
#' @export
loadRegistry <- function(path){
  tryCatch({
    futile.logger::flog.warn("||  The existing registry will be overwritten with the registry from the RDS file  ||",
                             name = "logger.base")

    load(path, envir = environment())
    functionNames = setdiff(ls(envir = environment()), c("path", ".registry"))

    eval(parse(text = paste0(".setRegistry(.registry)")))

    lapply(functionNames, function(x){
      assign(x, get(x, environment()), globEnv)
    })


    futile.logger::flog.info("||  Registry loaded succesfully  ||",
                             name = "logger.base")
  },error = function(e){
    futile.logger::flog.error(e, name = "logger.base")
    stop()
  })
}


