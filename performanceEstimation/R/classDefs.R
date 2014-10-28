################################################################# 
## THIS FILE CONTAINS THE CLASSES AND RESPECTIVE CONSTRUCTORS   #
## THAT ARE DEFINED IN THIS PACKAGE                             #
#################################################################
## Author : Luis Torgo (ltorgo@dcc.fc.up.pt)     Date: Nov 2013 #
## License: GPL (>= 2)                                          #
#################################################################


setClassUnion("StrOrDF",c("character","data.frame"))
setClassUnion("OptList",c("list","NULL"))
setClassUnion("OptMatrix",c("matrix","NULL"))
setClassUnion("OptString",c("character","NULL"))
setClassUnion("NameOrCall",c("call","name"))

## ==============================================================
## CLASS: PredTask
##
## Class for storing information concerning a prediction task
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("PredTask",
         slots=c(formula="formula",
                 dataSource="NameOrCall",
                 taskName="character",
                 type="character",
                 target="character")
         )


## --------------------------------------------------------------
## constructor
##
PredTask <- function(form,data,taskName=NULL,type=NULL) {
  if (missing(form) || missing(data))
    stop('\nYou need to provide a formula and a data frame name.\n',call.=FALSE)
  if (is.data.frame(data)) data <- substitute(data)
  if (inherits(try(mf <- model.frame(form,eval(data),na.action=NULL),TRUE),"try-error"))
#  if (is.data.frame(data)) data <- deparse(substitute(data))
#  if (inherits(try(mf <- model.frame(form,get(data),na.action=NULL),TRUE),"try-error"))
    stop('\nInvalid formula for the given data frame.\n',call.=FALSE)

  tgt <- deparse(form[[2]])
  if (is.null(taskName)) {
    m <- match.call()
    taskName <- paste(data,tgt,sep=".")
  }
  
  if (is.null(type)) {
      taskType <- if (is.factor(eval(data)[,tgt])) "class" else "regr"
  } else {
      if (!(type %in% c("class","regr","ts")))
          stop(paste("PredTask::",type,"tasks not implemented."),call.=FALSE)
      taskType <- type
  }

  if (taskType == "ts" && !is.numeric(eval(data)[[tgt]]))
      stop("PredTask:: time series task should have numeric target.",call.=FALSE)
  
  new("PredTask",
      formula=form,dataSource=data,taskName=taskName,
      type=taskType,
      target=deparse(form[[2]]))
}



## ==============================================================
## CLASS: Workflow
##
## Class for storing a workflow to solve a predictive task
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("Workflow",
         slots=c(name="character",
                 func="character",
                 pars="list",
                 deps="OptList"))



## --------------------------------------------------------------
## Constructor
##

Workflow <- function(wf, ..., wfID, deps=NULL) {

    wf.pars <- list(...)
    
    ## if no ID was provided then it is one of the standard workflows
    if (missing(wf)) {
        if ("type" %in% names(wf.pars)) {
            n <- paste(wf.pars[["learner"]],wf.pars[["type"]],sep='.')
            f <- "timeseriesWF"
        } else {
            n <- wf.pars[["learner"]]
            f <- "standardWF"
        }
    ## using a user-defined workflow
    } else {
        n <- f <- wf
    }
    if (!missing(wfID)) n <- wfID
    
    new("Workflow", name=n, func=f, pars=wf.pars, deps=deps)

}


## ==============================================================
## CLASS: WFoutput
##
## A class for storing the information resulting from applying a
## workflow to a predictive task
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("WFoutput",
         slots=c(predictions   = "data.frame",  # a data.frame nTest x 2 (true,pred)
                 extraInfo     = "list")        # list of whatever info the wf author wants
)


## --------------------------------------------------------------
## Constructor
##
WFoutput <- function(rIDs,ts,ps,e=list()) {
  o              <- new("WFoutput")

  if (!is.null(dim(ps))) {
      preds <- colnames(ps)[apply(ps,1,which.max)]
      probs <- ps
  } else {
      preds <- ps
      probs <- NULL
  }
  if (length(preds) != length(ts)) {
      warning("WFoutput:: less predictions than test cases, filling with NAs.")
      t <- ts
      t[] <- NA
      t[names(ps)] <- preds
      preds <- t
  }
  o@predictions  <- data.frame(row.names=rIDs,true=ts,predicted=preds)
  if (!is.null(probs)) o@predictions <- cbind(o@predictions,probs)
  o@extraInfo    <- e
  o
}


## ==============================================================
## CLASS: EstCommon
##
## A class containing the common estimation settings
## ==============================================================
setClass("EstCommon",
         slots=c(seed='numeric',         # seed of the random generator
                 dataSplits='OptList')   # user supplied data splits
         )



## ==============================================================
## CLASS: CV
##
## A class containing the settings of a cross validation experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("CV",
         slots=c(nReps='numeric',      # nr. of repetitions
                 nFolds='numeric',     # nr. of folds of each rep.
                 strat='logical'),     # is the sampling stratified?
         contains="EstCommon"
         )


## --------------------------------------------------------------
## constructor
##
CV <- function(nReps=1,nFolds=10,
                       seed=1234,strat=FALSE,
                       dataSplits=NULL) {
    new("CV",
        nReps=nReps,
        nFolds=if (is.null(dataSplits)) nFolds else length(dataSplits)/nReps,
        seed=seed,strat=strat,dataSplits=dataSplits)
}


## ==============================================================
## CLASS: Holdout
##
## A class containing the settings of a holdout experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("Holdout",
         slots=c(nReps='numeric', # number of repetitions
                 hldSz='numeric', # the size (0..1) of the holdout
                 strat='logical'),# is the sampling stratified?
         contains="EstCommon"
         )


## --------------------------------------------------------------
## Constructor
##
Holdout <- function(nReps=1,hldSz=0.3,
                        seed=1234,strat=FALSE,
                        dataSplits=NULL) {
    new("Holdout",
        nReps=if (is.null(dataSplits)) nReps else length(dataSplits),
        hldSz=if (is.null(dataSplits)) hldSz else length(dataSplits[[1]]),
        seed=seed,strat=strat,dataSplits=dataSplits)
}


## ==============================================================
## CLASS: LOOCV
##
## A class containing the settings of a leave one out cross validation
## experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("LOOCV",
         contains="EstCommon"
         )


## --------------------------------------------------------------
## constructor
LOOCV <- function(seed=1234,dataSplits=NULL)
  new("LOOCV",
      seed=seed,
      dataSplits=dataSplits)



## ==============================================================
## CLASS: Bootstrap
##
## A class containing the settings of a boostrap experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("Bootstrap",
         slots=c(type='character', # type of boostrap ("e0" or ".632")
                 nReps='numeric'), # number of repetitions
         contains="EstCommon"
         )


## --------------------------------------------------------------
## constructor
##
Bootstrap <- function(type='e0',nReps=200,seed=1234,dataSplits=NULL) {
     new("Bootstrap",
         type=type,
         nReps=if (is.null(dataSplits)) nReps else length(dataSplits),
         seed=seed,dataSplits=dataSplits)
}



## ==============================================================
## CLASS: MonteCarlo
##
## A class containing the settings of a monte carlo experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("MonteCarlo",
         slots=c(nReps='numeric',
                 szTrain='numeric',
                 szTest='numeric'),
         contains="EstCommon"
         )


## --------------------------------------------------------------
## constructor
##
MonteCarlo <- function(nReps=10,szTrain=0.25,szTest=0.25,
                       seed=1234,dataSplits=NULL)
    new("MonteCarlo",
        nReps= if (is.null(dataSplits)) nReps else length(dataSplits),
        szTrain=szTrain,szTest=szTest,
        seed=seed,dataSplits=dataSplits)



## ==============================================================
## CLASS UNION: EstimationMethod
##
## A class encapsulating all types of Estimation Experiments
## ==============================================================

setClassUnion("EstimationMethod",
              c("CV", "MonteCarlo", "Holdout",
                "LOOCV","Bootstrap"))





## ==============================================================
## CLASS: EstimationTask
##
## A class containing the information on a estimation task, i.e.
## the metrics (and eventually the function to calculate them) and
## the estimation methodology to use
## ==============================================================
setClass("EstimationTask",
         slots=c(metrics='character',        # the metrics to be estimated
                 evaluator='character',      # function used to calculate the metrics
                 evaluator.pars='OptList',   # pars to this function
                 method="EstimationMethod"   # the estimation method to use
         )
         )

## --------------------------------------------------------------
## constructor
##
EstimationTask <- function(metrics,
                           evaluator="",evaluator.pars=NULL,
                           method=CV()) {
    new("EstimationTask",
        metrics=metrics,
        evaluator=evaluator,evaluator.pars=evaluator.pars,
        method=method)
}


    
## ==============================================================
## CLASS: EstimationResults
##
## A class containing the results of a single experiment, i.e. the
## the estimation results of applying a single workflow to a single
## predictive task
## ==============================================================


## --------------------------------------------------------------
## Constructor
##
setClass("EstimationResults",
         slots=c(task             = "PredTask",
                 workflow         = "Workflow",
                 estTask          = "EstimationTask",
                 iterationsScores = "matrix",   # nIts x nStats
                 iterationsInfo   = "list"      # list of nIts lists with comps preds, info and train
                )
         )


## --------------------------------------------------------------
## constructor
##
EstimationResults <- function(t,w,et,sc,e) {
  o                  <- new("EstimationResults")
  o@task             <- t
  o@workflow         <- w
  o@estTask          <- et
  o@iterationsScores <- sc
  ## classification tasks, code back predictions to class labels
  if (is.factor(model.response(model.frame(t@formula,eval(t@dataSource))))) {
      for (i in 1:length(e))
          e[[i]]$preds[,"predicted"] <- factor(e[[i]]$preds[,"predicted"],levels=levels(responseValues(t@formula,eval(t@dataSource))))
  }
  o@iterationsInfo  <- e
  o
}



## ==============================================================
## CLASS: ComparisonResults
##
## A class containing the results of a full experimental comparison,
## i.e. the  estimation results of applying several workflows to 
## several predictive tasks
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("ComparisonResults",contains="list")

## --------------------------------------------------------------
## constructor
##
ComparisonResults <- function(t) new("ComparisonResults",t)


