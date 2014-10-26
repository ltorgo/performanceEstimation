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
         representation(taskName="character",
                        formula="formula",
#                        dataSource="StrOrDF",
                        dataSource="NameOrCall",
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
      taskName=taskName,formula=form,dataSource=data,
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
         representation(name="character",
                        func="character",
                        pars="list",
                        deps="OptList"))



## --------------------------------------------------------------
## Cnstructor
##
#Workflow <- function(func,...,wfID=func) {
#    if (missing(func)) stop("\nYou need to provide the name of a function imple#menting the workflow.\n")
#    fn <- if (is(func,"function")) deparse(substitute(func)) else func
#    do.call("new",list("Workflow",name=wfID,func=fn,pars=list(...)))
#}

Workflow <- function(wfID,
                     learner,learner.pars=NULL,
                     type,relearn.step=1,
                     user,user.pars=NULL,
                     predictor="predict",predictor.pars=NULL,
                     pre=NULL,pre.pars=NULL,
                     post=NULL,post.pars=NULL,
                     fullOutput=FALSE,
                     deps=NULL
                     ) {
    if (missing(learner) && missing(user)) stop("\nYou need to provide either the name of a learner or of a user-defined workflow.\n",call.=FALSE)

    if (!missing(learner) && is(learner,"function"))
        learner <- deparse(substitute(learner)) 
    if (!missing(user) && is(user,"function"))
        user <- deparse(substitute(user))
     if (!missing(predictor) && is(predictor,"function"))
         predictor <- deparse(substitute(predictor))
    if (!missing(pre) && is(pre,"function"))
        pre <- deparse(substitute(pre))
    if (!missing(post) && is(post,"function"))
        post <- deparse(substitute(post))

    if (!missing(learner)) {  # no user-defined workflow
        if (missing(type)) {  # standard workflow
            new("Workflow",
                name= if (missing(wfID) || wfID %in% c("standardWF","timeseriesWF")) learner else wfID,
                func= "standardWF",
                pars=list(learner=learner,learner.pars=learner.pars,
                    predictor=predictor,predictor.pars=predictor.pars,
                    pre=pre,pre.pars=pre.pars,post=post,post.pars=post.pars,
                    .fullOutput= fullOutput),
                deps=deps
                )
        } else {              # slide or growing window workflow (time series)
            new("Workflow",
                name= if (missing(wfID) || wfID %in% c("standardWF","timeseriesWF")) paste(learner,type,sep=".") else wfID,
                func= "timeseriesWF",
                pars=list(type=type,relearn.step=relearn.step,
                    learner=learner,learner.pars=learner.pars,
                    predictor=predictor,predictor.pars=predictor.pars,
                    pre=pre,pre.pars=pre.pars,post=post,post.pars=post.pars,
                    .fullOutput= fullOutput),
                deps=deps
                )
        }
    } else {
        new("Workflow",
            name= if (missing(wfID)) user else wfID,
            func=user,
            pars=user.pars,
            deps=deps
            )
    }
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
         representation(predictions   = "data.frame",   # a data.frame nTest x 2 (true,pred)
                        extraInfo     = "list")     # list of whatever info the wf author wants
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
## CLASS: CvTask
##
## A class containing the settings of a cross validation experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("CvTask",
         representation(metrics='character',     # metrics to be estimated
                        evaluator='character',   # function used to obtain metrics
                        evaluator.pars='OptList',   # pars to this function
                        nReps='numeric',      # nr. of repetitions
                        nFolds='numeric',     # nr. of folds of each rep.
                        seed='numeric',       # seed of the random generator
                        strat='logical',      # is the sampling stratified?
                        dataSplits='OptMatrix') # user supplied data splits
         )


## --------------------------------------------------------------
## constructor
##
CvTask <- function(metrics,evaluator="",evaluator.pars=NULL,
                   nReps=1,nFolds=10,
                   seed=1234,strat=FALSE,
                   dataSplits=NULL) {
    new("CvTask",
        metrics=metrics,evaluator=evaluator,evaluator.pars=evaluator.pars,
        nReps=if (is.null(dataSplits)) nReps else length(dataSplits),
        nFolds=if (is.null(dataSplits)) nFolds else ncol(dataSplits[[1]]),
        seed=seed,strat=strat,dataSplits=dataSplits)
}


## ==============================================================
## CLASS: HldTask
##
## A class containing the settings of a holdout experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("HldTask",
         representation(metrics='character',# metrics to be estimated
                        evaluator='character',   # function used to obtain metrics
                        evaluator.pars='OptList',   # pars to this function
                        nReps='numeric', # number of repetitions
                        hldSz='numeric', # the size (0..1) of the holdout
                        seed='numeric',  # the random number seed
                        strat='logical', # is the sampling stratified?
                        dataSplits='OptMatrix') # user supplied data splits
         )


## --------------------------------------------------------------
## Constructor
##
HldTask <- function(metrics,evaluator="",evaluator.pars=NULL,
                    nReps=1,hldSz=0.3,
                    seed=1234,strat=FALSE,
                    dataSplits=NULL) {
    new("HldTask",
        metrics=metrics,evaluator=evaluator,evaluator.pars=evaluator.pars,
        nReps=if (is.null(dataSplits)) nReps else ncol(dataSplits),
        hldSz=if (is.null(dataSplits)) hldSz else sum(dataSplits[,1])/nrow(dataSplits),
        seed=seed,strat=strat,dataSplits=dataSplits)
}


## ==============================================================
## CLASS: LoocvTask
##
## A class containing the settings of a leave one out cross validation
## experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("LoocvTask",
         representation(metrics='character',  # metrics to be estimated
                        evaluator='character',   # function used to obtain metrics
                        evaluator.pars='OptList',   # pars to this function
                        seed='numeric',    # seed of the random generator
                        verbose='logical', # function used to evalute preds
                        dataSplits='OptMatrix')
         )


## --------------------------------------------------------------
## constructor
LoocvTask <- function(metrics,evaluator="",evaluator.pars=NULL,
                      seed=1234,verbose=FALSE,
                      dataSplits=NULL)
  new("LoocvTask",
      metrics=metrics,evaluator=evaluator,evaluator.pars=evaluator.pars,
      seed=seed,verbose=verbose,
      dataSplits=dataSplits)



## ==============================================================
## CLASS: BootTask
##
## A class containing the settings of a boostrap experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("BootTask",
         representation(metrics='character', # metrics to be estimated
                        evaluator='character',   # function used to obtain metrics
                        evaluator.pars='OptList',   # pars to this function
                        type='character', # type of boostrap ("e0" or ".632")
                        nReps='numeric',  # number of repetitions
                        seed='numeric',   # seed of the random generator
                        dataSplits='OptMatrix') # user supplied data splits
         )


## --------------------------------------------------------------
## constructor
##
BootTask <- function(metrics,evaluator="",evaluator.pars=NULL,
                     type='e0',nReps=200,seed=1234,dataSplits=NULL) {
     new("BootTask",
         metrics=metrics,evaluator=evaluator,evaluator.pars=evaluator.pars,
         type=type,
         nReps=if (is.null(dataSplits)) nReps else ncol(dataSplits),
         seed=seed,dataSplits=dataSplits)
}



## ==============================================================
## CLASS: McTask
##
## A class containing the settings of a monte carlo experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("McTask",
         representation(metrics='character',
                        evaluator='character',   # function used to obtain metrics
                        evaluator.pars='OptList',   # pars to this function
                        nReps='numeric',
                        szTrain='numeric',
                        szTest='numeric',
                        seed='numeric',
                        dataSplits='OptMatrix')
         )


## --------------------------------------------------------------
## constructor
##
McTask <- function(metrics,evaluator="",evaluator.pars=NULL,
                   nReps=10,szTrain=0.25,szTest=0.25,
                   seed=1234,dataSplits=NULL)
    new("McTask",
        metrics=metrics,evaluator=evaluator,evaluator.pars=evaluator.pars,
        nReps= if (is.null(dataSplits)) nReps else ncol(dataSplits),
        szTrain=szTrain,szTest=szTest,
        seed=seed,dataSplits=dataSplits)



## ==============================================================
## CLASS UNION: EstimationTask
##
## A class encapsulating all types of Estimation Experiments
## ==============================================================

setClassUnion("EstimationTask",
              c("CvTask", "McTask", "HldTask",
                "LoocvTask","BootTask"))





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
         representation(task             = "PredTask",
                        workflow         = "Workflow",
                        estTask          = "EstimationTask",
                        iterationsScores = "matrix",   # nIts x nStats
                        iterationsInfo   = "list"      # list of nIts lists
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
#  o@iterationsPreds <- p
  ## classification tasks, code back predictions to class labels
  if (!is.null(p) && is.factor(model.response(model.frame(t@formula,eval(t@dataSource))))) {
      for (i in 1:length(o@iterationsPreds))
          o@iterationsInfo[[i]]$preds[,"predicted"] <- factor(o@iterationsInfo[[i]]$preds[,"predicted"],levels=levels(responseValues(t@formula,eval(t@dataSource))))
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
setClass("ComparisonResults",representation(tasks="list"))

## --------------------------------------------------------------
## constructor
##
ComparisonResults <- function(t) {
  o <- new("ComparisonResults")
  o@tasks <- t
  o
}

