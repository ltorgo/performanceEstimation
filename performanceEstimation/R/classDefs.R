################################################################# 
## THIS FILE CONTAINS THE CLASSES AND RESPECTIVE CONSTRUCTORS   #
## THAT ARE DEFINED IN THIS PACKAGE                             #
#################################################################
## Author : Luis Torgo (ltorgo@dcc.fc.up.pt)     Date: Nov 2013 #
## License: GPL (>= 2)                                          #
#################################################################



## ==============================================================
## CLASS: PredTask
##
## Class for storing information concerning a prediction task
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("PredTask",
         representation(name="character",formula="formula",data="data.frame"))


## --------------------------------------------------------------
## constructor
##
PredTask <- function(formula,data,name=NULL) {
  if (missing(formula) || missing(data))
    stop('\nYou need to provide a formula and a data frame.\n')
  if (inherits(try(mf <- model.frame(formula,data),T),"try-error"))
    stop('\nInvalid formula for the given data frame.\n')

  if (is.null(name)) {
    m <- match.call()
    name <- deparse(m$data)
  }
  new("PredTask",
      name=name,formula=formula,data=as.data.frame(mf))
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
         representation(name="character",func="character",pars="list"))



## --------------------------------------------------------------
## Cnstructor
##
Workflow <- function(func,...,wfID=func) {
    if (missing(func)) stop("\nYou need to provide the name of a function implementing the workflow.\n")
    fn <- if (is(func,"function")) deparse(substitute(func)) else func
    do.call("new",list("Workflow",name=wfID,func=fn,pars=list(...)))
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
         representation(scores        = "numeric",  # vector of stats scores
                        predictions   = "data.frame",   # a data.frame nTest x 2 (true,pred)
                        extraInfo     = "list")     # list of whatever info the wf author wants
)


## --------------------------------------------------------------
## Constructor
##
WFoutput <- function(s,p=data.frame(),e=list()) {
  o              <- new("WFoutput")
  o@scores       <- s
  o@predictions  <- p
  o@extraInfo    <- e
  o
}



## ==============================================================
## CLASS: CvSettings
##
## A class containing the settings of a cross validation experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClassUnion("OptList",c("list","NULL"))
setClass("CvSettings",
         representation(nReps='numeric',      # nr. of repetitions
                        nFolds='numeric',     # nr. of folds of each rep.
                        seed='numeric',       # seed of the random generator
                        strat='logical',      # is the sampling stratified?
                        dataSplits='OptList')    # user supplied data splits
         )


## --------------------------------------------------------------
## constructor
##
CvSettings <- function(nReps=1,nFolds=10,seed=1234,strat=FALSE,dataSplits=NULL) {
    new("CvSettings",
        nReps=if (is.null(dataSplits)) nReps else length(dataSplits),
        nFolds=if (is.null(dataSplits)) nFolds else ncol(dataSplits[[1]]),
        seed=seed,strat=strat,dataSplits=dataSplits)
}


## ==============================================================
## CLASS: HldSettings
##
## A class containing the settings of a holdout experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClassUnion("OptMatrix",c("matrix","NULL"))
setClass("HldSettings",
         representation(nReps='numeric', # number of repetitions
                        hldSz='numeric', # the size (0..1) of the holdout
                        seed='numeric',  # the random number seed
                        strat='logical', # is the sampling stratified?
                        dataSplits='OptMatrix') # user supplied data splits
         )


## --------------------------------------------------------------
## Constructor
##
HldSettings <- function(nReps=1,hldSz=0.3,seed=1234,strat=FALSE,dataSplits=NULL) {
    new("HldSettings",
        nReps=if (is.null(dataSplits)) nReps else ncol(dataSplits),
        hldSz=if (is.null(dataSplits)) hldSz else sum(dataSplits[,1])/nrow(dataSplits),
        seed=seed,strat=strat,dataSplits=dataSplits)
}


## ==============================================================
## CLASS: LoocvSettings
##
## A class containing the settings of a leave one out cross validation
## experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("LoocvSettings",
         representation(seed='numeric',    # seed of the random generator
                        verbose='logical') # function used to evalute preds
         )


## --------------------------------------------------------------
## constructor
LoocvSettings <- function(seed=1234,verbose=FALSE)
  new("LoocvSettings",seed=seed,verbose=verbose)



## ==============================================================
## CLASS: BootSettings
##
## A class containing the settings of a boostrap experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("BootSettings",
         representation(type='character', # type of boostrap ("e0" or ".632")
                        nReps='numeric',  # number of repetitions
                        seed='numeric',   # seed of the random generator
                        dataSplits='OptMatrix') # user supplied data splits
         )


## --------------------------------------------------------------
## constructor
##
BootSettings <- function(type='e0',nReps=200,seed=1234,dataSplits=NULL) {
     new("BootSettings",type=type,
         nReps=if (is.null(dataSplits)) nReps else ncol(dataSplits),
         seed=seed,dataSplits=dataSplits)
}



## ==============================================================
## CLASS: McSettings
##
## A class containing the settings of a monte carlo experiment
## ==============================================================


## --------------------------------------------------------------
## Definition
##
setClass("McSettings",
         representation(nReps='numeric',
                        szTrain='numeric',
                        szTest='numeric',
                        seed='numeric',
                        dataSplits='OptMatrix')
         )


## --------------------------------------------------------------
## constructor
##
McSettings <- function(nReps=10,szTrain=0.25,szTest=0.25,seed=1234,dataSplits=NULL)
    new("McSettings",nReps= if (is.null(dataSplits)) nReps else ncol(dataSplits),
        szTrain=szTrain,szTest=szTest,seed=seed,dataSplits=dataSplits)



## ==============================================================
## CLASS UNION: EstimationSettings
##
## A class encapsulating all types of experimental settings
## ==============================================================

setClassUnion("EstimationSettings",
              c("CvSettings", "McSettings", "HldSettings",
                "LoocvSettings","BootSettings"))





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
                        settings         = "EstimationSettings",
                        iterationsScores = "matrix",   # nIts x nStats
                        iterationsPreds  = "list",     # list of nIts matrices or NULL
                        iterationsInfo   = "list"      # list of nIts lists or NULL
                        )
         )


## --------------------------------------------------------------
## constructor
##
EstimationResults <- function(t,w,st,sc,p=NULL,e=NULL) {
  o                  <- new("EstimationResults")
  o@task             <- t
  o@workflow         <- w
  o@settings         <- st
  o@iterationsScores <- sc
  o@iterationsPreds <- p
  ## classification tasks, code back predictions to class labels
  if (!is.null(p) && is.factor(model.response(model.frame(t@formula,t@data))))
      o@iterationsPreds <- lapply(p,function(q) apply(q,2,function(x) factor(x,labels=levels(responseValues(t@formula,t@data)))))
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

