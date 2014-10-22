#################################################################
## Workflows
#################################################################
## This module contains functions related to workflows
#################################################################




# =====================================================
# This function generates a named list of objects of class
# learner.
# It is used for easily generating a list of variants of
# a learning system that is usable by the experimentalComparison()
# function.
# If you give only the learning system name it will generate
# a learner wit the default parameters of that system.
# The names of the components are generated automatically
# and have the form <sys>-v<x> where <x> is an increasing
# integer. In the case of using defaults the name is
# <sys>-defaults
# =====================================================
# Luis Torgo, Ago 2013
# =====================================================
# Example call:
# 
# 
#

workflowVariants <- function(wf,...,varsRootName,as.is=NULL) {
    if (missing(wf)) wf <- "standardWF"
    wf <- if (is(wf,"function")) deparse(substitute(wf)) else wf
    if (missing(varsRootName)) varsRootName <- wf

    default.novar <- c('evaluator.pars.stats','evaluator.pars.allCls','evaluator.pars.benMtrx')
    allnovar <- c(as.is,default.novar)

    ## unfolding the parameters hidden inside the special parameters


    vars <- list(...)
    if (!length(vars)) {
        ##vars <- c(Workflow(wf,list()))
        vars <- c(Workflow(wf))
        names(vars)[1] <- paste(varsRootName,'.v1',sep='')
        vars[[1]]@name <- names(vars)[1]
        return(vars)
    }
  
    ## the special parameters that are list and thus need to be unfolded
    islist <- sapply(vars,function(v) is.list(v))
    spec <- names(vars)[islist]
    ##spec <- c('learner.pars','predictor.pars','evaluator.pars')  
    newVars <- list()
    for(i in 1:length(vars))
        if (names(vars)[i] %in% spec)
            newVars <- c(newVars,unlist(vars[i],recursive=F))
        else
            newVars <- c(newVars,vars[i])
    vars <- newVars

    ## the parameters not involved in variants generation
    ## their names:
    allnovar <- c(allnovar,names(vars)[which(sapply(vars,length)==1)])
    ## their positions in vars:
    toExcl <- which(names(vars) %in% allnovar)
    ## their number:
    nExcl <- length(toExcl)
    
    ## checking how many variants per parameter and generate the grid
    nvarsEach <- rep(1,length(vars))
    varying <- if (nExcl) (1:length(vars))[-toExcl] else 1:length(vars)
    if (length(varying)) nvarsEach[varying] <- sapply(vars[varying],length)
    idxsEach <- lapply(nvarsEach,function(x) 1:x)
    theVars <- expand.grid(idxsEach)
    
    ## now go for generating the different variants
    vs <- vector("list",nrow(theVars))
    for(v in 1:nrow(theVars)) {
        ## start
        varPars <- list()
        for(k in 1:ncol(theVars)) {
            if (nExcl & (k %in% toExcl))
                varPars <- c(varPars,vars[k])
            else {
                x <- vars[k]
                x[[1]] <- x[[1]][theVars[v,k]]
                varPars <- c(varPars,x)
            }
        }
        specParsPos <- grep(paste(paste('^',spec,'[:.:]',sep=''),collapse='|'),
                            names(varPars))
        normal <- 1:length(varPars)
        if (length(specParsPos)) normal <- normal[-specParsPos]
        normalPars <- if (length(normal)) varPars[normal] else NULL
        
        finalVars <- list()
        for(i in 1:length(spec)) {
            pos <- grep(paste('^',spec[i],'[:.:]',sep=''),names(varPars))
            if (length(pos)) {
                x <- list()
                for(j in pos) {
                    x <- c(x,list(varPars[[j]]))
                    names(x)[length(x)] <- gsub(paste('^',spec[i],'[:.:]',sep=''),'',names(varPars)[j])
                }
                finalVars <- c(finalVars,list(x))
                names(finalVars)[length(finalVars)] <- spec[i]
            }
        }
        finalVars <- c(finalVars,normalPars)
        ##vs[[v]] <- Workflow(wf,finalVars)
        vs[[v]] <- do.call("Workflow",c(list(wf),finalVars))
    }
    
    for(i in 1:length(vs)) 
        names(vs)[i] <- vs[[i]]@name <- if (wf=="standardWF" || wf=="timeseriesWF") vs[[i]]@pars$learner else varsRootName

    cs <- table(names(vs))
    tochg <- names(cs)[which(cs>1)]
    for(n in tochg) {
        pos <- which(names(vs)==n)
        for(i in seq_along(pos))
            names(vs)[pos[i]] <- vs[[pos[i]]]@name <- paste0(n,".v",i)
    }
    vs
}


# =====================================================
# This function obtains the parameter settings associated
# to a certain variant name in the context of the variants
# of an experimental comparison
# =====================================================
# Luis Torgo, Ago 2013
# =====================================================
# Example Call:
# > getWorkflow('cv.nnet-v6',cvResults)
#
# Note: The result of this can then be "run" as follows,
# > runWorkflow(getWorkflow('cv.nnet-v6',cvResults),
#               medv~.,Boston[1:100,],Boston[-(1:100),])
#
getWorkflow <- function(var,obj)
    obj@tasks[[1]][[which(names(obj@tasks[[1]]) == var)]]@workflow



# =====================================================
# Function that can be used to call a workflow function
# whose information is stored in an object of class workflow.
# =====================================================
# Luis Torgo, Jul 2013
# =====================================================
# Example run:
# l  <- workflow('nnet',pars=list(size=4,linout=T))
# runWorkflow(l,medv ~ ., Boston)
#
runWorkflow <- function(l,...) {
  if (!inherits(l,'Workflow')) stop(l,' is not of class "Workflow".')
  res <- do.call(l@func,c(list(...),l@pars))
  if (!inherits(res,'WFoutput')) stop('Provided workflow should return a "WFoutput" object.') else res
}


## --------------------------------------------------------------
## Workflow output
## --------------------------------------------------------------


workflowPredictions <- function(obj) obj@predictions


`workflowInformation<-` <- function(obj,value) {
    if (!is.list(value)) stop("workflowInformation:: value must be a list!")
    obj@extraInfo <- value
    obj
}

workflowInformation <- function(obj) obj@extraInfo

getITsInfo <- function(obj,task=1,workflow=1,rep,fold,it) {
    if ((missing(rep) || missing(fold)) && missing(it))
        stop("getITsInfo:: you need to supply both 'rep' and 'fold' or 'it'")
    if (!missing(it))
        obj@tasks[[task]][[workflow]]@iterationsInfo[[it]]
    else
        obj@tasks[[task]][[workflow]]@iterationsInfo[[(rep-1)*obj@tasks[[task]][[workflow]]@estTask@nFolds+fold]]
}

getPredictionsInfo <- function(obj,task=1,workflow=1,rep,fold,it) {
    if ((missing(rep) || missing(fold)) && missing(it))
        stop("getPredictionsInfo:: you need to supply both 'rep' and 'fold' or 'it'")
    if (!missing(it))
        obj@tasks[[task]][[workflow]]@iterationsPreds[[it]]
    else
        obj@tasks[[task]][[workflow]]@iterationsPreds[[(rep-1)*obj@tasks[[task]][[workflow]]@estTask@nFolds+fold]]
}


## --------------------------------------------------------------
## Standard Workflows
## --------------------------------------------------------------


## =====================================================================
## A function implementing a typical workflow for predictive tasks.
## ---------------------------------------------------------------------
## L. Torgo (Ago, 2013)
##
standardWF <- function(form,train,test,
                       learner,learner.pars=NULL,
                       predictor='predict',predictor.pars=NULL,
                       pre=NULL,pre.pars=NULL,
                       post=NULL,post.pars=NULL,
                       .fullOutput=FALSE)
{
    .fullRes <- if (.fullOutput) list() else NULL

    ## Data pre-processing stage
    if (!is.null(pre)) {
        preprocRes <- do.call(pre,c(list(form,train,test),pre.pars))
        train <- preprocRes$train
        test <- preprocRes$test
        if (.fullOutput) .fullRes$preprocessing <- preprocRes
    }

    ## Learning stage
    if (is.null(predictor)) {  ## there is no separate predict stage (e.g. kNN)
        ps <- do.call(learner,c(list(form,train,test),learner.pars))
        if (.fullOutput) .fullRes$modeling <- ps
    } else {
        m <- do.call(learner,c(list(form,train),learner.pars))
        ps <- do.call(predictor,c(list(m,test),predictor.pars))
        if (.fullOutput) .fullRes$modeling <- if (is.null(post)) m else list(model=m,initPreds=ps)
    }

    ## Data post-processing stage
    if (!is.null(post)) {
        ps <- do.call(post,c(list(form,train,test,ps),post.pars))
        if (.fullOutput) .fullRes$postprocessing <- ps
    }
    
    res <- WFoutput(rownames(test),responseValues(form,test),ps)
    if (.fullOutput) workflowInformation(res) <- .fullRes
    res
}
 

## =====================================================================
## The workhorse function implementing sliding and growing window worflows
## for time series prediction tasks
## ---------------------------------------------------------------------
## L. Torgo (Jan, 2013)
##
timeseriesWF <- function(form,train,test,
                         learner,learner.pars=NULL,
                         type='slide',relearn.step=1,
                         predictor='predict',predictor.pars=NULL,
                         evaluator=if (is.factor(responseValues(form,train))) 'classificationMetrics' else 'timeseriesMetrics',
                         evaluator.pars=NULL,
                         .outPreds=TRUE,.outModels=FALSE,
                         verbose=T)
{
   
  data <- rbind(train,test)
  n <- NROW(data)
  train.size <- NROW(train)
  sts <- seq(train.size+1,n,by=relearn.step)

  preds <- vector()
  if (.outModels && !is.null(predictor)) models <- list()
  
  for(s in sts) {

    tr <- if (type=='slide') data[(s-train.size):(s-1),] else data[1:(s-1),]
    ts <- data[s:min((s+relearn.step-1),n),]
    
    if (verbose) cat('*')

    if (is.null(predictor)) {
      ps <- do.call(learner,c(list(form,tr,ts),learner.pars))
    } else {
      m <- do.call(learner,c(list(form,tr),learner.pars))
      ps <- do.call(predictor,c(list(m,ts),predictor.pars))
    }
    if (.outModels && !is.null(predictor)) models <- c(models,list(list(start=s,model=m)))
    preds <- c(preds,ps)
  }
  if (verbose) cat('\n')
  ##cat(length(preds),'\t',length(responseValues(form,test)),'\n')
  eval.res <- do.call(evaluator,c(list(responseValues(form,test),preds),c(evaluator.pars,list(train.y=if (any(c('nmse','nmae') %in% evaluator.pars$stats)) responseValues(form,train) else NULL))))

  res <- WFoutput(eval.res)
  if (.outPreds) workflowPredictions(res) <- list(responseValues(form,test),preds,rownames(test))
  if (.outModels && !is.null(predictor)) workflowInformation(res) <- models
  res
}

