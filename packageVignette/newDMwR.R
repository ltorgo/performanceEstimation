
#################################################################
# General Stuff
#################################################################



# =====================================================================
# Function to calculate some standard regression evaluation statistics
# ---------------------------------------------------------------------
# L. Torgo (2009)
#
# Examples:
# s <- regr.eval(tr,ps,train.y=data[,'Y'])
# s <- regr.eval(tr,ps,stats=c('mse','mae'))
#
regr.eval <- function(trues,preds,
                      stats=if (is.null(train.y)) c('mae','mse','rmse','mape') else c('mae','mse','rmse','mape','nmse','nmae'),
                      train.y=NULL)
{
  allSs <- c('mae','mse','rmse','mape','nmse','nmae')
  if (any(c('nmse','nmad') %in% stats) && is.null(train.y))
    stop('regr.eval:: train.y parameter not specified.',call.=F)
  if (!all(stats %in% allSs))
    stop("regr.eval:: don't know how to calculate -> ",call.=F,
         paste(stats[which(!(stats %in% allSs))],collapse=','))
  N <- length(trues)
  sae <- sum(abs(trues-preds))
  sse <- sum((trues-preds)^2)
  r <- c(mae=sae/N,mse=sse/N,rmse=sqrt(sse/N),mape=sum(abs((trues-preds)/trues))/N)
  if (!is.null(train.y)) r <- c(r,c(nmse=sse/sum((trues-mean(train.y))^2),nmae=sae/sum(abs(trues-mean(train.y)))))
  return(r[stats])
}

# =====================================================================
# Function to calculate some standard classification evaluation statistics
# ---------------------------------------------------------------------
# L. Torgo (2012)
#
# Examples:
# s <- class.eval(tr,ps)
# s <- class.eval(tr,ps,benMtrx=matrix(c(2,-13,-4,5),2,2))
#
class.eval <- function(trues,preds,
                       stats=if (is.null(benMtrx)) c('acc','err') else c('acc','err','totU'),
                       benMtrx=NULL,
                       allCls=levels(factor(trues)))

  {
    preds <- factor(preds,levels=allCls)
    trues <- factor(trues,levels=allCls)
    allSs <- c('acc','err','totU')
    if (any(c('totU') %in% stats) && is.null(benMtrx))
      stop('class.eval:: benMtrx parameter not specified.',call.=F)
    if (!all(stats %in% allSs))
      stop("class.eval:: don't know how to calculate -> ",call.=F,
           paste(stats[which(!(stats %in% allSs))],collapse=','))
    N <- length(trues)
    cm <- as.matrix(table(trues,preds))
    a <- sum(diag(cm))/N
    r <- c(acc=a,err=1-a)
    if (!is.null(benMtrx))
      if (!all(dim(cm)==dim(benMtrx)))
        stop("class.eval:: dimensions of confusion and benefits metrices do not match",call.=F)
      else r <- c(r,totU=sum(cm*benMtrx))
    
    return(r[stats])
  }   


# =====================================================================
# Function to calculate some standard  evaluation statistics for time series
# problems
# ---------------------------------------------------------------------
# L. Torgo (2013)
#
# Examples:
# s <- ts.eval(tr,ps,train.y=data[,'Y'])
# s <- ts.eval(tr,ps,stats=c('mse','mae'))
#
ts.eval <- function(trues,preds,
                    stats=if (is.null(train.y)) c('mae','mse','rmse','mape') else c('mae','mse','rmse','mape','nmse','nmae','theil'),
                    train.y=NULL)
{
  r <- if (!is.null(train.y))  c(regr.eval(trues,preds,setdiff(stats,'theil'),train.y),theil=sum((trues-preds)^2)/sum((c(train.y[length(train.y)],trues[-length(trues)])-preds)^2)) else regr.eval(trues,preds,setdiff(stats,'theil'),train.y)
  return(r[stats])
}



#################################################################
## Some simple work-flow functions for experimental comparisons
## using some common algorithms for both classification and
## regression tasks
#################################################################


#######################
## Regression workflows

.RegrStats <- c('mae','mse')  # the default evaluation metrics for regression
.Regr <- list(rpartXse     = 'DMwR', 
              svm          = 'e1071',
              ksvm         = 'kernlab',
              lm           = 'stats',
              earth        = 'earth',
              randomForest = 'randomForest',
              bagging      = 'ipred',
              gbm          = 'gbm',
              nnet         = 'nnet'
             )



## =====================================================================
## A function implementing a typical workflow for regression working
## with different learners
## ---------------------------------------------------------------------
## L. Torgo (Dec, 2012)
##
regrWF <- function(form,train,test,learner,eval=.RegrStats,simpl=F,...) {
  do.call('require',list(.Regr[[learner]],quietly=T))
  
  args <- list(...)
  if (learner == 'lm') {
    s.args <- which(names(args) %in% names(formals('step')))
    if (length(s.args)) {
      step.args <- args[s.args]
      args <- args[-s.args]
    }
    m <- do.call(learner,c(list(form,train),args))
    if (simpl) m <- if (length(s.args)) do.call('step',c(list(m,trace=0),step.args)) else step(m,trace=0)
  } else  if (learner == 'nnet') {
    if (!hasArg(linout)) m <- do.call(learner,c(list(form,train,linout=T),args))
    else m <- do.call(learner,c(list(form,train),args))
  } else  if (learner == 'gbm') {
    if (!hasArg(distribution)) m <- do.call(learner,c(list(form,distribution='gaussian',data=train,verbose=F),args))
    else m <- do.call(learner,c(list(form,data=train,verbose=F),args))
  } else  m <- do.call(learner,c(list(form,train),args))

  p <- if (learner == 'gbm')  predict(m,test,n.trees=m$n.trees) else predict(m,test)

  regr.eval(resp(form,test),p,stats=eval,train.y=if(any(c('nmse','nmae') %in% eval)) resp(form,train) else NULL)
}


## =====================================================================
## A function implementing a typical workflow for regression approaches
## to time series forecasting using sliding window working  with different
## learners
## ---------------------------------------------------------------------
## L. Torgo (Jan, 2013)
##
slideRegrWF <- function(...) tsRegrWF(type='slide',...)

## =====================================================================
## A function implementing a typical workflow for regression approaches
## to time series forecasting using growing window working  with different
## learners
## ---------------------------------------------------------------------
## L. Torgo (Jan, 2013)
##
growRegrWF <- function(...) tsRegrWF(type='grow',...)


## =====================================================================
## The workhorse function implementing sliding and growing window worflows
## for regression techniques
## ---------------------------------------------------------------------
## L. Torgo (Jan, 2013)
##
tsRegrWF <- function(form,train,test,type,learner,eval=.RegrStats,simpl=F,relearn.step=1,verbose=T,...) {
  do.call('require',list(.Regr[[learner]],quietly=T))
  
  args <- list(...)
  data <- rbind(train,test)
  n <- NROW(data)
  train.size <- NROW(train)
  sts <- seq(train.size+1,n,by=relearn.step)

  preds <- vector()
  for(s in sts) {

    tr <- if (type=='slide') data[(s-train.size):(s-1),] else data[1:(s-1),]
    ts <- data[s:min((s+relearn.step-1),n),]
    
    if (verbose) cat('*')

    if (learner == 'lm') {
      s.args <- which(names(args) %in% names(formals('step')))
      if (length(s.args)) {
        step.args <- args[s.args]
        args <- args[-s.args]
      }
      m <- do.call(learner,c(list(form,tr),args))
      if (simpl) m <- if (length(s.args)) do.call('step',c(list(m,trace=0),step.args)) else step(m,trace=0)
    } else  if (learner == 'nnet') {
      if (!hasArg(linout)) m <- do.call(learner,c(list(form,tr,linout=T),args))
      else m <- do.call(learner,c(list(form,tr),args))
    } else  if (learner == 'gbm') {
      if (!hasArg(distribution)) m <- do.call(learner,c(list(form,distribution='gaussian',data=tr,verbose=F),args))
      else m <- do.call(learner,c(list(form,data=tr,verbose=F),args))
    } else  m <- do.call(learner,c(list(form,tr),args))

    ps <- if (learner == 'gbm')  predict(m,ts,n.trees=m$n.trees) else predict(m,ts)

    preds <- c(preds,ps)
  }
  if (verbose) cat('\n')
    
  regr.eval(resp(form,test),preds,stats=eval,train.y=if(any(c('nmse','nmae') %in% eval)) resp(form,train) else NULL)
}


###########################
## Classification workflows

.ClassStats <- c('err')  # the default evaluation metrics for classification
.Class <- list(rpartXse     = 'DMwR', 
               svm          = 'e1071',
               ksvm         = 'kernlab',
               lda          = 'MASS',
               naiveBayes   = 'e1071',
               kNN          = 'DMwR',
               randomForest = 'randomForest',
               bagging      = 'adabag',
               boosting     = 'adabag',
               C5.0         = 'C50',
               nnet         = 'nnet'
               )


## =====================================================================
## A function implementing a typical workflow for classification working
## with different learners
## ---------------------------------------------------------------------
## L. Torgo (Dec, 2012)
##
classWF <- function(form,train,test,learner,eval=.ClassStats,...) {
  do.call('require',list(.Class[[learner]],quietly=T))
  
  args <- list(...)
  eval.args <- which(names(args) %in% names(formals(class.eval)))
  learner.args <- if (length(eval.args)) args[-eval.args] else args

  if (learner != 'kNN') {
    
    m <- do.call(learner,c(list(form,train),learner.args)) 
  
    p <- if (learner %in% c('rpartXse','nnet')) predict(m,test,type='class') else if (learner %in% c('lda','bagging','boosting')) predict(m,test)$class else predict(m,test)
    
  } else p <- do.call(learner,c(list(form,train,test),learner.args))
  
  do.call('class.eval',c(list(resp(form,test),p,stats=eval),args[eval.args]))
}


## =====================================================================
## A function implementing a typical workflow for classification approaches
## to time series forecasting using sliding window, working  with different
## learners
## ---------------------------------------------------------------------
## L. Torgo (Jan, 2013)
##
slideClassWF <- function(...) tsClassWF(type='slide',...)

## =====================================================================
## A function implementing a typical workflow for regression approaches
## to time series forecasting using growing window working  with different
## learners
## ---------------------------------------------------------------------
## L. Torgo (Jan, 2013)
##
growClassWF <- function(...) tsClassWF(type='grow',...)


## =====================================================================
## The workhorse function implementing sliding and growing window worflows
## for classification techniques
## ---------------------------------------------------------------------
## L. Torgo (Jan, 2013)
##
tsClassWF <- function(form,train,test,type,learner,eval=.ClassStats,relearn.step=1,verbose=T,...) {
  do.call('require',list(.Class[[learner]],quietly=T))
  
  args <- list(...)
  eval.args <- which(names(args) %in% names(formals(class.eval)))
  learner.args <- if (length(eval.args)) args[-eval.args] else args

  data <- rbind(train,test)
  n <- NROW(data)
  train.size <- NROW(train)
  sts <- seq(train.size+1,n,by=relearn.step)

  preds <- factor(rep('',NROW(test)),levels=levels(resp(form,data)))

  for(s in sts) {

    tr <- if (type=='slide') data[(s-train.size):(s-1),] else data[1:(s-1),]
    ts <- data[s:min((s+relearn.step-1),n),]
    
    if (verbose) cat('*')
    
    if (learner != 'kNN') {
      
      m <- do.call(learner,c(list(form,tr),learner.args)) 
      
      ps <- if (learner %in% c('rpartXse','nnet')) predict(m,ts,type='class') else if (learner %in% c('lda','boosting')) predict(m,ts)$class else predict(m,ts)
      
    } else ps <- do.call(learner,c(list(form,tr,ts),learner.args))

    preds[(s-sts[1]+1):(s-sts[1]+length(ps))] <- ps
  }
  if (verbose) cat('\n')

  do.call('class.eval',c(list(resp(form,test),preds,stats=eval),args[eval.args]))
}

