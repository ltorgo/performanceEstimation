################################################################# 
## THIS FILE CONTAINS FUNCTIONS THAT ARE RELATED TO RUNNING     #
## EXPERIMENTS WITH MODELLING TOOLS                             #
#################################################################
## Author : Luis Torgo (ltorgo@dcc.fc.up.pt)     Date: Aug 2013 #
## License: GPL (>= 2)                                          #
#################################################################





###################################################################
## FUNCTIONS FOR CARRYING OUT PERFORMANCE ESTIMATION EXPERIMENTS ## 
###################################################################


## ==============================================================
##
## ==============================================================
## Luis Torgo, Aug 2013
## ==============================================================
##
performanceEstimation <- function(tasks,workflows,estTask,...) {

  if (!is(tasks,'list')) tasks <- list(tasks)
  if (!is(workflows,'list')) workflows <- list(workflows)
  
  if (is.null(names(workflows)))
    names(workflows) <- paste('var',1:length(workflows),sep='.')
  
  nTasks <- length(tasks)
  taskNames <- sapply(tasks,function(x) x@taskName)
  nWFs <- length(workflows)
  wfNames <- sapply(workflows,function(x) x@name)
  
  allRes <- vector("list",nTasks)
  names(allRes) <- taskNames
  
  cat('\n\n##### PERFORMANCE ESTIMATION USING ',
      switch(class(estTask@method),
             CV='CROSS VALIDATION',
             Holdout='HOLD OUT',
             MonteCarlo='MONTE CARLO',
             Bootstrap='BOOTSTRAP',
             LOOCV='LOOCV',
             ),
      ' #####')
  
  for(d in 1:length(tasks)) {

    cat('\n\n** PREDICTIVE TASK ::',tasks[[d]]@taskName)

    taskRes <- vector("list",nWFs)
    names(taskRes) <- wfNames
    ##rr <- NULL
    for (s in 1:length(workflows)) {

      cat('\n\n++ MODEL/WORKFLOW ::',workflows[[s]]@name,"\n")
      
      taskRes[[s]] <- do.call(
               switch(class(estTask@method),
                      CV='cvEstimates',
                      Holdout='hldEstimates',
                      Bootstrap='bootEstimates',
                      MonteCarlo='mcEstimates',
                      LOOCV='loocvEstimates'
                      ),

               c(list(workflows[[s]],
                    tasks[[d]],
                    estTask),...)
                         )
    }
    
    allRes[[d]] <- taskRes

  }
  
  ComparisonResults(allRes)
}



#################################################################
## Cross Validation Experiments
#################################################################



## =====================================================
## Function that performs a cross validation experiment
## of a system on a given data set.
## The function is completely generic. The generality comes
## from the fact that the function that the user provides
## as the system to evaluate, needs in effect to be a
## user-defined function that takes care of the learning,
## testing and calculation of the statistics that the user
## wants to estimate through cross validation. 
## =====================================================
## Luis Torgo, Jan 2009
## =====================================================
## Example runs:
## x <- cvEstimates(Workflow('cv.rpartXse',se=2),
##                      PredTask(medv~.,Boston),
##                      CvTask(1,10,1234))
##
cvEstimates <- function(wf,task,estTask) {

  show(estTask)

  ## Did the user supplied the data splits for all folds and repetitions?
  userSplit <- !is.null(estTask@method@dataSplits)
  
  n <- nrow(eval(task@dataSource))
  if (!userSplit) n.each.part <- n %/% estTask@method@nFolds

  itsInfo <- vector("list",estTask@method@nFolds*estTask@method@nReps)

  if (!userSplit && estTask@method@strat) {  # stratified sampling
    respVals <- responseValues(task@formula,eval(task@dataSource))
    regrProb <- is.numeric(respVals)
    if (regrProb) {  # regression problem
      ## the bucket to which each case belongs  
      b <- cut(respVals,10)  # this 10 should be parametrizable
    } else {
      b <- respVals
    }
    ## how many on each bucket
    bc <- table(b)
    ## how many should be on each test partition
    bct <- bc %/% estTask@method@nFolds
    ## still missing (due to rounding effects of the previous statement)
    ##rem <- n.test-sum(bct)
    ##ib <- 1
    ##nb <- length(bct)
    ##while (rem) 
    ##  if (bct[ib] < bc[ib]) {
    ##    bct[ib] <- bct[ib]+1
    ##    rem <- rem-1
    ##    ib <- ib %% nb + 1
    ##  }

  }
  
  for(r in 1:estTask@method@nReps) {
    cat('Repetition ',r,'\nFold:')

    if (!userSplit) {
      set.seed(estTask@method@seed*r)
      permutation <- sample(n)
    } else permutation <- 1:n

    for(i in seq(estTask@method@nFolds)) {
      itN <- (r-1)*estTask@method@nFolds+i  # the iteration number
      cat(' ',i)
      
      if (!userSplit) {
        if (estTask@method@strat) {
          out.fold <- c()
          for(x in seq(along=levels(b))) 
            if (bct[x]) out.fold <- c(out.fold,which(b[permutation] == levels(b)[x])[((i-1)*bct[x]+1):((i-1)*bct[x]+bct[x])])
        } else {
          out.fold <- ((i-1)*n.each.part+1):(i*n.each.part)
        }
      } else out.fold <- outFold(estTask@method@dataSplits,itN)
      
      it.res <- runWorkflow(wf,
                            task@formula,
                            #perm.data[-out.fold,],
                            eval(task@dataSource)[permutation[-out.fold],],
                            #perm.data[out.fold,])
                            eval(task@dataSource)[permutation[out.fold],])
      
      itsInfo[[itN]] <- list(preds=it.res@predictions,
                             info=it.res@extraInfo,
                             train=permutation[-out.fold])
      
    }
    cat('\n')
  }

  ## randomize the number generator to avoid undesired
  ## problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))
  
  ## Calculate the metrics estimation
  scores <- .scoresIts(task,estTask,itsInfo)
      
    
  EstimationResults(task,wf,estTask,scores,itsInfo)
}


#################################################################
# Hold Out Experiments
#################################################################



# =====================================================
# Function that performs a hold out experiment
# of a system on a given data set.
# The function is completely generic. The generality comes
# from the fact that the function that the user provides
# as the system to evaluate, needs in effect to be a
# user-defined function that takes care of the learning,
# testing and calculation of the statistics that the user
# wants to estimate through hold out. A few example
# functions are provided (cv.rpartXse, cv.lm, cv.nnet)
# =====================================================
# Luis Torgo, Feb 2010
# =====================================================
# Example runs:
# x <- hldEstimates(learner('cv.rpartXse',list(se=2)),
#              dataset(medv~.,Boston),
#              hldTask(4,0.25,1234))
#
hldEstimates <- function(wf,task,estTask) {

  show(estTask)

  ## Did the user supplied the data splits for all folds and repetitions?
  userSplit <- !is.null(estTask@method@dataSplits)

  n <- nrow(eval(task@dataSource))
  if (!userSplit) n.test <- as.integer(n * estTask@method@hldSz)

  itsInfo <- vector("list",estTask@method@nReps)

  if (!userSplit & estTask@method@strat) {  # stratified sampling
    respVals <- responseValues(task@formula,eval(task@dataSource))
    regrProb <- is.numeric(respVals)
    if (regrProb) {  # regression problem
      # the bucket to which each case belongs  
      b <- cut(respVals,10)  # this 10 should be parameterizable
    } else {
      b <- respVals
    }
    # how many on each bucket
    bc <- table(b)
    # how many should be on each test partition
    bct <- as.integer(bc * estTask@method@hldSz)
    # still missing (due to rounding effects of the previous statement)
    #rem <- n.test-sum(bct)
    #ib <- 1
    #nb <- length(bct)
    #while (rem) 
    #  if (bct[ib] < bc[ib]) {
    #    bct[ib] <- bct[ib]+1
    #    rem <- rem-1
    #    ib <- ib %% nb + 1
    #  }
  }

  cat('Repetition :')
  for(r in 1:estTask@method@nReps) {
    cat(' ',r)

    if (!userSplit) {
      set.seed(estTask@method@seed*r)
      permutation <- sample(n)
      #perm.data <- task@data[permutation,]
#    } else perm.data <- task@data
    } else permutation <- 1:n


    if (!userSplit) {
      if (estTask@method@strat) {
        out.fold <- c()
        for(x in seq(along=levels(b))) 
          if (bct[x]) out.fold <- c(out.fold,which(b[permutation] == levels(b)[x])[1:bct[x]])
      } else {
        out.fold <- 1:n.test
      }
    } else out.fold <- outFold(estTask@method@dataSplits,r)

    it.res <- runWorkflow(wf,
                          task@formula,
                            #perm.data[-out.fold,],
                            eval(task@dataSource)[permutation[-out.fold],],
                            #perm.data[out.fold,])
                            eval(task@dataSource)[permutation[out.fold],])

    itsInfo[[r]] <- list(preds=it.res@predictions,
                         info=it.res@extraInfo,
                         train=permutation[-out.fold])
          
  }
  cat('\n')
  
  # randomize the number generator to avoid undesired
  # problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))

  ## Calculate the metrics estimation
  scores <- .scoresIts(task,estTask,itsInfo)
  
  EstimationResults(task,wf,estTask,scores,itsInfo)
}





#################################################################
# Leave One Out Cross Validation (LOOCV) Experiments
#################################################################



# =====================================================
# Function that performs a LOOCV experiment
# of a system on a given data set.
# The function is completely generic. The generality comes
# from the fact that the function that the user provides
# as the system to evaluate, needs in effect to be a
# user-defined function that takes care of the learning,
# testing and calculation of the statistics that the user
# wants to estimate through hold out. 
# =====================================================
# Luis Torgo, Mar 2010
# =====================================================
# Example runs:
# x <- loocvEstimates(learner('cv.rpartXse',list(se=2)),
#            dataset(medv~.,Boston))
#
loocvEstimates <- function(wf,task,estTask,verbose=FALSE) {

  show(estTask)

  ## Did the user supplied the data splits for all folds and repetitions?
  userSplit <- !is.null(estTask@method@dataSplits)

  n <- nrow(eval(task@dataSource))

  itsInfo <- vector("list",1)

  if (verbose) cat('Iteration: ')
  for(r in 1:n) {
    if (verbose) cat('*')

    if (!userSplit) {
        set.seed(estTask@method@seed*r)
        out.fold <- r
    } else out.fold <- outFold(estTask@method@dataSplits,r)

    it.res <- runWorkflow(wf,
                          task@formula,
                          eval(task@dataSource)[-out.fold,],
                          eval(task@dataSource)[out.fold,])
    
    itsInfo[[r]] <- list(preds=it.res@predictions,
                         info=it.res@extraInfo,
                         train=(1:n)[-out.fold])
  }
  if (verbose) cat('\n')
  
  ## randomize the number generator to avoid undesired
  ## problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))

  ## Calculate the metrics estimation
  scores <- .scoresIts(task,estTask,itsInfo)

  EstimationResults(task,wf,estTask,scores,itsInfo)

}




#################################################################
# Bootstrap Experiments
#################################################################



# =====================================================
# Function that performs a bootstrap experiment
# of a system on a given data set.
# The function is completely generic. The generality comes
# from the fact that the function that the user provides
# as the system to evaluate, needs in effect to be a
# user-defined function that takes care of the learning,
# testing and calculation of the statistics that the user
# wants to estimate through cross validation. 
# =====================================================
# Luis Torgo, Apr 2010
# =====================================================
# Example runs:
# x <- bootEstimates('cv.rpartXse',list(se=2)),
#                      dataset(medv~.,Boston),
#                      bootTask(1234,10))
#
bootEstimates <- function(wf,task,estTask) {

  show(estTask)

  if (estTask@method@type == '.632')
      resub <- runWorkflow(wf,task@formula,eval(task@dataSource),eval(task@dataSource))

  ## Did the user supplied the data splits for all folds and repetitions?
  userSplit <- !is.null(estTask@method@dataSplits)

  n <- nrow(eval(task@dataSource))

  itsInfo <- vector("list",estTask@method@nReps)

  cat('Repetition :')
  for(r in 1:estTask@method@nReps) {
    cat(' ',r)

    if (!userSplit) {
      set.seed(estTask@method@seed*r)
      idx <- sample(n,n,replace=T)
      it.res <- runWorkflow(wf,
                            task@formula,
                            eval(task@dataSource)[idx,],
                            eval(task@dataSource)[-idx,])
      itsInfo[[r]] <- list(preds=it.res@predictions,
                           info=it.res@extraInfo,
                           train=idx)
    } else {
      it.res <- runWorkflow(wf,
                            task@formula,
                            eval(task@dataSource)[outFold(estTask@method@dataSplits,r,"train"),],
                            eval(task@dataSource)[outFold(estTask@method@dataSplits,r),])
      itsInfo[[r]] <- list(preds=it.res@predictions,
                           info=it.res@extraInfo,
                           train=outFold(estTask@method@dataSplits,r,"train"))

    }
      
  }
  cat('\n')

  # randomize the number generator to avoid undesired
  # problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))
  
  ## Calculate the metrics estimation
  if (estTask@method@type == ".632") {  # this method is different from all others
      trReq <- any(estTask@metrics %in% c("nmse","nmae","theil"))
      nIts <- length(itsInfo)
      if (estTask@evaluator=="" ) 
          if (is.classification(task)) estTask@evaluator <- "classificationMetrics"
          else                         estTask@evaluator <- "regressionMetrics"
      scores <- matrix(NA,nrow=nIts,ncol=length(estTask@metrics))
      colnames(scores) <- estTask@metrics
      wts <- intersect(estTask@metrics,c("trTime","tsTime","totTime"))
      predMs <- setdiff(estTask@metrics,wts)
      if (length(predMs)) {
          if (trReq) {
              resubScores <- do.call(estTask@evaluator,
                                     c(list(trues=resub@predictions[,"true"],
                                            preds=resub@predictions[,"predicted"],
                                            stats=predMs,
                                            train.y=eval(task@dataSource)[1:n,task@target]),
                                   estTask@evaluator.pars))
          } else {
              resubScores <- do.call(estTask@evaluator,
                                     c(list(trues=resub@predictions[,"true"],
                                            preds=resub@predictions[,"predicted"],
                                            stats=predMs),
                                   estTask@evaluator.pars))
          }
      }
      for(i in 1:nIts) {
          if (length(predMs)) {
              if (trReq) {
                  scores[i,predMs] <- 0.632*do.call(estTask@evaluator,
                                                    c(list(trues=itsInfo[[i]]$preds[,"true"],
                                                           preds=itsInfo[[i]]$preds[,"predicted"],
                                                           stats=predMs,
                                                           train.y=eval(task@dataSource)[itsInfo[[i]]$train,task@target]),
                                                      estTask@evaluator.pars)) +
                                      0.368*resubScores
              } else {
                  scores[i,predMs] <- 0.632*do.call(estTask@evaluator,
                                                    c(list(trues=itsInfo[[i]]$preds[,"true"],
                                                           preds=itsInfo[[i]]$preds[,"predicted"],
                                                           stats=predMs),
                                                      estTask@evaluator.pars)) +
                                      0.368*resubScores
              }
          }
          if (length(wts)) {
              allts <- as.numeric(itsInfo[[i]]$info$times)
              scores[i,wts] <- c(trTime=allts[1],tsTime=allts[2],
                                 totTime=allts[1]+allts[2])[wts]
          }
      }
      
  } else scores <- .scoresIts(task,estTask,itsInfo)
      
  
  EstimationResults(task,wf,estTask,scores,itsInfo)

}



#################################################################
# Monte Carlo Experiments
#################################################################



# =====================================================
# Function that performs a Monte Carlo experiment of a 
# system on a given data set.
# The function is completely generic. The generality comes
# from the fact that the function that the user provides
# as the system to evaluate, needs in effect to be a
# user-defined function that takes care of the learning,
# testing and calculation of the statistics that the user
# wants to estimate through this experiment. A few example
# functions are provided.
# =====================================================
# Luis Torgo, Aug 2009
# =====================================================

mcEstimates <- function(wf, task, estTask, verbose=TRUE) {

  show(estTask)

  ## Did the user supplied the data splits for all  repetitions?
  userSplit <- !is.null(estTask@method@dataSplits)

  itsInfo <- vector("list",estTask@method@nReps)

  n <- NROW(eval(task@dataSource))

  if (!userSplit) {
      train.size <- if (estTask@method@szTrain < 1) as.integer(n*estTask@method@szTrain) else estTask@method@szTrain
      test.size <- if (estTask@method@szTest < 1) as.integer(n*estTask@method@szTest) else estTask@method@szTest
      if (n-test.size+1 <= train.size+1) stop('mcEstimates:: Invalid train/test sizes.',call.=FALSE)
  } else {
      train.size <- length(estTask@method@dataSplits[[1]][[1]]$train)
      test.size <- length(estTask@method@dataSplits[[1]][[1]]$test)
  }
  
  set.seed(estTask@method@seed)

  if (!userSplit) {
      selection.range <- (train.size+1):(n-test.size+1)
      starting.points <- sort(sample(selection.range,estTask@method@nReps))
  } else {
      starting.points <- sapply(estTask@method@dataSplits[[1]], function(d) d$test[1])
  }


  # main loop over all repetitions
  for(it in seq(along=starting.points)) {
    start <- starting.points[it]

    if (verbose)  cat('Repetition ',it,'\n\t start test = ',
                      start,'; test size = ',test.size,'\n')

    if (!userSplit) {
        rep.res <- runWorkflow(wf,
                               task@formula,
                               eval(task@dataSource)[(start-train.size):(start-1),],
                               eval(task@dataSource)[start:(start+test.size-1),])
    } else {
        rep.res <- runWorkflow(wf,
                               task@formula,
                               eval(task@dataSource)[estTask@method@dataSplits[[1]][[it]]$train,],
                               eval(task@dataSource)[estTask@method@dataSplits[[1]][[it]]$test,])

    }

    itsInfo[[it]] <- list(preds=rep.res@predictions,
                          info=rep.res@extraInfo,
                          train=(start-train.size):(start-1))

  }
  if (verbose) cat('\n')

  ## randomize the number generator to avoid undesired
  ## problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))
  
  ## Calculate the metrics estimation
  scores <- .scoresIts(task,estTask,itsInfo)

  EstimationResults(task,wf,estTask,scores,itsInfo)
}




# =====================================================
# Small utility functions 
# =====================================================


is.regression <- function(task) task@type == 'regr'

is.classification <- function(task) task@type == 'class'

responseValues <- function(formula,data,na=NULL) model.response(model.frame(formula,data,na.action=na))


## ----------------------------------------
## Internal functions that are not exported

outFold <- function(ds,it,what="test") if (is.list(ds[[1]])) ds[[it]][[what]] else ds[[it]]

.scores2summary <- function(obj)
    apply(obj@iterationsScores,2,function(x)
          c(avg=mean(x,na.rm=T),std=sd(x,na.rm=T),
            min=min(x,na.rm=T),max=max(x,na.rm=T),
            invalid=sum(is.na(x)))
          )


.scores2long <- function(itRes) {
    d <- data.frame(rep=1:nrow(itRes),itRes)
    s <- reshape(d,direction='long',varying=list(2:(ncol(itRes)+1)),idvar='rep',v.names='score')
    colnames(s)[2] <- 'stat'
    s[,2] <- factor(s[,2],labels=colnames(d)[2:(ncol(itRes)+1)])
    s
}


.statScores <- function(compRes,stat=1) {
    r <- list()
    for(t in compRes) {
        ws <- NULL
        for(w in t)
            ws <- cbind(ws,t(.scores2summary(w)[stat,,drop=FALSE]))
        colnames(ws) <- names(t)
        r <- c(r,list(ws))
    }
    names(r) <- names(compRes)
    r
}

## Though simpler and more elegant this one fails due to over-simplification of
## sapply when we have only one metric (and it did not worked with simplify=FALSE
## on sapply)
## .statScores.old <- function(compRes,stat=1) lapply(compRes,function(t) sapply(t,function(w) .scores2summary(w)[stat,,drop=FALSE]))


## calculates the scores of all iterations of an estimation exp
.scoresIts <- function(task,estTask,its) {
    trReq <- any(estTask@metrics %in% c("nmse","nmae","theil"))

    nIts <- length(its)
    if (estTask@evaluator=="" ) 
        if (is.classification(task)) estTask@evaluator <- "classificationMetrics"
        else                         estTask@evaluator <- "regressionMetrics"
    scores <- matrix(NA,nrow=nIts,ncol=length(estTask@metrics))
    colnames(scores) <- estTask@metrics
    wts <- intersect(estTask@metrics,c("trTime","tsTime","totTime"))
    predMs <- setdiff(estTask@metrics,wts)
    for(i in 1:nIts) {
        if (length(predMs)) {
            if (trReq) {
                scores[i,predMs] <- do.call(estTask@evaluator,
                                            c(list(trues=its[[i]]$preds[,"true"],
                                                   preds=its[[i]]$preds[,"predicted"],
                                                   stats=predMs,
                                                   train.y=eval(task@dataSource)[its[[i]]$train,task@target]),
                                              estTask@evaluator.pars))
            } else {
                scores[i,predMs] <- do.call(estTask@evaluator,
                                            c(list(trues=its[[i]]$preds[,"true"],
                                                   preds=its[[i]]$preds[,"predicted"],
                                                   stats=predMs),
                                              estTask@evaluator.pars))
            }
        }
        if (length(wts)) {
            allts <- as.numeric(its[[i]]$info$times)
            scores[i,wts] <- c(trTime=allts[1],tsTime=allts[2],totTime=allts[1]+allts[2])[wts]
        }
    }
    scores
}
