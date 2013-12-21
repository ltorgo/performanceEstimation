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
performanceEstimation <- function(tasks,workflows,setts,...) {

  if (!is(tasks,'list')) tasks <- list(tasks)
  if (!is(workflows,'list')) workflows <- list(workflows)
  
  if (is.null(names(workflows)))
    names(workflows) <- paste('var',1:length(workflows),sep='.')
  
  nTasks <- length(tasks)
  taskNames <- sapply(tasks,function(x) x@name)
  nWFs <- length(workflows)
  wfNames <- sapply(workflows,function(x) x@name)
  
  allRes <- vector("list",nTasks)
  names(allRes) <- taskNames
  
  cat('\n\n##### PERFORMANCE ESTIMATION USING ',
      switch(class(setts),
             CvSettings='CROSS VALIDATION',
             HldSettings='HOLD OUT',
             McSettings='MONTE CARLO',
             BootSettings='BOOTSTRAP',
             LoocvSettings='LOOCV',
             ),
      ' #####')
  
  for(d in 1:length(tasks)) {

    cat('\n\n** PREDICTIVE TASK ::',tasks[[d]]@name)

    taskRes <- vector("list",nWFs)
    names(taskRes) <- wfNames
    ##rr <- NULL
    for (s in 1:length(workflows)) {

      cat('\n\n++ MODEL/WORKFLOW ::',workflows[[s]]@func,
          ' variant -> ',names(workflows)[s],'\n')

      taskRes[[s]] <- do.call(
               switch(class(setts),
                      CvSettings='cvEstimates',
                      HldSettings='hldEstimates',
                      BootSettings='bootEstimates',
                      McSettings='mcEstimates',
                      LoocvSettings='loocvEstimates'
                      ),

               c(list(workflows[[s]],
                    tasks[[d]],
                    setts),...)
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
##                      CvSettings(1,10,1234))
##
cvEstimates <- function(wf,task,sets) {

  show(sets)

  ## Did the user supplied the data splits for all folds and repetitions?
  userSplit <- !is.null(sets@dataSplits)
  
  n <- nrow(task@data)
  if (!userSplit) n.each.part <- n %/% sets@nFolds

  #results <- EstimationResults(task,wf,sets,matrix(NA,0,0))
  #itsI <- results <- NULL
  scores <- NULL
  preds <- vector("list",sets@nFolds*sets@nReps)
  info <- vector("list",sets@nFolds*sets@nReps)

  if (!userSplit && sets@strat) {  # stratified sampling
    respVals <- responseValues(task@formula,task@data)
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
    bct <- bc %/% sets@nFolds
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
  
  for(r in 1:sets@nReps) {
    cat('Repetition ',r,'\nFold:')

    if (!userSplit) {
      set.seed(sets@seed*r)
      permutation <- sample(n)
      perm.data <- task@data[permutation,]
    } else perm.data <- task@data

    for(i in seq(sets@nFolds)) {
      itN <- (r-1)*sets@nFolds+i  # the iteration number
      cat(' ',i)
      
      if (!userSplit) {
        if (sets@strat) {
          out.fold <- c()
          for(x in seq(along=levels(b))) 
            if (bct[x]) out.fold <- c(out.fold,which(b == levels(b)[x])[((i-1)*bct[x]+1):((i-1)*bct[x]+bct[x])])
        } else {
          out.fold <- ((i-1)*n.each.part+1):(i*n.each.part)
        }
      } else out.fold <- outFold(sets@dataSplits,i,r)
      
      it.res <- runWorkflow(wf,
                            task@formula,
                            perm.data[-out.fold,],
                            perm.data[out.fold,])
      
      scores <- rbind(scores,it.res@scores)
      preds[[itN]] <- it.res@predictions
      info[[itN]] <- it.res@extraInfo
      
    }
    cat('\n')
  }

  ## randomize the number generator to avoid undesired
  ## problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))

  EstimationResults(task,wf,sets,scores,preds,info)
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
#              hldSettings(4,0.25,1234))
#
hldEstimates <- function(wf,task,sets) {

  show(sets)

  ## Did the user supplied the data splits for all folds and repetitions?
  userSplit <- !is.null(sets@dataSplits)

  n <- nrow(task@data)
  if (!userSplit) n.test <- as.integer(n * sets@hldSz)

  scores <- NULL
  preds <- vector("list",sets@nReps)
  info <- vector("list",sets@nReps)

  
  if (!userSplit & sets@strat) {  # stratified sampling
    respVals <- responseValues(task@formula,task@data)
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
    bct <- as.integer(bc * sets@hldSz)
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
  for(r in 1:sets@nReps) {
    cat(' ',r)

    if (!userSplit) {
      set.seed(sets@seed*r)
      permutation <- sample(n)
      perm.data <- task@data[permutation,]
    } else perm.data <- task@data

    if (!userSplit) {
      if (sets@strat) {
        out.fold <- c()
        for(x in seq(along=levels(b))) 
          if (bct[x]) out.fold <- c(out.fold,which(b == levels(b)[x])[1:bct[x]])
      } else {
        out.fold <- 1:n.test
      }
    } else out.fold <- outFold(sets@dataSplits,r)

    it.res <- runWorkflow(wf,
                          task@formula,
                          perm.data[-out.fold,],
                          perm.data[out.fold,])

    scores <- rbind(scores,it.res@scores)
    preds[[r]] <- it.res@predictions
    info[[r]] <- it.res@extraInfo
          
  }
  cat('\n')
  
  # randomize the number generator to avoid undesired
  # problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))

  EstimationResults(task,wf,sets,scores,preds,info)
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
loocvEstimates <- function(wf,task,sets,verbose=FALSE) {

  show(sets)

  n <- nrow(task@data)

  scores <- NULL
  preds <- vector("list",1)
  info <- vector("list",n)


  if (verbose) cat('Iteration: ')
  for(r in 1:n) {
    if (verbose) cat('*')

    set.seed(sets@seed*r)

    it.res <- runWorkflow(wf,
                          task@formula,
                          task@data[-r,],
                          task@data[r,])
    
    scores <- rbind(scores,it.res@scores)
    preds[[1]] <- rbind(preds[[1]],it.res@predictions)
    info[[r]] <- it.res@extraInfo
      
  }
  if (verbose) cat('\n')
  
  EstimationResults(task,wf,sets,scores,preds,info)

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
#                      bootSettings(1234,10))
#
bootEstimates <- function(wf,task,sets,verbose=TRUE) {

  show(sets)

  if (sets@type == '.632')
      resub <- runWorkflow(wf,task@formula,task@data,task@data)
      

  ## Did the user supplied the data splits for all folds and repetitions?
  userSplit <- !is.null(sets@dataSplits)

  n <- nrow(task@data)

  scores <- NULL
  preds <- vector("list",sets@nReps)
  info <- vector("list",sets@nReps)

  cat('Repetition :')
  for(r in 1:sets@nReps) {
    cat(' ',r)

    if (!userSplit) {
      set.seed(sets@seed*r)
      idx <- sample(n,n,replace=T)
    } else idx <- (1:n)[-outFold(sets@dataSplits,r)]
    
    it.res <- runWorkflow(wf,
                          task@formula,
                          task@data[idx,],
                          task@data[-idx,])

    if (sets@type == ".632") scores <- rbind(scores,0.632*it.res@scores+0.368*resub@scores)
    else scores <- rbind(scores,it.res@scores)
    
    preds[[r]] <- it.res@predictions
    info[[r]] <- it.res@extraInfo

      
  }
  cat('\n')

  # randomize the number generator to avoid undesired
  # problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))
  
  EstimationResults(task,wf,sets,scores,preds,info)

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

mcEstimates <- function(wf,
                       task,
                       mcSet,verbose=TRUE) {

  show(mcSet)

  ## Did the user supplied the data splits for all  repetitions?
  userSplit <- !is.null(mcSet@dataSplits)
  
  scores <- NULL
  preds <- vector("list",mcSet@nReps)
  info <- vector("list",mcSet@nReps)

  n <- NROW(task@data)

  if (!userSplit) {
      train.size <- if (mcSet@szTrain < 1) as.integer(n*mcSet@szTrain) else mcSet@szTrain
      test.size <- if (mcSet@szTest < 1) as.integer(n*mcSet@szTest) else mcSet@szTest
      if (n-test.size+1 <= train.size+1) stop('mcEstimates:: Invalid train/test sizes.')
  } else {
      train.size <- NROW(mcSet@dataSplits[mcSet@dataSplits[,1] == "TRAIN" & mcSet@dataSplits[,3]==1 & mcSet@dataSplits[,4]==1,2])
      test.size <- NROW(mcSet@dataSplits[mcSet@dataSplits[,1] == "TEST" & mcSet@dataSplits[,3]==1 & mcSet@dataSplits[,4]==1,2])
  }
  
  set.seed(mcSet@seed)

  if (!userSplit) {
      selection.range <- (train.size+1):(n-test.size+1)
      starting.points <- sort(sample(selection.range,mcSet@nReps))
  } else {
      starting.points <- sapply(1:mcSet@nReps,function(r) mcSet@dataSplits[mcSet@dataSplits[,1] == "TEST" & mcSet@dataSplits[,3]==1 & mcSet@dataSplits[,4]==r,2][1])
  }


  # main loop over all repetitions
  for(it in seq(along=starting.points)) {
    start <- starting.points[it]

    if (verbose)  cat('Repetition ',it,'\n\t start test = ',
                      start,'; test size = ',test.size,'\n')

    if (!userSplit) {
        rep.res <- runWorkflow(wf,
                               task@formula,
                               task@data[(start-train.size):(start-1),],
                               task@data[start:(start+test.size-1),])
    } else {
        rep.res <- runWorkflow(wf,
                               task@formula,
                               task@data[mcSet@dataSplits[mcSet@dataSplits[,1] == "TRAIN" & mcSet@dataSplits[,3]==1 & mcSet@dataSplits[,4]==it,2],],
                               task@data[mcSet@dataSplits[mcSet@dataSplits[,1] == "TEST" & mcSet@dataSplits[,3]==1 & mcSet@dataSplits[,4]==it,2],])

    }

    scores <- rbind(scores,rep.res@scores)
    preds[[it]] <- rep.res@predictions
    info[[it]] <- rep.res@extraInfo


  }
  if (verbose) cat('\n')

  ## randomize the number generator to avoid undesired
  ## problems caused by inner set.seed()'s
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date()," ")[[1]][4],":")))))

  EstimationResults(task,wf,mcSet,scores,preds,info)
}




# =====================================================
# Small utility functions 
# =====================================================

is.regression <- function(formula,data) is.numeric(model.response(model.frame(formula,data)))

is.classification <- function(formula,data) is.factor(model.response(model.frame(formula,data)))

responseValues <- function(formula,data) model.response(model.frame(formula,data))


##outFold <- function(ds,f,r)
##  unlist(subset(ds,ds[,1] == "TEST" & ds[,3]==f & ds[,4]==r,colnames(ds)[2]))
outFold <- function(ds,f,r=NULL)  {
    if (is.null(r)) which(ds[,f])
    else which(ds[[r]][,f])
}


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
    for(t in compRes@tasks) {
        ws <- NULL
        for(w in t)
            ws <- cbind(ws,t(.scores2summary(w)[stat,,drop=FALSE]))
        colnames(ws) <- names(t)
        r <- c(r,list(ws))
    }
    names(r) <- names(compRes@tasks)
    r
}

## Though simpler and more elegant this one fails due to over-simplification of
## sapply when we have only one metric (and it did not worked with simplify=FALSE
## on sapply)
## .statScores.old <- function(compRes,stat=1) lapply(compRes@tasks,function(t) sapply(t,function(w) .scores2summary(w)[stat,,drop=FALSE]))
