# =====================================================
# Finding the best performing workflows
# =====================================================
# Luis Torgo, Nov 2013
#
topPerformers <- function(compRes,maxs=rep(FALSE,dim(compRes@tasks[[1]][[1]]@iterationsScores)[2]),digs=3) {
  if (!inherits(compRes,'ComparisonResults')) stop(compRes,' needs to be of class "ComparisonResults".\n')
  if (length(maxs) == 1) maxs <- rep(maxs,dim(compRes@tasks[[1]][[1]]@iterationsScores)[2])
  else if (length(maxs) != dim(compRes@tasks[[1]][[1]]@iterationsScores)[2]) stop('"maxs" needs to have the same size as the number of evaluation statistics.\n')

  avgScs <- .statScores(compRes,'avg')
  bs <- lapply(avgScs,function(t) {
      r <- data.frame(Workflow=rep('',NROW(t)),Estimate=rep(0,NROW(t)),stringsAsFactors=F,row.names=rownames(t))
      for(i in 1:NROW(t))
          r[i,] <- if (maxs[i]) c(colnames(t)[which.max(t[i,])],round(max(t[i,],na.rm=TRUE),digs)) else c(colnames(t)[which.min(t[i,])],round(min(t[i,],na.rm=TRUE),digs))
      r
  })
  bs
}

# =====================================================
# Finding the best performing workflow for a given task and metric
# =====================================================
# Luis Torgo, Nov 2013
#
topPerformer <- function(compRes,metric,task,max=FALSE) {
  if (!inherits(compRes,'ComparisonResults'))
      stop(compRes,' needs to be of class "ComparisonResults".\n')
  if (!(metric %in% metricNames(compRes)))
      stop(metric,' not estimated in this "ComparisonResults" object.\n')
  if (!(task %in% taskNames(compRes)))
      stop(task,' not used in this "ComparisonResults" object.\n')

  avgScs <- .statScores(compRes,'avg')
  scs <- avgScs[[task]]
  w <- if (max) colnames(scs)[which.max(scs[metric,])] else colnames(scs)[which.min(scs[metric,])]
  getWorkflow(w,compRes)
}

# =====================================================
# Obtaining a ranking of the workflows
# =====================================================
# Luis Torgo, Nov 2013
#
rankWorkflows <- function(compRes,top=min(5,length(workflowNames(res))),maxs=rep(FALSE,dim(compRes@tasks[[1]][[1]]@iterationsScores)[2])) {
  if (!inherits(compRes,'ComparisonResults')) stop(compRes,' needs to be of class "ComparisonResults".\n')
  if (length(maxs) == 1) maxs <- rep(maxs,dim(compRes@tasks[[1]][[1]]@iterationsScores)[2])
  else if (length(maxs) != dim(compRes@tasks[[1]][[1]]@iterationsScores)[2]) stop('"maxs" needs to have the same size as the number of evaluation statistics.\n')

  avgScs <- .statScores(compRes,'avg')
  lapply(avgScs,function(t) {
      l <- vector("list",nrow(t))
      for(i in 1:nrow(t))
          l[[i]] <- data.frame(Workflow=colnames(t)[order(t[i,],decreasing=maxs[i])],Estimate=sort(t[i,],decreasing=maxs[i]),stringsAsFactors=F,row.names=1:ncol(t))[1:top,]
      names(l) <- rownames(t)
      l
  })
}
      
# =====================================================
# Apply a summary function over the individual iteration scores
# =====================================================
# Luis Torgo, Nov 2013
# =====================================================
# 
metricsSummary <- function(compRes,summary='mean',...) {
  if (!inherits(compRes,'ComparisonResults'))
    stop(compRes,' needs to be of class "ComparisonResults".\n')
  lapply(compRes@tasks,function(t)
         sapply(t,function(w)
                apply(w@iterationsScores,2,function(x) do.call(summary,list(x,...)))
                ))
}



# ======================================================================
# Construction of comparative analysis tables based on the results of 
# comparative experiment obtained with experimentalComparison() (i.e. based
# on a compExp object).
# The first argument is the compExp object that resulted from the 
# experiments. Then we have the system against which all remaning are
# compared to (defaults to first in the structure). Finally we can
# provide a vector of the names of the statistics we which to get a
# table (defaults to all).
# =====================================================
# Luis Torgo, Jan-Aug 2009, 2014
# =====================================================
pairedComparisons <-  function(obj,baseline,test="wilcoxon") {
    if (!inherits(obj,'ComparisonResults')) stop(obj,' is not of class "ComparisonResults".\n')

    if (! (test %in% c('wilcoxon','t.test')))
        stop("pairedComparison: valid tests are 'wilcoxon' or 't.test'.")
    else t.func <- switch(test,
                          wilcoxon="wilcox.test",
                          t.test="t.test")
    
    ts <- taskNames(obj);     nts <- length(ts)
    ws <- workflowNames(obj); nws <- length(ws)
    ms <- metricNames(obj);   nms <- length(ms)

    if (missing(baseline))  # using the first workflow as baseline if none indicated
        baseline <- topPerformer(obj,ms[1],ts[1])@name

    other <- setdiff(ws,baseline)

    ## the results of the paired comparisons against the baseline (will be on first row)
    res <- array(NA,dim=c(nws,4,nms,nts),
                 dimnames=list(c(baseline,other),
                               c("AvgScore","SdScore","Diff","p.value"),
                               ms,
                               ts)
                 )


    for(t in ts) {
        ## get the results of the baseline
        res[baseline,1:2,,t] <- t(apply(obj@tasks[[t]][[baseline]]@iterationsScores,2,
                             function(x) c(AvgScore=mean(x,na.rm=TRUE),SdScore=sd(x,na.rm=TRUE))))
        for(m in ms) {
            res[baseline,"AvgScore",m,t] <- mean(obj@tasks[[t]][[baseline]]@iterationsScores[,m],
                                                 na.rm=TRUE)
            res[baseline,"SdScore",m,t] <- sd(obj@tasks[[t]][[baseline]]@iterationsScores[,m],
                                              na.rm=TRUE)
            ## get the results of the others and compare
            for(o in other) {
                #oRes <- obj@tasks[[t]][[o]]@iterationsScores

                a <- mean(obj@tasks[[t]][[o]]@iterationsScores[,m],na.rm=TRUE)
                s <- sd(obj@tasks[[t]][[o]]@iterationsScores[,m],na.rm=TRUE)
                tst <- try(w <- do.call(t.func,
                                        list(obj@tasks[[t]][[o]]@iterationsScores[,m],
                                             obj@tasks[[t]][[baseline]]@iterationsScores[,m],
                                             paired=T))
                         )
                p <- if (inherits(tst,"try-error"))  NA else tst$p.value
                res[o,,m,t] <- c(a,s,res[baseline,'AvgScore',m,t]-a,p)
            }
            
        }
    }
    
    res
}


signifDiffs <- function(ps,p.limit=0.05,metrics=dimnames(ps)[[3]],tasks=dimnames(ps)[[4]]) {
    res <- list()
    theOnes <- ps[,'p.value',,,drop=FALSE] < p.limit | is.na(ps[,'p.value',,,drop=FALSE])
    for(t in tasks) {
        res[[t]] <- list()
        for(m in metrics) {
            rs <- which(theOnes[,1,m,t])
            if (length(rs)>1) res[[t]][[m]] <- ps[rs,,m,t]
        }
    }
    res
}


    
