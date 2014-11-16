# =====================================================
# Finding the best performing workflows
# =====================================================
# Luis Torgo, Nov 2013
#
topPerformers <- function(compRes,maxs=rep(FALSE,dim(compRes[[1]][[1]]@iterationsScores)[2]),digs=3) {
  if (!inherits(compRes,'ComparisonResults')) stop(compRes,' needs to be of class "ComparisonResults".\n')
  if (length(maxs) == 1) maxs <- rep(maxs,dim(compRes[[1]][[1]]@iterationsScores)[2])
  else if (length(maxs) != dim(compRes[[1]][[1]]@iterationsScores)[2]) stop('"maxs" needs to have the same size as the number of evaluation statistics.\n')

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
rankWorkflows <- function(compRes,top=min(5,length(workflowNames(compRes))),maxs=rep(FALSE,dim(compRes[[1]][[1]]@iterationsScores)[2])) {
  if (!inherits(compRes,'ComparisonResults')) stop(compRes,' needs to be of class "ComparisonResults".\n')
  if (length(maxs) == 1) maxs <- rep(maxs,dim(compRes[[1]][[1]]@iterationsScores)[2])
  else if (length(maxs) != dim(compRes[[1]][[1]]@iterationsScores)[2]) stop('"maxs" needs to have the same size as the number of evaluation statistics.\n')

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
  lapply(compRes,function(t)
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
pairedComparisons <-  function(obj,baseline,
                               maxs=rep(FALSE,length(metricNames(obj))),
                               p.val=0.05) {
    
    if (!inherits(obj,'ComparisonResults')) stop(obj,' is not of class "ComparisonResults".\n')

    ## basic information on the expriment
    ts <- taskNames(obj);     nts <- length(ts)
    ws <- workflowNames(obj); nws <- length(ws)
    ms <- metricNames(obj);   nms <- length(ms)

    if (missing(baseline))  # using the first workflow as baseline if none indicated
        baseline <- topPerformer(obj,ms[1],ts[1])@name
    other <- setdiff(ws,baseline)
    pb <- which(ws==baseline)

    ## this list will hold the results of the comparisons, one for each metric
    compResults <- vector("list",nms)
    names(compResults) <- ms

    ## Constructing the tests information for each metric
    for(p in 1:nms) {
        compResults[[p]] <- list()
        compResults[[p]]$setup <- list(nTasks=nts,nWorkflows=nws)
        compResults[[p]]$scores <- t(sapply(obj,function(m) sapply(m,function(i) mean(i@iterationsScores[,p],na.rm=TRUE))))
        compResults[[p]]$rks <- t(apply(if (maxs[p]) -compResults[[p]]$scores else compResults[[p]]$scores,1,rank))
        compResults[[p]]$avgRksWFs <- apply(compResults[[p]]$rks,2,mean)

        ## Wilcoxon Signed Rank and t-Student tests
        compResults[[p]]$t.test <- array(NA,dim=c(nws,3,nts),
                 dimnames=list(c(baseline,other),c("Score","DiffAvgScores","p.value"),
                     ts)
                         )
        compResults[[p]]$WilcoxonSignedRank.test <- array(NA,dim=c(nws,3,nts),
                 dimnames=list(c(baseline,other),c("Score","DiffAvgScores","p.value"),
                     ts)
                         )
        for(t in ts) {
            compResults[[p]]$WilcoxonSignedRank.test[baseline,,t] <- NA
            compResults[[p]]$WilcoxonSignedRank.test[baseline,"Score",t] <- compResults[[p]]$scores[t,baseline]
            compResults[[p]]$t.test[baseline,,t] <- NA
            compResults[[p]]$t.test[baseline,"Score",t] <- compResults[[p]]$scores[t,baseline]
            for(o in other) {
                tst <- try(wilcox.test(obj[[t]][[o]]@iterationsScores[,p],
                                       obj[[t]][[baseline]]@iterationsScores[,p],
                                       paired=T))
                compResults[[p]]$WilcoxonSignedRank.test[o,"DiffAvgScores",t] <- compResults[[p]]$scores[t,baseline] - compResults[[p]]$scores[t,o]
                compResults[[p]]$WilcoxonSignedRank.test[o,"Score",t] <- compResults[[p]]$scores[t,o]
                compResults[[p]]$WilcoxonSignedRank.test[o,"p.value",t] <-
                    if (inherits(tst,"try-error"))  NA else tst$p.value
                tst <- try(t.test(obj[[t]][[o]]@iterationsScores[,p],
                                  obj[[t]][[baseline]]@iterationsScores[,p],
                                  paired=T))
                compResults[[p]]$t.test[o,"DiffAvgScores",t] <- compResults[[p]]$scores[t,baseline] - compResults[[p]]$scores[t,o]
                compResults[[p]]$t.test[o,"Score",t] <- compResults[[p]]$scores[t,o]
                compResults[[p]]$t.test[o,"p.value",t] <-
                    if (inherits(tst,"try-error"))  NA else tst$p.value
            }
        }
        
        ## Testing the null hypothesis that all WFs are equivalent
        chi <- 12*nts/(nws*(nws+1)) * (sum(compResults[[p]]$avgRksWFs^2) - (nws*(nws+1)^2)/4)
        FF <- (nts-1)*chi / (nts*(nws-1) - chi)
        critVal <- df(1-p.val,nws-1,(nws-1)*(nts-1))
        rejNull <- FF > critVal
        compResults[[p]]$F.test <- list(chi=chi,FF=FF,critVal=critVal,rejNull=rejNull)
        
        compResults[[p]]$Nemenyi.test <- NA
        compResults[[p]]$BonferroniDunn.test <- NA
        if (rejNull) {
            ## Nemenyi critical difference
            CD.n <- qtukey(1-p.val,nws,1e06)/sqrt(2)*sqrt(nws*(nws+1)/(6*nts))
            allRkDifs <- outer(compResults[[p]]$avgRksWFs,compResults[[p]]$avgRksWFs,
                               function(x,y) abs(x-y))
            signifDifs <- allRkDifs >= CD.n
            compResults[[p]]$Nemenyi.test <- list(critDif=CD.n,
                                                  rkDifs=allRkDifs,
                                                  signifDifs=signifDifs)
            
            ## Bonferroni-Dunn test against the baseline
            
            ## Bonferroni-Dunn critical difference
            CD.bd <- qtukey(1-(p.val/(nws-1)),2,1e06)/sqrt(2)*sqrt(nws*(nws+1)/(6*nts))
            diffs2baseline <- abs(compResults[[p]]$avgRksWFs[-pb]-compResults[[p]]$avgRksWFs[pb])
            signifDifs <- diffs2baseline >= CD.bd
            compResults[[p]]$BonferroniDunn.test <- list(critDif=CD.bd,
                                                         baseline=baseline,
                                                         rkDifs=diffs2baseline,
                                                         signifDifs=signifDifs)

        }
    }
    compResults
}



# ======================================================================
# Filtering the results of a call to pairedComparisons by a minimum p.value
# =====================================================
# Luis Torgo, Jan-Aug 2009, 2014
# =====================================================
signifDiffs <- function(ps,p.limit=0.05,metrics=names(ps),tasks=rownames(ps[[1]]$scores)) {
    res <- vector("list",length(ps))
    names(res) <- names(ps)
    for(p in names(ps)) {
        res[[p]] <- list()
        res[[p]]$WilcoxonSignedRank.test <- vector("list",length(tasks))
        res[[p]]$t.test <- vector("list",length(tasks))
        names(res[[p]]$WilcoxonSignedRank.test) <- names(res[[p]]$t.test) <- tasks
        for(t in tasks) {
            res[[p]]$WilcoxonSignedRank.test[[t]] <- ps[[p]]$WilcoxonSignedRank.test[which(ps[[p]]$WilcoxonSignedRank.test[,"p.value",t] < p.limit | is.na(ps[[p]]$WilcoxonSignedRank.test[,"p.value",t])),,t]
            res[[p]]$t.test[[t]] <- ps[[p]]$t.test[which(ps[[p]]$t.test[,"p.value",t] < p.limit | is.na(ps[[p]]$t.test[,"p.value",t])),,t]
        }
    }
    res
}


    

