
pcs <-  function(obj,baseline,maxs=rep(FALSE,length(metricNames(obj))),p.val=0.05) {
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
    
    #scores <- vector("list",nms)
    #rks <- vector("list",nms)
    #avgRksWFs <- vector("list",nms)
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
#            names(compResults[[p]]$WilcoxonSignedRank.test$p.values) <- other
#            names(compResults[[p]]$t.test$p.values) <- other
        
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

