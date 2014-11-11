
pcs <-  function(obj,baseline,test="wilcoxon") {
    if (!inherits(obj,'ComparisonResults')) stop(obj,' is not of class "ComparisonResults".\n')

    if (! (test %in% c('wilcoxon','t.test')))
        stop("pairedComparison: valid tests are 'wilcoxon' or 't.test'.")
    else t.func <- switch(test,
                          wilcoxon="wilcox.test",
                          t.test="t.test")



  
    ts <- taskNames(obj);     nts <- length(ts)
    ws <- workflowNames(obj); nws <- length(ws)
    ms <- metricNames(obj);   nms <- length(ms)

    scores <- vector("list",nms)
    rks <- vector("list",nms)
    avgRksWFs <- vector("list",nms)
    for(p in 1:nms) scores[[p]] <- t(sapply(obj,function(m) sapply(m,function(i) mean(i@iterationsScores[,p],na.rm=TRUE))))

    rks <- lapply(1:nms,function(i) t(apply(scores[[i]],1,rank)))
    avgRksWFs <- lapply(1:nms,function(i) apply(rks[[i]],2,mean))
    names(scores) <- names(rks) <- names(avgRksWFs) <- ms

    ## Testing the null hypothesis that all WFs are equivalent
    xi <- 12*nts/(nws*(nws+1)) * (sapply(avgRksWFs,function(r) sum(r*r)) - (nws*(nws+1)^2)/4)
    FFs <- (nts-1)*xi / (nts*(nws-1) - xi)

    p.val <- 0.05
    critVal <- df(1-p.val,nws-1,(nws-1)*(nts-1))
    rejNull <- FFs > critVal

    ## posthoc tests

    ## the critical difference
    CD.n <- qtukey(1-p.val,nws,1e06)/sqrt(2)*sqrt(nws*(nws+1)/(6*nts))
    CD.bd <- qtukey(1-(p.val/(nws-1)),nws,1e06)/sqrt(2)*sqrt(nws*(nws+1)/(6*nts))

    allRkDifs <- outer(avgRksWFs$mse,avgRksWFs$mse,function(x,y) abs(x-y))


    
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
        res[baseline,1:2,,t] <- t(apply(obj[[t]][[baseline]]@iterationsScores,2,
                             function(x) c(AvgScore=mean(x,na.rm=TRUE),SdScore=sd(x,na.rm=TRUE))))
        for(m in ms) {
            res[baseline,"AvgScore",m,t] <- mean(obj[[t]][[baseline]]@iterationsScores[,m],
                                                 na.rm=TRUE)
            res[baseline,"SdScore",m,t] <- sd(obj[[t]][[baseline]]@iterationsScores[,m],
                                              na.rm=TRUE)
            ## get the results of the others and compare
            for(o in other) {
                #oRes <- obj[[t]][[o]]@iterationsScores

                a <- mean(obj[[t]][[o]]@iterationsScores[,m],na.rm=TRUE)
                s <- sd(obj[[t]][[o]]@iterationsScores[,m],na.rm=TRUE)
                tst <- try(w <- do.call(t.func,
                                        list(obj[[t]][[o]]@iterationsScores[,m],
                                             obj[[t]][[baseline]]@iterationsScores[,m],
                                             paired=T))
                         )
                p <- if (inherits(tst,"try-error"))  NA else tst$p.value
                res[o,,m,t] <- c(a,s,res[baseline,'AvgScore',m,t]-a,p)
            }
            
        }
    }
    
    res
}

