
#################################################################
## Functions calculating evaluation metrics
#################################################################


# =====================================================================
# Function to calculate some standard regression evaluation statistics
# ---------------------------------------------------------------------
# L. Torgo (2009)
#
# Examples:
# s <- regressionMetrics(tr,ps,train.y=data[,'Y'])
# s <- regressionMetrics(tr,ps,stats=c('mse','mae'))
#
regressionMetrics <- function(trues,preds,
                      stats="mse",
                      train.y=NULL)
{
    knownMetrics <- c('mae','mse','rmse','mape','nmse','nmae')
    if (identical(stats,"all"))
        stats <- if (is.null(train.y)) setdiff(knownMetrics,c("nmse","nmae")) else knownMetrics
    
    if (any(c('nmse','nmad') %in% stats) && is.null(train.y))
        stop('regressionMetrics:: train.y parameter not specified.',call.=FALSE)
    if (!all(stats %in% knownMetrics))
        stop("regressionMetrics:: don't know how to calculate -> ",call.=FALSE,
             paste(stats[which(!(stats %in% knownMetrics))],collapse=','))
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
# s <- classificationMetrics(tr,ps)
# s <- classificationMetrics(tr,ps,benMtrx=matrix(c(2,-13,-4,5),2,2))
#
classificationMetrics <- function(trues,preds,
                 stats="err",
                 benMtrx=NULL,
                 allCls=unique(c(levels(as.factor(trues)),levels(as.factor(preds)))),
                 posClass=allCls[1],
                 beta=1
                 )

{
    twoClsMetrics <- c('fpr','fnr','tpr','tnr','rec','sens','spec',
                       'prec','rpp','lift','F')
    knownMetrics <- c(twoClsMetrics,c('acc','err','totU',
                      'microF','macroF',"macroRec","macroPrec"))

    if (identical(stats,"all")) {
        stats <- knownMetrics
        if (length(allCls) > 2) stats <- setdiff(stats,twoClsMetrics)
        if (is.null(benMtrx))   stats <- setdiff(stats,'totU')
    }

    if (any(twoClsMetrics %in% stats) && length(allCls) > 2)
          stop("classificationMetrics:: some of the metrics are only available for two class problems.",call.=FALSE)
    if (any(c('totU') %in% stats) && is.null(benMtrx))
      stop('classificationMetrics:: benMtrx parameter not specified.',call.=FALSE)
    if (!all(stats %in% knownMetrics))
      stop("classificationMetrics:: don't know how to calculate -> ",call.=FALSE,
           paste(stats[which(!(stats %in% knownMetrics))],collapse=','))

    r <- rep(NA,length(knownMetrics))
    names(r) <- knownMetrics

    ## copying with eventually missing class labels
    preds <- factor(preds,levels=allCls)
    trues <- factor(trues,levels=allCls)
    
    N <- length(trues)
    cm <- as.matrix(table(trues,preds))

    a <- sum(diag(cm))/N
    r[c('acc','microF','err')] <- c(a,a,1-a)

    if (length(allCls) == 2) {
        negClass <- setdiff(allCls,posClass)
        r[c('tpr','rec','sens')] <- cm[posClass,posClass]/sum(cm[posClass,])
        r['spec'] <- cm[negClass,negClass]/sum(cm[negClass,])
        r['fpr'] <- cm[negClass,posClass]/sum(cm[,posClass])
        r['fnr'] <- cm[posClass,negClass]/sum(cm[,posClass])
        r['tnr'] <- cm[posClass,negClass]/sum(cm[,negClass])
        r['prec'] <- cm[posClass,posClass]/sum(cm[,posClass])
        r['rpp'] <- (cm[posClass,posClass] +cm[negClass,posClass])/N
        r['lift'] <- r['rec']/sum(cm[,posClass])
        r['F'] <- (1+beta^2)*r['prec']*r['rec']/(beta^2*r['prec']+r['rec'])
    }

    if (any(c("macroF","macroRec","macroPrec") %in% stats)) {
        F <- R <- P <- 0
        for(cl in allCls) {
            pr <- cm[cl,cl]/sum(cm[,cl])
            rc <- cm[cl,cl]/sum(cm[cl,])
            F <- F+(1+beta^2)*pr*rc/(beta^2*pr+rc)
            P <- P + pr
            R <- R + rc
        }
        r["macroF"] <- F/length(allCls)
        r["macroRec"] <- R/length(allCls)
        r["macroPrec"] <- P/length(allCls)
    }
    
    
    if (!is.null(benMtrx))
      if (!all(dim(cm)==dim(benMtrx)))
        stop("classificationMetrics:: dimensions of confusion and cost/benefit matrices do not match",call.=FALSE)
      else r['totU'] <- sum(cm*benMtrx)
    
    return(r[stats])
}   


# =====================================================================
# Function to calculate some standard  evaluation statistics for time series
# problems
# ---------------------------------------------------------------------
# L. Torgo (2013)
#
# Examples:
# s <- timeseriesMetrics(tr,ps,train.y=data[,'Y'])
# s <- timeseriesMetrics(tr,ps,stats=c('mse','mae'))
#
timeseriesMetrics <- function(trues,preds,
                    stats='mse',
                    train.y=NULL)
{
    if (('theil' %in% stats) && is.null(train.y))
        stop('timeseriesMetrics:: train.y parameter not specified.',call.=FALSE)

    other <- setdiff(stats,'theil')
    r <- if (length(other) > 0) regressionMetrics(trues,preds,other,train.y) else c()
    if ('theil' %in% stats) r <- c(r,theil=sum((trues-preds)^2)/sum((c(train.y[length(train.y)],trues[-length(trues)])-preds)^2))

    if (identical(stats,"all")) return(r) else return(r[stats])
}




