library(DMwR)
source('newDMwR.R')
data(algae)
DSs <- sapply(names(algae)[12:18],
         function(x,names.attrs) { 
           f <- as.formula(paste(x,"~ ."))
           dataset(f,algae[,c(names.attrs,x)],x) 
         },
         names(algae)[1:11])

WFs <- list()
WFs$svm <- list(cost=c(10,150,300),gamma=c(0.01,0.001))
WFs$randomForest <- list(mtry=c(5,7),ntree=c(500,750,1500))
WFs$earth <- list(nk=c(10,17),degree=c(1,2),thresh=c(0.01,0.001))

for(d in seq_along(DSs)) {
  for(w in names(WFs)) {
    resObj <- paste(names(DSs)[d],w,'Res',sep='')
    assign(resObj,
           experimentalComparison(
                  DSs[d],         
                  c(
                    do.call('variants',
                            c(list('regrWF',learner=w),WFs[[w]],
                              varsRootName=paste('regrWF',w,sep='.')))
                    ),
                   cvSettings(3,10,1234))
           )

    save(list=resObj,file=paste(names(DSs)[d],w,'Rdata',sep='.'))
  }
}


#" Now the part of result collections
nD <- paste('a',1:7,sep='')
nL <- c('svm','randomForest','earth')
res <- NULL
for(d in nD) {
  resD <- NULL
  for(l in nL) {
    load(paste(d,l,'Rdata',sep='.'))
    x <- get(paste(d,l,'Res',sep=''))
    resD <- if (is.null(resD)) x else join(resD,x,by='variants')
  }
  res <- if (is.null(res)) resD else join(res,resD,by='datasets')
}


## An experiment with nominal time series
library(quantmod)
getSymbols('^GSPC',from='2008-01-01',to='2012-12-31')
data.model <- specifyModel(
  Next(100*Delt(Ad(GSPC))) ~ Delt(Ad(GSPC),k=1:10)+Delt(Vo(GSPC),k=1:3))
data <- as.data.frame(modelData(data.model))
colnames(data)[1] <- 'signal'
data$signal <- trading.signals(data$signal,b.t=2.5,s.t=-2.5)
spExp <- experimentalComparison(
  c(dataset(signal ~ .,data[-nrow(data),],'SP500_2012')),
  c(standRF=learner('classWF',pars=list(learner='randomForest',
                                       ntree=500)),
    slideRF=learner('tsClassWF',pars=list(type='slide',learner='randomForest',
                                            ntree=500,relearn.step=5))),
  mcSettings(2,0.5,0.25))
