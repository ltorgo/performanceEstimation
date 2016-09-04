## ----echo=FALSE----------------------------------------------------------
pckg <- packageDescription("performanceEstimation")
peN <- pckg$Package
peV <- pckg$Version

## ----eval=FALSE----------------------------------------------------------
#  install.packages("performanceEstimation")

## ----eval=FALSE----------------------------------------------------------
#  library(devtools)  # You need to install this package before!
#  install_github("ltorgo/performanceEstimation",ref="develop")

## ------------------------------------------------------------------------
library(performanceEstimation)  # Loading our infra-structure
library(e1071)                  # A package containing SVMs

data(iris)                      # The data set we are going to use

res <- performanceEstimation(
         PredTask(Species ~ .,iris),
         Workflow("standardWF",learner="svm"),
         EstimationTask(metrics="err",method=CV())
         )  

## ------------------------------------------------------------------------
summary(res)

## ----ex1Iris, echo=FALSE, fig.cap="The distribution of the error rate on the 10 folds. Red dots show the performance on each individual iteration of the estimation process.", fig.height=6, fig.width=6----
plot(res) 

## ------------------------------------------------------------------------
data(iris)
PredTask(Species ~ .,iris)

## ------------------------------------------------------------------------
data(iris)
PredTask(Species ~ .,iris,copy=TRUE)

## ----results='hide'------------------------------------------------------
library(performanceEstimation)
library(e1071)
data(Boston,package="MASS")                    

exp <- performanceEstimation(
         PredTask(medv ~ .,Boston),
         workflowVariants(wf="standardWF",learner="svm",
                          learner.pars=list(cost=1:5,gamma=c(0.1,0.05,0.01))),
         EstimationTask(metrics="mae",method=CV())
         )  

## ----results='hide'------------------------------------------------------
library(performanceEstimation)
library(DMwR)
data(iris)                    

res <- performanceEstimation(
         PredTask(Species ~ .,iris),
         workflowVariants(learner="rpartXse",
                          learner.pars=list(se=c(0,0.5,1)),
                          predictor.pars=list(type="class")),
         EstimationTask(metrics="acc",method=CV(nReps=2,nFolds=5))
         )  

## ----results='hide',message=FALSE----------------------------------------
data(Boston,package='MASS')
library(DMwR)
library(e1071)
library(randomForest)
bostonRes <- performanceEstimation(
  PredTask(medv ~ .,Boston),
  workflowVariants(learner=c('rpartXse','svm','randomForest')),
  EstimationTask(metrics="mse",method=CV())
  )

## ----results='hide', cache=TRUE------------------------------------------
data(Boston,package='MASS')
library(DMwR)
library(e1071)
library(randomForest)
bostonRes <- performanceEstimation(
  PredTask(medv ~ .,Boston),
  c(workflowVariants(learner='rpartXse',
                     learner.pars=list(se=c(0,1))),
    workflowVariants(learner='svm',
                     learner.pars=list(cost=c(1,5),gamma=c(0.01,0.1))),
    workflowVariants(learner='randomForest',
                     learner.pars=list(ntree=c(500,1000)))),
  EstimationTask(metrics="mse",method=CV())
  )

## ------------------------------------------------------------------------
library(performanceEstimation)
data(algae,package="DMwR")

res <- performanceEstimation(
         PredTask(a1 ~ .,algae[,1:12],"AlgaA1"),
         Workflow(learner="lm",pre=c("centralImp","scale")),
         EstimationTask(metrics="mae",method=CV())
         )

## ------------------------------------------------------------------------
library(performanceEstimation)
data(algae,package="DMwR")

res <- performanceEstimation(
         PredTask(a1 ~ .,algae[,1:12],"AlgaA1"),
         c(Workflow(wfID="lm",
                    learner="lm",
                    pre=c("centralImp","scale")),
           Workflow(wfID="lmOnlyPos",
                    learner="lm",
                    pre=c("centralImp","scale"),
                    post=c("onlyPos"))),
         EstimationTask(metrics="mae",method=CV())
         ) 
summary(res)

## ----eval=FALSE----------------------------------------------------------
#  library(performanceEstimation)
#  library(quantmod)
#  library(randomForest)
#  GSPC <- getSymbols('^GSPC',from='2008-01-01',to='2012-12-31',auto.assign = FALSE)
#  data.model <- specifyModel(
#    Next(100*Delt(Ad(GSPC))) ~ Delt(Ad(GSPC),k=1:10))
#  data <- as.data.frame(modelData(data.model))
#  colnames(data)[1] <- 'PercVarClose'
#  spExp <- performanceEstimation(
#    PredTask(PercVarClose ~ .,data,'SP500_2012'),
#    c(Workflow(wf='standardWF',wfID="standRF",
#               learner='randomForest',
#               learner.pars=list(ntree=500)),
#      Workflow(wf='timeseriesWF',wfID="slideRF",
#               learner='randomForest',
#               learner.pars=list(ntree=500),
#               type="slide",
#               relearn.step=30),
#      Workflow(wf='timeseriesWF',wfID="growRF",
#               learner='randomForest',
#               learner.pars=list(ntree=500),
#               type="grow",
#               relearn.step=30)
#     ),
#    EstimationTask(metrics=c("mse","theil"),
#                   method=MonteCarlo(nReps=5,szTrain=0.5,szTest=0.25))
#    )

## ----echo=FALSE----------------------------------------------------------
load("tsExp.Rdata")

## ----eval=FALSE----------------------------------------------------------
#  myWorkFlow <- function(form,train,test,...) {
#    require(mySpecialPackage,quietly=TRUE)
#    ## cary out some data pre-processing
#    myTrain <- mySpecificPreProcessingSteps(train)
#    ## now obtain the model
#    myModel <- myModelingTechnique(form,myTrain,...)
#    ## obtain the predictions
#    preds <- predict(myModel,test)
#    ## cary out some predictions post-processing
#    newPreds <- mySpecificPostProcessingSteps(form,train,test,preds)
#    names(newPreds) <- rownames(test)
#    ## finally produce the list containing the output of the workflow
#    res <- list(trues=responseValues(form,test),preds=newPreds)
#    return(res)
#  }

## ----eval=FALSE----------------------------------------------------------
#  workflowVariants('myWorkFlow',x=c(0,3,5,7),y=c(TRUE,FALSE))

## ----cache=FALSE,eval=TRUE-----------------------------------------------
RLensemble <- function(f, tr, ts, weightRT=0.5, step=FALSE, ..., .models=FALSE) {
  require(DMwR,quietly=TRUE)
  ## Getting the column id of the target variable
  tgtCol <- which(colnames(tr) == as.character(f[[2]]))
  ## filling in NAs using knnImputation
  noNAsTR <- tr
  noNAsTS <- ts
  noNAsTR[,-tgtCol] <- knnImputation(tr[,-tgtCol])
  noNAsTS[,-tgtCol] <- knnImputation(ts[,-tgtCol],distData=tr[,-tgtCol])
  r <- rpartXse(f,tr,...)
  l <- lm(f,noNAsTR)
  if (step) l <- step(l,trace=0)
  pr <- predict(r,ts)
  pl <- predict(l,noNAsTS) 
  ps <- weightRT*pr+(1-weightRT)*pl
  names(ps) <- rownames(ts)
  res <- list(trues=responseValues(f,ts),preds=ps)
  if (.models) res <- c(res,list(linearModel=l,tree=r))
  res
}

## ----results="hide",eval=FALSE-------------------------------------------
#  data(algae,package='DMwR')
#  expRes <- performanceEstimation(
#      PredTask(a1 ~ .,algae[,1:12],'alga1'),
#      workflowVariants('RLensemble',
#                       se=c(0,1),step=c(TRUE,FALSE),weightRT=c(0.4,0.5,0.6)),
#      EstimationTask("mse",method=CV()))

## ------------------------------------------------------------------------
powErr <- function(trues,preds,pow=3,...) {
    c(pow.err = mean((trues-preds)^pow))
}

## ----eval=FALSE----------------------------------------------------------
#  EstimationTask(metrics="pow.err",method=CV(),
#                 evaluator="powErr",evaluator.pars=list(pow=4))

## ------------------------------------------------------------------------
EstimationTask(evaluator="powErr",evaluator.pars=list(pow=4))

## ----eval=FALSE----------------------------------------------------------
#  data(BreastCancer,package='mlbench')
#  library(e1071)
#  bcExp <- performanceEstimation(
#    PredTask(Class ~ .,BreastCancer[,-1],'BreastCancer'),
#    workflowVariants('standardWF',
#             learner='svm',
#             learner.pars=list(cost=c(1,5),gamma=c(0.01,0.1))
#            ),
#    EstimationTask(metrics=c("F","prec","rec"),
#                   evaluator.pars=list(posClass="malignant"),
#                   method=CV(nReps=3,nFolds=10,strat=TRUE)))

## ----eval=FALSE----------------------------------------------------------
#  data(Servo,package='mlbench')
#  library(nnet)
#  nnExp <- performanceEstimation(
#    PredTask(Class ~ .,Servo),
#    workflowVariants(learner='nnet',
#                     learner.pars=list(trace=F,linout=T,
#                         size=c(3,5),decay=c(0.01,0.1))
#            ),
#    EstimationTask(metrics="mse",method=Bootstrap(nReps=100)))

## ----eval=FALSE----------------------------------------------------------
#  data(LetterRecognition,package='mlbench')
#  ltrExp <- performanceEstimation(
#      PredTask(lettr ~ .,LetterRecognition),
#      workflowVariants(learner='rpartXse',
#                       learner.pars=list(se=c(0,1)),
#                       predictor.pars=list(type='class')
#                       ),
#      EstimationTask(metrics="err",method=Holdout(nReps=3,hldSz=0.3)))

## ----eval=FALSE----------------------------------------------------------
#  data(iris)
#  library(e1071)
#  irisExp <- performanceEstimation(
#      PredTask(Species ~ .,iris),
#      workflowVariants(learner='svm',
#                       learner.pars=list(cost=c(1,10))
#                       ),
#      EstimationTask(metrics="acc",method=LOOCV()))

## ----eval=FALSE, size="small"--------------------------------------------
#  library(quantmod)
#  library(randomForest)
#  GSPC <- getSymbols('^GSPC',from='2008-01-01',to='2012-12-31', auto.assign=FALSE)
#  data.model <- specifyModel(
#    Next(100*Delt(Ad(GSPC))) ~ Delt(Ad(GSPC),k=1:10))
#  data <- modelData(data.model)
#  colnames(data)[1] <- 'PercVarClose'
#  spExp <- performanceEstimation(
#    PredTask(PercVarClose ~ .,data,'SP500_2012'),
#    c(Workflow('standardWF',wfID="standRF",
#               learner='randomForest',learner.pars=list(ntree=500)),
#      Workflow('timeseriesWF',wfID="slideRF",
#               learner='randomForest',
#               learner.pars=list(ntree=500,relearn.step=30))
#     ),
#    EstimationTask(metrics="theil",
#                   method=MonteCarlo(nReps=10,szTrain=0.5,szTest=0.25)))

## ----eval=FALSE----------------------------------------------------------
#  library(e1071)
#  data(PimaIndiansDiabetes,package='mlbench')
#  data(iris)
#  data(Glass,package="mlbench")
#  res <- performanceEstimation(
#      c(PredTask(diabetes ~ .,PimaIndiansDiabetes,"Pima"),
#        PredTask(Type ~ ., Glass),
#        PredTask(Species ~ .,iris)),
#      workflowVariants(learner="svm",
#                       learner.pars=list(cost=1:5,gamma=c(0.1,0.01,0.001))),
#      EstimationTask(metrics="err",method=CV()))

## ----echo=FALSE----------------------------------------------------------
load("pima.Rdata")

## ----message=FALSE, warning=FALSE----------------------------------------
pres <- pairedComparisons(res)

## ------------------------------------------------------------------------
pres$err$F.test

## ----size="tiny"---------------------------------------------------------
pres$err$Nemenyi.test

## ----exCDnemenyi, fig.cap="The CD diagram of the Nemenyi post-hoc test.", fig.height=6, fig.width=10----
CDdiagram.Nemenyi(pres)

## ----eval=FALSE----------------------------------------------------------
#  library(performanceEstimation)
#  library(e1071)
#  data(Satellite,package="mlbench")
#  pres <- performanceEstimation(
#      PredTask(classes ~ .,Satellite),
#      Workflow(learner="svm"),
#      EstimationTask("err",CV()),
#      cluster=TRUE
#      )

## ----eval=FALSE, size="small"--------------------------------------------
#  library(parallel)
#  myclust <- makeCluster(c("localhost","192.168.2.10","192.168.2.13"),"SOCK")
#  library(performanceEstimation)
#  library(e1071)
#  data(Satellite,package="mlbench")
#  pres <- performanceEstimation(
#      PredTask(classes ~ .,Satellite),
#      Workflow(learner="svm"),
#      EstimationTask("err",CV()),
#      cluster=myclust
#      )
#  stopCluster(myclust)

## ----eval=FALSE, size="small"--------------------------------------------
#  library(performanceEstimation)
#  library(e1071)
#  library(randomForest)
#  
#  data(algae,package="DMwR")
#  DSs <- sapply(names(algae)[12:18],
#           function(x,names.attrs) {
#             f <- as.formula(paste(x,"~ ."))
#             PredTask(f,algae[,c(names.attrs,x)],x,copy=TRUE)
#           },
#           names(algae)[1:11])
#  
#  WFs <- list()
#  WFs$svm <- list(learner.pars=list(cost=c(10,150,300),
#                                    gamma=c(0.01,0.001),
#                                    epsilon=c(0.1,0.05)),
#                  pre="centralImp",post="na2central")
#  WFs$randomForest <- list(learner.pars=list(mtry=c(5,7),
#                                             ntree=c(500,750,1500)),
#                           pre="centralImp")
#  
#  for(d in seq_along(DSs)) {
#    for(w in names(WFs)) {
#      resObj <- paste(names(DSs)[d],w,'Res',sep='')
#      assign(resObj,
#             performanceEstimation(
#                    DSs[d],
#                    c(
#                      do.call('workflowVariants',
#                              c(list(learner=w),WFs[[w]]))
#                      ),
#                     EstimationTask(metrics=c('mse','mae'),method=CV(nReps=3)),
#                     cluster=TRUE)
#             )
#  
#      save(list=resObj,file=paste(names(DSs)[d],w,'Rdata',sep='.'))
#    }
#  }

## ----eval=FALSE, size="footnotesize"-------------------------------------
#  nD <- paste('a',1:7,sep='')
#  nL <- c('svm','randomForest')
#  res <- NULL
#  for(d in nD) {
#    resD <- NULL
#    for(l in nL) {
#      load(paste(d,l,'Rdata',sep='.'))
#      x <- get(paste(d,l,'Res',sep=''))
#      resD <- if (is.null(resD)) x else mergeEstimationRes(resD,x,by='workflows')
#    }
#    res <- if (is.null(res)) resD else mergeEstimationRes(res,resD,by='tasks')
#  }
#  save(res,file='allResultsAlgae.Rdata')

## ----echo=FALSE----------------------------------------------------------
load("allResultsAlgae.Rdata")

## ----size="small"--------------------------------------------------------
res
taskNames(res)
workflowNames(res)
metricNames(res)

## ----size="scriptsize"---------------------------------------------------
topPerformers(res)

## ----size="scriptsize"---------------------------------------------------
rankWorkflows(res)

## ----figMAEa1, fig.cap="The MAE results for the task A1.", fig.height=6, fig.width=6----
plot(subset(res, tasks='a1', metrics='mae'))

## ----figMSEa1, fig.cap="Illustration of the use of regular expressions in sub-setting the results objects.", fig.height=6, fig.width=6----
plot(subset(res, tasks='a1', workflows='4$'))

## ----size="small"--------------------------------------------------------
summary(subset(res, tasks='a1', workflows=glob2rx('*svm*'),metrics='mse'))

## ------------------------------------------------------------------------
getScores(res, 'svm.v6','a3')

## ------------------------------------------------------------------------
estimationSummary(res,'svm.v3', 'a7')

## ----size="small"--------------------------------------------------------
metricsSummary(subset(res, workflows=glob2rx('*svm*'), tasks='a1'),
               summary='median')

