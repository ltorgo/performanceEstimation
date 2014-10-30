library(devtools)
load_all("~/Software/R/MyPackages/performanceEstimation/performanceEstimation")

## small illustrative example with iris
library(e1071)
data(iris)
res <- performanceEstimation(
         PredTask(Species ~ .,iris),
         workflowVariants('standardWF',
                          learner='svm',
                          learner.pars=list(cost=c(1,5,10),gamma=c(0.1,0.001))
                 ),
         CvSettings(nReps=1,nFolds=10))

save(res,file="ex1Iris.Rdata")


summary(res)

topPerformers(res)

plot(res)

getWorkflow('svm.v1',res)

data(iris)
PredTask(Species ~ .,iris,'irisClassificationTask')
PredTask(Species ~ Petal.Length + Sepal.Length,iris,'ShortIrisTask')


RLensemble <- function(f,tr,ts,weightRT=0.5,step=FALSE,...) {
  require(DMwR,quietly=F)
  noNAsTR <- knnImputation(tr)
  noNAsTS <- knnImputation(ts)
  r <- rpartXse(f,tr,...)
  l <- lm(f,noNAsTR)
  if (step) l <- step(l,trace=0)
  pr <- predict(r,ts)
  pl <- predict(l,noNAsTS)
  ps <- weightRT*pr+(1-weightRT)*pl
  WFoutput(c(correlation=cor(responseValues(f,ts),ps)))
}

data(algae,package='DMwR')
expRes <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],'alga1'),
  workflowVariants('RLensemble',
                  se=c(0,1),step=c(T,F),weightRT=c(0.4,0.5,0.6)),
  BootSettings(nReps=100,type='e0'))



data(Boston,package='MASS')
library(DMwR)
library(e1071)
library(randomForest)
bostonRes <- performanceEstimation(
  PredTask(medv ~ .,Boston),
  workflowVariants('standardWF',learner=c('rpartXse','svm','randomForest')),
  CvSettings(nReps=1,nFolds=10)
  )

bostonRes <- performanceEstimation(
  PredTask(medv ~ .,Boston),
  c(workflowVariants('standardWF',learner='rpartXse',learner.pars=list(se=c(0,1))),
    workflowVariants('standardWF',learner='svm',learner.pars=list(cost=c(1,5),gamma=c(0.01,0.1))),
    workflowVariants('standardWF',learner='randomForest',learner.pars=list(ntree=c(500,1000)))),
  CvSettings(nReps=1,nFolds=10)
  )

library(DMwR)
data(BreastCancer,package='mlbench')
library(e1071)
bc <- knnImputation(BreastCancer[,-1])
bcExp <- performanceEstimation(
  PredTask(Class ~ .,bc,'BreastCancer'),
  workflowVariants('standardWF',
           learner='svm',
           learner.pars=list(cost=c(1,5),gamma=c(0.01,0.1)),
           evaluator.pars=list(stats=c("F","prec","rec"),posClass="malignant")
          ),
  CvSettings(nReps=3,nFolds=10,strat=TRUE))


data(Servo,package='mlbench')
library(nnet)
nnExp <- performanceEstimation(
  PredTask(Class ~ .,Servo),
  workflowVariants('standardWF',
           learner='nnet',
           learner.pars=list(trace=F,linout=T,size=c(3,5),decay=c(0.01,0.1))
          ),
  BootSettings(nReps=100))

data(LetterRecognition,package='mlbench')
ltrExp <- performanceEstimation(
  PredTask(lettr ~ .,LetterRecognition),
  workflowVariants('standardWF',
           learner='rpartXse',
           learner.pars=list(se=c(0,1)),
           predictor.pars=list(type='class')
           ),
  HldSettings(nReps=3,hldSz=0.3))


data(iris)
library(e1071)
irisExp <- performanceEstimation(
  PredTask(Species ~ .,iris),
  workflowVariants('standardWF',
           learner='svm',
           learner.pars=list(cost=c(1,10))
           ),
  LoocvSettings())


library(quantmod)
library(randomForest)
getSymbols('^GSPC',from='2008-01-01',to='2012-12-31')
data.model <- specifyModel(
  Next(100*Delt(Ad(GSPC))) ~ Delt(Ad(GSPC),k=1:10)+Delt(Vo(GSPC),k=1:3))
data <- modelData(data.model)
colnames(data)[1] <- 'PercVarClose'
spExp <- performanceEstimation(
  PredTask(PercVarClose ~ .,data,'SP500_2012'),
  c(standRF=Workflow('standardWF',wfID="standRF",
                    pars=list(learner='randomForest',
                              learner.pars=list(ntree=500))
                   ),
    slideRF=Workflow('timeseriesWF',wfID="slideRF",
                    pars=list(learner='randomForest',
                              learner.pars=list(ntree=500,relearn.step=5))
                   )
    ),
  McSettings(10,0.5,0.25))

