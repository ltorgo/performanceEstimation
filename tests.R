## load the current sources
## scs <- c("classDefs.R","methods.R","experiments.R","workflows.R","evaluationMetrics.R","resultsAnalysis.R","resultsManipulation.R")
## for(s in scs) source(paste0("R/",s))

###############################
## Load the new package version (installed on a different folder)
library(performanceEstimation,lib.loc="~/R/dev.libs/")

library(e1071)
library(rpart)
library(randomForest)
library(ggplot2)
data(iris)
data(algae,package="DMwR")
data(Boston,package="MASS")
data(BreastCancer,package="mlbench")
data(LetterRecognition,package="mlbench")
data(Satellite,package="mlbench")
data(Servo,package="mlbench")

## Creating the tasks
ti <- PredTask(Species ~ .,iris)
ta1 <- PredTask(a1 ~ .,algae[,1:12],"alga1")
ta2 <- PredTask(a2 ~ .,algae[,c(1:11,13)],"alga2")
tb <- PredTask(medv ~ .,Boston)
tl <- PredTask(lettr ~ .,LetterRecognition,"letter")
tsv <- PredTask(Class ~ .,Servo)
tst <- PredTask(classes ~ .,Satellite,"sat")
# 2-class iris
iris2 <- iris
iris2$Species <- rep("ok",nrow(iris))
iris2$Species[sample(1:150,20)] <- "fraud"
iris2$Species <- factor(iris2$Species)
ti2 <- PredTask(Species ~ .,iris2,"twoClassIris")

##############
## Some workflows
w.s <- Workflow(wfID="std.svm",learner="svm")
w.s1 <- Workflow(wfID="svm.c10",learner="svm",learner.pars=list(cost=10))
w.s2 <- Workflow(wfID="svm.soph",learner="svm",learner.pars=list(cost=5,gamma=.01),pre=c("centralImp","scale"),fullOutput=TRUE)

w.rt <- Workflow(wfID="rt",learner="rpart")

w.rf <- Workflow(wfID="rf",learner="randomForest")
w.rf1000 <- Workflow(wfID="rf",learner="randomForest",learner.pars=list(ntree=1000))

# user-defined wf
myWF <- function(form,train,test,wL=0.5,...) {
    require(rpart,quietly=TRUE)
    ml <- lm(form,train)
    mr <- rpart(form,train)
    pl <- predict(ml,test)
    pr <- predict(mr,test)
    ps <- wL*pl+(1-wL)*pr
    WFoutput(rownames(test),responseValues(form,test),ps)
}
wu <- Workflow(wf="myWF",wL=0.6)

##############
## Basic stuff
res1 <- performanceEstimation(ti,w.rt,EstimationTask("acc"))
res1
summary(res1)
plot(res1)
rankWorkflows(res1)
topPerformers(res1)
topPerformer(res1,metricNames(res1)[1],taskNames(res1)[1])
workflowNames(res1)
getScores(res1,workflowNames(res1)[1],taskNames(res1)[1])
getIterationInfo(res1,it=1)
getIterationPreds(res1,rep=1,fold=1)
estimationSummary(res1,workflowNames(res1)[1],taskNames(res1)[1])

################
## merging tests

## adding new workflows
res2 <- performanceEstimation(ti,w.s,EstimationTask("acc"))

res1.2 <- mergeEstimationRes(res1,res2,by="workflows")
summary(res1.2)
plot(res1.2)
metricNames(res1.2)
workflowNames(res1.2)
taskNames(res1.2)

## adding new tasks
res3 <- performanceEstimation(tst,c(w.rt,w.s),EstimationTask("acc"))

res.123 <- mergeEstimationRes(res1.2,res3,by="tasks")
summary(res.123)
plot(res.123)
metricNames(res.123)
workflowNames(res.123)
taskNames(res.123)

## adding new metrics
res4 <- performanceEstimation(c(ti,tst),c(w.rt,w.s),EstimationTask(metrics=c("totTime","macroF")))


res.1234 <- mergeEstimationRes(res.123,res4,by="metrics")
summary(res.1234)
plot(res.1234)
metricNames(res.1234)
workflowNames(res.1234)
taskNames(res.1234)
rankWorkflows(res.1234,maxs=c(TRUE,FALSE,TRUE))

##################
## some subsetting
summary(subset(res.1234,metrics="ac"))
summary(subset(res.1234,metrics="ac",tasks="i"))
summary(subset(res.1234,metrics="i",workflows="r"))


####################
## Workflow variants

res5 <- performanceEstimation(ti,
                              workflowVariants(learner=c("rpart","svm","randomForest")),
                              EstimationTask(metrics="err",method=Holdout(nReps=5)))
summary(res5)
plot(res5)

## variants on a user-defined workflow
res6 <- performanceEstimation(tb,
               workflowVariants(wf="myWF",wL=seq(0,1,by=0.1)),
               EstimationTask(metrics=c("mse","nmae"),method=Bootstrap(nReps=5)))
summary(res6)
plot(res6)

getWorkflow("myWF.v11",res6)
x <- runWorkflow(getWorkflow("myWF.v11",res6),medv ~ . , Boston, Boston)
ggplot(workflowPredictions(x),aes(x=true,y=predicted)) + geom_point() + geom_smooth(method='loess') + geom_abline(slope=1, intercept=0,color="red") + ggtitle("Performance of myWF.v11 on training set")

####################
## Time series

## pretending Boston is a time series task
res7 <- performanceEstimation(tb,
                 c(Workflow(wfID="std.svm",learner="svm"),workflowVariants(learner="svm",type=c("slide","grow"),relearn.step=c(1,10))),
                 EstimationTask(metrics=c("totTime","mse","theil"),method=MonteCarlo(nReps=5,szTrain=0.3,szTest=0.2)))

summary(res7)
getWorkflow("svm.v1",res7)
getWorkflow("svm.v2",res7)
getWorkflow("svm.v3",res7)


############################################
## Measuring Utility on Classification Tasks
bcm <- matrix(c(10,-10,-21,-30,50,-5,-10,-5,80),byrow=TRUE,ncol=3)

res8 <- performanceEstimation(ti,
             workflowVariants(learner="svm",learner.pars=list(gamma=seq(0.01,0.1,by=0.01))),
             EstimationTask(metrics=c("err","totU"),evaluator.pars=list(benMtrx=bcm)))
rankWorkflows(res8,maxs=c(FALSE,TRUE))

############################################
## Two-classes metrics, unbalanced and stratification
res9 <- performanceEstimation(ti2,w.rf1000,
                EstimationTask(metrics=c("prec","rec","F","lift"),
                               evaluator.pars=list(posClass="fraud"),
                               method=CV(strat=TRUE)))
summary(res9)


###################
## User data splits
res10 <- performanceEstimation(ti,w.rt,
              EstimationTask(metrics="err",
                  method=CV(dataSplits=list(sample(1:150,50),
                                            sample(1:150,50),
                                            sample(1:150,50)))
                             ))
summary(res10)


res11 <- performanceEstimation(ti,w.rt,
              EstimationTask(metrics="err",
                  method=Bootstrap(
                      dataSplits=list(list(test=sample(1:150,50),train=sample(1:150,50)),
                                      list(test=sample(1:150,50),train=sample(1:150,50)),
                                      list(test=sample(1:150,50),train=sample(1:150,50))
                             ))))
summary(res11)


##################
## Some pre-processing examples

##  A small example with standard pre-preprocessing: clean NAs and scale

idx <- sample(1:nrow(algae),150)
tr <- algae[idx,1:12]
ts <- algae[-idx,1:12]
summary(tr)
summary(ts)

preData <- standardPRE(a1 ~ ., tr, ts, steps=c("centralImp","scale"))
summary(preData$train)
summary(preData$test)

## Using in the context of an experiment
res <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm",pre=c("centralImp","scale")),
  EstimationTask(metrics="mse")
  )

summary(res)


## A user-defined pre-processing function
myScale <- function(f,tr,ts,avg,std,...) {
    tgtVar <- deparse(f[[2]])
    allPreds <- setdiff(colnames(tr),tgtVar)
    numPreds <- allPreds[sapply(allPreds,
                          function(p) is.numeric(tr[[p]]))]
    tr[,numPreds] <- scale(tr[,numPreds],center=avg,scale=std)
    ts[,numPreds] <- scale(ts[,numPreds],center=avg,scale=std)
    list(train=tr,test=ts)
}

## now using it with some random averages and stds for the 8 numeric
## predictors (just for illustration)
newData <- standardPRE(a1 ~ .,tr,ts,steps="myScale",
                       avg=rnorm(8),std=rnorm(8))
summary(newData$train)
summary(newData$test)


######################
## Some examples of post-processing

## This will issue several warnings because this implementation of SVMs
## will ignore test cases with NAs in some predictor. Our infra-structure
## issues a warning and fills in these with the prediction of an NA
res <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm"),
  EstimationTask(metrics="mse")
  )
summary(getIterationPreds(res,1,1,it=1))

## one way of overcoming this would be to post-process the NA
## predictions into a statistic of centrality
resN <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm",post="na2central"),
  EstimationTask(metrics="mse")
  )
summary(getIterationPreds(resN,1,1,it=1))

## because the SVM also predicts negative values which does not make
## sense in this application (the target are frequencies thus >= 0) we
## could also include some further post-processing to take care of
## negative predictions
resN <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm",post=c("na2central","onlyPos")),
  EstimationTask(metrics="mse")
  )
summary(getIterationPreds(resN,1,1,it=1))
