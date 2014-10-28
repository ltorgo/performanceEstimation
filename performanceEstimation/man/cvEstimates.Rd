\name{cvEstimates}
\alias{cvEstimates}

\title{
Performance estimation using cross validation
}

\description{
  
  This function obtains cross validation estimates of performance
  metrics for a given predictive task and method to solve it (i.e. a
  workflow). The function is general in the sense that the workflow
  function that the user provides as the solution to the task can
  implement or call whatever modeling technique the user wants.

  The function implements k-fold cross validation (CV)
estimation. Different settings concerning this methodology are available
through the argument \code{estTask} (check the help page of
\code{\linkS4class{EstimationTask}} and \code{\linkS4class{CV}}).

Please note that most of the times you will not call this function
directly (though there is nothing wrong in doing it) but instead you
will use the function \code{\link{performanceEstimation}}, that allows you to
carry out performance estimation for multiple workflows on multiple tasks,
using the estimation method you want (e.g. cross validation). Still, when you
simply want to have the CV estimate of one workflow on one task,
you may prefer to use this function directly. 
}

\usage{
cvEstimates(wf,task,estTask)
}

\arguments{
  \item{wf}{
 an object of the class \code{\linkS4class{Workflow}} representing the
  modeling approach to be evaluated on a certain task.
}
  \item{task}{
 an object of the class \code{\linkS4class{PredTask}} defining the
  prediction task for which we want estimates.
}
  \item{estTask}{
an object of the class \code{\linkS4class{EstimationTask}} indicating the metrics to
  be estimated and the cross validation settings to use.
}
}

\details{
  The idea of this function is to carry out a cross validation
  experiment with the goal of obtaining reliable estimates of the
  predictive performance of a certain approach to a predictive
  task. This approach (denoted here as a \emph{workflow}) will be evaluated on
  the given predictive task using some user-selected  metrics,
  and this function will 
  provide k-fold cross validation estimates of the true values of these
  evaluation metrics.  k-Fold cross validation
  estimates are obtained by randomly partitioning the given data set into k
  equal size sub-sets. Then a learn+test process is repeated k times. At
  each iteration one of the k partitions is left aside as test set and
  the model is obtained with a training set formed by the remaining k-1
  partitions. The process is repeated leaving each time one of the k
  partitions aside as test set. In the end the average of the k scores
  obtained on each iteration is the cross validation estimate.

  It is the user responsibility to decide which statistics are to be
  evaluated on each iteration and how they are calculated. This is done
  by creating a function that the user knows it will be called by this
  cross validation routine at each iteration of the cross validation
  process. This user-defined workflow function must assume that it will
  receive in the first 3 arguments a formula, a training set and a
  testing set, respectively. It should also assume that it may receive
  any other set of parameters that should be passed towards the
  modelling algorithm. The result of this user-defined function should
  be an object of class \code{\linkS4class{WFoutput}}. See the Examples
  section below for an example of these functions. Further examples are
  given in the package vignette. 

  To facilitate the task of creating these user-defined workflow
  functions we provide the \code{\link{standardWF}} function that can be used
  as a kind of skeleton of a general purpose workflow for predictive
  tasks where the user simply indicates a few small details (e.g. the
  learning algorithm to use). Check the help page of this function for
  more details and examples. In the Examples section of the current
  function you will also find some illustrations of the use of this
  function.
  
}

\value{
  The result of the function is an object of class \code{\linkS4class{EstimationResults}}.
}

\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
\code{\link{performanceEstimation}},
\code{\linkS4class{EstimationResults}},\code{\linkS4class{BootSettings}}, \code{\link{mcEstimates}}, \code{\link{hldEstimates}}, \code{\link{loocvEstimates}}, \code{\link{bootEstimates}}
}

\examples{
\dontrun{

## Estimating the mean squared error  of svm on the swiss data,
## using two repetitions of 10-fold CV
library(e1071)
data(swiss)

## Now the evaluation
eval.res <- cvEstimates(
             Workflow("standardWF", wfID="mySVMtrial",
                      learner="svm", learner.pars=list(cost=10,gamma=0.1)
                     ),
             PredTask(Infant.Mortality ~ ., swiss),
             CvSettings(2,10,1234))

## Check a summary of the results
summary(eval.res)


## An example with a user-defined workflow function implementing a
## simple approach using linear regression models but also containing
## some data-preprocessing and well as results post-processing.
myLM <- function(form,train,test,k=10,.outPreds=T,.outModel=F) {
    require(DMwR)
    ## fill-in NAs on both the train and test sets
    ntr <- knnImputation(train,k)
    nts <- knnImputation(test,k,distData=train)
    ## obtain a linear regression model and simplify it
    md <- lm(form,ntr)
    md <- step(md)
    ## get the model predictions
    p <- predict(md,nts)
    ## post-process the predictions (this is an example assuming the target
    ## variable is always positive so we change negative predictions into 0)
    p <- ifelse(p < 0,0,p)
    ## now get the evaluation metrics
    sts <- regressionMetrics(responseValues(form,test),p,stats=c("mse","mae"))
    ## creating the return object
    res <- WFoutput(sts)
    if (.outPreds)
      workflowPredictions(res) <- list(responseValues(form,test),p,rownames(test))
    if (.outModel)
      workflowInformation(res) <- list(model=m) 
    res
}

## Now for the CV estimation 
data(algae,package="DMwR")
eval.res2 <- cvEstimates(
             Workflow("myLM",k=5),
             PredTask(a1 ~ ., algae[,1:12],"alga1"),
             CvSettings(1,10,1234))

## Check a summary of the results
summary(eval.res2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

