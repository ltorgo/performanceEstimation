\name{hldEstimates}
\alias{hldEstimates}

\title{
Performance estimation using holdout and random resampling
}
\description{
  This function obtains hold-out and random sub-sampling estimates of
  performance  metrics for a given predictive task and method to solve
  it (i.e. a  workflow). The function is general in the sense that the
  workflow function that the user provides as the solution to the task,
  can implement or call whatever modeling technique the user wants.
  

The function implements hold-out and random sub-sampling (repeated
hold-out) estimation. Different settings concerning this methodology are
available through the argument \code{estTask} (check the help page of
\code{\linkS4class{Holdout}}).

Please note that most of the times you will not call this function
directly (though there is nothing wrong in doing it) but instead you
will use the function \code{\link{performanceEstimation}}, that allows you to
carry out performance estimation for multiple workflows on multiple tasks,
using some estimation method you want (e.g. hold-out). Still, when you
simply want to have the hold-out estimate of one workflow on one task,
you may prefer to use this function directly.
}

\usage{
hldEstimates(wf,task,estTask,cluster)
}

\arguments{
  \item{wf}{
    an object of the class \code{\linkS4class{Workflow}} representing the
    modeling approach to be evaluated on a certain task.
  }
  \item{task}{
    an object of the class \code{\linkS4class{PredTask}} representing the
    prediction task to be used in the evaluation.
  }
  \item{estTask}{
    an object of the class \code{\linkS4class{EstimationTask}} representing the
    hold-out settings to use.
  }
  \item{cluster}{
    an optional parameter that can either be \code{TRUE} or a
    \code{\link{cluster}}. In case of \code{TRUE} the function will run in
    parallel and will internally setup the parallel back-end (defaulting
    to using half of the cores in your local machine). You may also setup
    outside your parallel back-end (c.f. \code{\link{makeCluster}}) and
    then pass the resulting \code{cluster} object to this function using
    this parameter. In case no value is provided for this parameter the
    function will run sequentially.
  }
}

\details{
  The idea of this function is to carry out a hold-out
  experiment with the goal of obtaining reliable estimates of the
  predictive performance of a certain approach to a predictive
  task. This approach (denoted here as a \emph{workflow}) will be evaluated on
  the given predictive task using some user-selected  metrics,
  and this function will 
  provide hold-out or random sub-sampling estimates of the true value of these
  evaluation metrics.  Hold-out estimates are obtained by randomly
  partition the given data set into train and test sets. The training
  set is used to obtain a model for the predictive task, which is then
  tested by making predictions for the test set. This random split of
  the given data can be repeated several times leading to what is
  usually known as random sub-sampling estimates. In the end the average of
  the scores over the several repetitions (if using \emph{pure}
  hold-out this is only one) are the hold-out estimates of the selected
  metrics.   

  Parallel execution of the estimation experiment is only recommended
  for minimally large data sets otherwise you may actually increase the
  computation time due to communication costs between the processes.

}

\value{
  The result of the function is an object of class \code{\linkS4class{EstimationResults}}.
}

\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\linkS4class{Holdout}},
  \code{\linkS4class{Workflow}},
  \code{\link{standardWF}},
  \code{\linkS4class{PredTask}},
  \code{\linkS4class{EstimationTask}},
  \code{\link{performanceEstimation}},
  \code{\link{cvEstimates}},
  \code{\link{bootEstimates}},  
  \code{\link{loocvEstimates}},
  \code{\link{mcEstimates}},
  \code{\linkS4class{EstimationResults}}

}

\examples{
\dontrun{

## Estimating the mean absolute error and the normalized mean squared
## error of rpart on the swiss data, using 70\%-30\% hold-out
library(e1071)
data(swiss)

## Now the evaluation
eval.res <- hldEstimates(
             Workflow(wf="standardWF",wfID="svmApproach",
                      learner="svm",learner.pars=list(cost=10,gamma=0.1)
                     ),
             PredTask(Infant.Mortality ~ ., swiss),
             EstimationTask(metrics=c("mae","nmse"),
                            method=Holdout(nReps=5,hldSz=0.3))
                        )

## Check a summary of the results
summary(eval.res)

## An example with a user-defined workflow function implementing a
## simple approach using linear regression models but also containing
## some data-preprocessing and well as results post-processing.
myLM <- function(form,train,test,k=10,.outModel=FALSE) {
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
    ## now get the final return object
    res <- list(trues=responseValues(form,nts), preds=p)
    if (.outModel) res <- c(res,list(model=m))
    res
}

## Now for the Holdout estimation 
data(algae,package="DMwR")
eval.res2 <- hldEstimates(
             Workflow(wf="myLM",k=5),
             PredTask(a1 ~ ., algae[,1:12],"alga1"),
             EstimationTask("mse",method=Holdout(nReps=5)))

## Check a summary of the results
summary(eval.res2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

