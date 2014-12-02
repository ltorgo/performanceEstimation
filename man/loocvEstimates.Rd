\name{loocvEstimates}
\alias{loocvEstimates}

\title{
Performance estimation using  Leave One Out Cross Validation 
}
\description{
  This function obtains leave one out cross validation estimates of
  performance  metrics for a given predictive task and method to solve
  it (i.e. a  workflow). The function is general in the sense that the
  workflow function that the user provides as the solution to the task,
  can implement or call whatever modeling technique the user wants.
  

The function implements leave one out cross validation
estimation. Different settings concering this methodology are 
available through the argument \code{estTask} (check the help page of
\code{\linkS4class{LOOCV}}).

Please note that most of the times you will not call this function
directly, though there is nothing wrong in doing it, but instead you
will use the function \code{\link{performanceEstimation}}, that allows you to
carry out performance estimation of multiple workflows on multiple tasks,
using some estimation method like for instance cross validation. Still, when you
simply want to have the leave one out cross validation estimate of one
workflow on one task, you may use this function directly.
}

\usage{
loocvEstimates(wf,task,estTask,verbose=FALSE)
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
an object of the class \code{\linkS4class{EstimationTask}} indicating the metrics to
  be estimated and the leave one out cross validation settings to use.
}
  \item{verbose}{
    A boolean value controlling the level of output of the function
    execution, defaulting to \code{FALSE}

}
}

\details{
  The idea of this function is to carry out a leave one out cross
  validation  experiment with the goal of obtaining reliable estimates
  of the predictive performance of a certain approach to a predictive
  task. This approach (denoted here as a \emph{workflow}) will be evaluated on
  the given predictive task using some user-selected  metrics,
  and this function will provide leave one out cross validation
  estimates of the true value of these
  evaluation metrics.  Leave one out cross validation estimates are
  obtained as the average of \emph{N} iterations, where \emph{N} is the
  size of the given data sample. On each of these iterations one of the
  cases in the data sample is left out as \emph{test set} and the
  worflow is applied to the remaining \emph{N-1} cases. The process is
  repeated for all cases, i.e. \emph{N} times. This estimation is
  similar to k-fold cross validation where k equals to \emph{N}. The
  resulting estimates are obtained by averaging over the \emph{N}
  iteration scores. 

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
  \code{\linkS4class{LOOCV}},
  \code{\linkS4class{Workflow}},
  \code{\link{standardWF}},
  \code{\linkS4class{PredTask}},
  \code{\linkS4class{EstimationTask}},
  \code{\link{performanceEstimation}},
  \code{\link{hldEstimates}},
  \code{\link{bootEstimates}},  
  \code{\link{cvEstimates}},
  \code{\link{mcEstimates}},
  \code{\linkS4class{EstimationResults}}
}

\examples{
\dontrun{

## Estimating the error rate of an SVM on the iris data set using
## leave one out cross validation
library(e1071)
data(iris)

## Now the evaluation
eval.res <- loocvEstimates(
             Workflow(wfID="svmTrial",
                      learner="svm",learner.pars=list(cost=10,gamma=0.1)
                     ),
             PredTask(Species ~ ., iris),
             EstimationTask("err",method=LOOCV()))

## Check a summary of the results
summary(eval.res)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
