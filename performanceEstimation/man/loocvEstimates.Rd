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
available through the argument \code{sets} (check the help page of
\code{\linkS4class{LoocvSettings}}).

Please note that most of the times you will not call this function
directly, though there is nothing wrong in doing it, but instead you
will use the function \code{\link{performanceEstimation}}, that allows you to
carry out performance estimation of multiple workflows on multiple tasks,
using some estimation method like for instance cross validation. Still, when you
simply want to have the leave one out cross validation estimate of one
workflow on one task, you may use this function directly.
}

\usage{
loocvEstimates(wf,task,sets,verbose=FALSE)
}

\arguments{
  \item{wf}{
 an object of the class \code{Workflow} representing the
  modeling approach to be evaluated on a certain task.
}
  \item{task}{
an object of the class \code{PredTask} representing the
  prediction task to be used in the evaluation.
}
  \item{sets}{
an object of the class \code{LoocvSettings} representing the
  cross validation settings to use.
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

  It is the user responsibility to decide which statistics are to be
  evaluated on each iteration and how they are calculated. This is done
  by creating a function that the user must assume it will be called by this
  leave one out CV routine at each iteration of the
  process. This user-defined workflow function must assume that it will
  receive in the first 3 arguments a formula, a training set and a
  testing set, respectively. It should also assume that it may receive
  any other set of parameters that should be passed towards the
  modeling algorithm. The result of this user-defined function should
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
\code{\linkS4class{EstimationResults}},\code{\linkS4class{BootSettings}}, \code{\link{mcEstimates}}, \code{\link{cvEstimates}}, \code{\link{hldEstimates}}, \code{\link{bootEstimates}}
}

\examples{
\dontrun{

## Estimating the error rate of an SVM on the iris data set using
## leave one out cross validation
library(e1071)
data(iris)

## Now the evaluation
eval.res <- loocvEstimates(
             Workflow("standardWF",wfID="svmTrial",
                      learner="svm",learner.pars=list(cost=10,gamma=0.1)
                     ),
             PredTask(Species ~ ., iris),
             LoocvSettings(seed=1234))

## Check a summary of the results
summary(eval.res)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
