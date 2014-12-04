\name{bootEstimates}
\alias{bootEstimates}

\title{
  Performance estimation using (e0 or .632) bootstrap 
}
\description{
  This function obtains boostrap  estimates of
  performance  metrics for a given predictive task and method to solve
  it (i.e. a  workflow). The function is general in the sense that the
  workflow function that the user provides as the solution to the task,
  can implement or call whatever modeling technique the user wants.


The function implements both e0 boostrap estimates as well as .632
boostrap. The selection of the type of boostrap is done through the 
\code{estTask} argument (check the help page of
\code{\linkS4class{Bootstrap}}). 

Please note that most of the times you will not call this function
directly, though there is nothing wrong in doing it, but instead you
will use the function \code{performanceEstimation}, that allows you to
carry out performance estimation for multiple workflows on multiple tasks,
using some estimation method like for instance boostrap. Still, when you
simply want to have the boostrap estimate for one workflow on one task,
you may use this function directly. 
}
\usage{
bootEstimates(wf,task,estTask,cluster)
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
  \item{estTask}{
    an object of the class \code{\linkS4class{EstimationTask}} indicating the metrics to
    be estimated and the boostrap settings to use.
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
  The idea of this function is to carry out a bootstrap
  experiment with the goal of obtaining reliable estimates of the
  predictive performance of a certain modeling approach (denoted here as
  a \emph{workflow}) on a given predictive task. Two 
  types of bootstrap estimates are implemented: i) e0 bootstrap and ii)
  .632 bootstrap.  Bootstrap
  estimates are obtained by averaging over a set of k scores each
  obtained in the following way: i) draw a random sample with replacement
  with the same size as the original data set; ii) obtain a model with
  this sample; iii) test it and obtain the estimates for this run on the
  observations of the original data set that were not used in the sample
  obtained in step i). This process is repeated k times and the average
  scores are the bootstrap estimates. The main difference between e0 and
  .632 bootstrap is the fact that the latter tries to integrate the e0
  estimate with the resubstitution estimate, i.e. when the model is
  learned and tested on the full available data sample.

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
  \code{\linkS4class{Bootstrap}},
  \code{\linkS4class{Workflow}},
  \code{\link{standardWF}},
  \code{\linkS4class{PredTask}},
  \code{\linkS4class{EstimationTask}},
  \code{\link{performanceEstimation}},
  \code{\link{hldEstimates}},
  \code{\link{loocvEstimates}},  
  \code{\link{cvEstimates}},
  \code{\link{mcEstimates}},
  \code{\linkS4class{EstimationResults}}
}
\examples{
\dontrun{

## Estimating the MSE of a SVM variant on the 
##  swiss data, using 50 repetitions of .632 bootstrap
library(e1071)
data(swiss)

## running the estimation experiment
res <- bootEstimates(
  Workflow(wfID="svmC10G01",
           learner="svm",learner.pars=list(cost=10,gamma=0.1)
          ),
  PredTask(Infant.Mortality ~ .,swiss),
  EstimationTask("mse",method=Bootstrap(type=".632",nReps=50))
  )

## Check a summary of the results
summary(res)

}
}
\keyword{models}

