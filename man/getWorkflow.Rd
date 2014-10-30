\name{getWorkflow}
\alias{getWorkflow}

\title{
Obtain the workflow object corresponding to an ID
}
\description{
  This function can be used to obtain the \code{\linkS4class{Workflow}}
  object corresponding to an ID used in a performance estimation
  experiment. This allows you for instance to check the full details of
  the workflow corresponding to that ID (e.g. the function implementing
  the workflow, the parameters and their values, etc.)
}
\usage{
getWorkflow(var, obj)
}
\arguments{
  \item{var}{
    The string with the workflow ID
  }
  \item{obj}{
    A \code{\linkS4class{ComparisonResults}} object with the data from a
    performance estimation experiment
  }
}
\value{
  A \code{\linkS4class{Workflow}} object
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{Workflow}},
  \code{\link{runWorkflow}},  
  \code{\link{performanceEstimation}}
}
\examples{
\dontrun{
## Estimating MSE for 3 variants of both
## regression trees and SVMs, on  two data sets, using one repetition
## of 10-fold CV
library(e1071)
library(DMwR)
data(swiss)
data(mtcars)

## running the estimation experiment
res <- performanceEstimation(
  c(PredTask(Infant.Mortality ~ .,swiss),PredTask(mpg ~ ., mtcars)),
  c(workflowVariants(learner="svm",
                     learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
    workflowVariants(learner="rpartXse",
                     learner.pars=list(se=c(0,0.5,1)))
  ),
  EstimationTask("mse",method=CV(nReps=2,nFolds=5))
  )

## Get the workflow corresponding to the ID svm.v2
getWorkflow("svm.v2",res)
}
}

\keyword{ models }

