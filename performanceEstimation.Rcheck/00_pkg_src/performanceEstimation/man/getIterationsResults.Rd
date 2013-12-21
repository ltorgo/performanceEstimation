\name{getIterationsResults}
\alias{getIterationsResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtaining the metric scores on the different iterations for a workflow /
task combination
}
\description{
  With this function we can obtain the different metric scores obtained
  by a given workflow on a given task, in the different iterations of a
  performance estimation experiment.
}
\usage{
getIterationsResults(results, wf, task)
}
\arguments{
  \item{results}{
A \code{\linkS4class{ComparisonResults}} object
}
  \item{wf}{
A string with the ID of a workflow
}
  \item{task}{
A string with the ID of a predictive task
}
}
\value{
  A matrix with as many rows as there are iterations and as many columns
  as there are metrics being estimated in the experiment
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
\code{\link{taskNames}},\code{\link{workflowNames}},\code{\link{metricNames}},\code{\link{estimationSummary}}
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
  c(workflowVariants("standardWF",learner="svm",
                     learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
    workflowVariants("standardWF",learner="rpartXse",
                     learner.pars=list(se=c(0,0.5,1)))
  ),
  CvSettings(nReps=1,nFolds=10,seed=1234)
  )

## Get the iterations scores of svm.v2 on swiss
getIterationsResults(res,"svm.v2","swiss")

}
}
\keyword{models}

