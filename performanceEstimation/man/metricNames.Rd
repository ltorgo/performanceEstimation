\name{metricNames}
\alias{metricNames}

\title{
The evaluation metrics estimated in an experiment
}
\description{
This function can be used to get a vector with the names of the
evaluation metrics that were used in a performance estimation experiment.
}
\usage{
metricNames(o)
}

\arguments{
  \item{o}{
An object of class \code{\linkS4class{ComparisonResults}}
}
}
\value{
  A vector of strings (the names of the metrics)
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{ComparisonResults}},
  \code{\link{performanceEstimation}},
  \code{\link{taskNames}},
  \code{\link{workflowNames}}

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
  EstimationTask("mse")
  )

## the names of the metrics that were estimated in the above experiment
metricNames(res)
}
}
\keyword{models}

