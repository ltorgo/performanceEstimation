\name{estimationSummary}
\alias{estimationSummary}

\title{
  Obtain a set of descriptive statistics of the scores of a workflow on
  a task
}
\description{
  This function provides a set of descriptive statistics for each
  evaluation metric that is estimated on a performance estimation
  comparison. These statistics are obtained for a particular workflow,
  and for one of the prediction tasks involved in the experiment.
}
\usage{
estimationSummary(results,workflow,task)
}

\arguments{
  \item{results}{
    This is a \code{\linkS4class{ComparisonResults}} object (type "class?ComparisonResults" for details)
    that contains the results of a performance estimation  experiment obtained
    through the \code{performanceEstimation()} function. 
}
\item{workflow}{
A string with the ID of a workflow (it can also be an integer).
}
\item{task}{
A string with the ID of a task (it can also be an integer).
}
}
\value{
  The function returns a matrix with the rows representing summary
  statistics of the scores obtained by the model on the different
  iterations, and the columns representing the evaluation statistics
  estimated in the experiment.
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\seealso{
\code{\link{getScores}}, \code{\link{performanceEstimation}}
}
\examples{
\dontrun{
## Estimating MSE for 3 variants of both
## regression trees and SVMs, on  two data sets, using one repetition
## of 10-fold CV
library(e1071)
data(swiss)

## running the estimation experiment
res <- performanceEstimation(
  PredTask(Infant.Mortality ~ .,swiss),
  workflowVariants(learner="svm",
                   learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
  EstimationTask("mse",method=CV(nReps=2,nFolds=5))
  )

## Get the summary of the estimations of svm.v2 on swiss
estimationSummary(res,"svm.v2","swiss.Infant.Mortality")

}
}
\keyword{models}

