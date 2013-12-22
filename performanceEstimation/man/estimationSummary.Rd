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
estimationSummary(results,wf,task)
}

\arguments{
  \item{results}{
    This is a \code{\linkS4class{ComparisonResults}} object (type "class?ComparisonResults" for details)
    that contains the results of a performance estimation  experiment obtained
    through the \code{performanceEstimation()} function. 
}
\item{wf}{
  This is the string that identifies the workflow.
}
\item{task}{
  The string that identifies the task for which we want the summary of estimates.
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
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\seealso{
\code{\link{getIterationsResults}}, \code{\link{performanceEstimation}}
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

## Get the statistics of a specific learner
estimationSummary(res,'svm.v1','swiss')

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
