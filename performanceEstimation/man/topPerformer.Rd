\name{topPerformer}
\alias{topPerformer}

\title{
Obtain the workflow that best performed in terms of a metric on a task
}
\description{
  This function can be used to obtain the workflow (an object of class
  \code{\linkS4class{Workflow}}) that performed better in terms of a
  given metric on a certain task. 
}
\usage{
topPerformer(compRes,metric,task,max=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{compRes}{
    A \code{\linkS4class{ComparisonResults}} object with the results of your experimental comparison.
}
  \item{metric}{
    A string with the name of a metric estimated in the comparison
}
  \item{task}{
    A string with the name of a predictive task used in the comparison
}
  \item{max}{
    A boolean (defaulting to \code{FALSE}) indicating the meaning of
    best performance for the selected metric. If this is \code{FALSE} it
    means that the goal is to minimize this metric, otherwise it means
    that the metric is to be maximized.
}
}
\details{
  
  This is an utility function that can be used to obtain the workflow
  (an object of class \code{\linkS4class{Workflow}}) that achieved the
  best performance on a given predictive task in terms of a certain
  evaluation metric.  The notion of \emph{best performance} depends on
  the type of evaluation metric, thus the need for the \code{max}
  argument. Some evaluation statistics are to be maximized
  (e.g. accuracy), while others are to be minimized (e.g. mean squared
  error). For the former you should use \code{max=TRUE}, while the
  latter require \code{max=FALSE} (the default).
  
}
\value{
  The function returns an object of class \code{\linkS4class{Workflow}} .
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\seealso{
  \code{\link{performanceEstimation}},
  \code{\link{topPerformers}},
  \code{\link{rankWorkflows}},
  \code{\link{metricsSummary}}
}
\examples{
\dontrun{
## Estimating several evaluation metrics on different variants of a
## regression tree and of a SVM, on  two data sets, using one repetition
## of  10-fold CV

data(swiss)
data(mtcars)
library(e1071)

## run the experimental comparison
results <- performanceEstimation(
               c(PredTask(Infant.Mortality ~ ., swiss),
                 PredTask(mpg ~ ., mtcars)),
               c(workflowVariants(learner='svm',
                                  learner.pars=list(cost=c(1,5),gamma=c(0.1,0.01))
                                 )
               ),
               EstimationTask(metrics=c("mse","mae"),method=CV(nReps=2,nFolds=5))
                                 )

## get the top performer workflow for a metric and task
topPerformer(results,"mse","swiss.Infant.Mortality")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

