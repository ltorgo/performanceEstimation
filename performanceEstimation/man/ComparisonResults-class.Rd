\name{ComparisonResults-class}
\Rdversion{1.1}
\docType{class}
\alias{ComparisonResults}
\alias{ComparisonResults-class}
\alias{plot,ComparisonResults-method}
\alias{show,ComparisonResults-method}
%%\alias{subset,ComparisonResults-method}
\alias{summary,ComparisonResults-method}

\title{Class "ComparisonResults" }
\description{	 This is the main class that holds the results of
  performance estimation experiments involving several alternative
  workflows being applied and compared to several predictive tasks. For
  each workflow and task, a set of predictive performance metrics are
  estimated using some methodology and the results of this process are
  stored on this class of objets. }
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{ComparisonResults(...)}. These object are essentially
  a list of lists of objects of class
  \code{\linkS4class{EstimationResults}}. The top level list has as many
  components as there are tasks. For each task there will be a sub-list
  containing as many components as there are alternative workflows. Each
  of these components will contain and object of class
  \code{\linkS4class{EstimationResults}} with the estimation results for
  the particular workflow / task combination.
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "ComparisonResults", y = "missing")}: plots
      the results of the experiments. It can result in an over-cluttered
    graph if too many workflows/tasks/evaluation metrics - use the
    subset method (see below) to overcome this.}
    \item{show}{\code{signature(object = "ComparisonResults")}: shows the contents of an object in a proper way}
    \item{subset}{\code{signature(x = "ComparisonResults")}: can be used to obtain
    a smaller ComparisonResults object containing only a subset of the information
  of the provided object. This method also accepts the arguments "tasks",
  "workflows" and "statistics". All are vectors of numbers or names
  that can be used to subset the original object. They default to all values of each dimension. See "methods?subset" for further details.}
    \item{summary}{\code{signature(object = "ComparisonResults")}: provides a
      summary of the performance estimation experiment. }
	 }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{performanceEstimation}},
  \code{\link{pairedComparisons}},
  \code{\link{rankWorkflows}},
  \code{\link{topPerformers}},
  \code{\link{metricsSummary}},
  \code{\link{mergeEstimationRes}}  
}
\examples{
showClass("ComparisonResults")
\dontrun{
## Estimating MAE, MSE, RMSE and MAPE for 3 variants of both
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
  EstimationTask(metrics=c("mae","mse","rmse","mape"),method=CV())
  )

## Check a summary of the results
summary(res)

topPerformers(res)

}

}
\keyword{classes}
