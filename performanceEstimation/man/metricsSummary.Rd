\name{metricsSummary}
\alias{metricsSummary}

\title{
  Obtains a summary of the individual metric scores obtained by each
  workflow on a set of tasks. 
}
\description{
  Given a \code{\linkS4class{ComparisonResults}} object this function provides a summary
  statistic (defaulting to the mean) of the individual scores
  obtained on a each evaluation metric over all repetitions carried
  out in the estimation process. This is done for all workflows and
  tasks of the performance estimation experiment. The function can be handy to
  obtain things like for instance the maximum score obtained by each
  workflow on a particular metric over all repetitions of the
  experimental process. It is also usefull (using its defaults) as a way
  to quickly getting the estimated values for each metric obtained by
  each alternative workflow and task (see the Examples section).
}
\usage{
metricsSummary(compRes, summary = "mean", ...)
}

\arguments{
  \item{compRes}{
    An object of class \code{\linkS4class{ComparisonResults}} with the results of a
  performance estimation experiment.
}
  \item{summary}{
     A string with the name of the function that you want to use to
  obtain the summary (defaults to "mean"). This function will be applied
  to the set of individual scores of each workflow on each task and for
  all metrics.
}
\item{...}{
     Further arguments passed to the selected summary function.
}
}
\value{
  The result of this function is a named list with as many components as
  there are predictive tasks. For each
  task (component), we get a matrix with as many columns as there are
  workflows and as many rows as there are evaluation metrics. The values
  on this matrix are the results of applying the selected summary
  function to the metric scores on each iteration of the estimation process.
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\seealso{
\code{\link{performanceEstimation}}, \code{\link{rankWorkflows}}
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
               c(workflowVariants('standardWF',
                                  learner='svm',
                                  learner.pars=list(cost=c(1,5),gamma=c(0.1,0.01)),
                                  evaluator.pars=list(stats="all")
                                 )
               ),
               CvSettings(2,5,1234)
                                 )
## Get the minium value of each metric on all iterations of the CV
## process. 
metricsSummary(results,summary="min")

## Get a summary table for each task with the estimated scores for each
## metric by each workflow
metricsSummary(results)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

