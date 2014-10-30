\name{rankWorkflows}
\alias{rankWorkflows}

\title{
  Provide a ranking of workflows involved in an estimation process.
}
\description{
  Given a \code{\linkS4class{ComparisonResults}} object resulting from a
  performance estimation experiment, this function provides a ranking
  (by default the top 5) of  
  the \emph{best} workflows involved in the comparison. The rankings are provided by
  task and for each evaluation metric. 
}
\usage{
rankWorkflows(compRes,top=min(5,length(workflowNames(compRes))),
              maxs=rep(FALSE,dim(compRes[[1]][[1]]@iterationsScores)[2]))
}

\arguments{
  \item{compRes}{
    An object of class \code{\linkS4class{ComparisonResults}} with the results of the
  performance estimation experiment.
}
  \item{top}{
    The number of workflows to include in the rankings (defaulting to 5
  or the number of workflows in the experiment if less than 5)
}
  \item{maxs}{
    A vector of booleans with as many elements are there are statistics measured in
    the experimental comparison. A \code{TRUE} value means the respective
    metric is to be maximized, while a \code{FALSE} means
    minimization. Defaults to all \code{FALSE} values.
}
}

\value{
  The function returns a named list with as many components as there
  are predictive tasks in the experiment. For each task you will get
  another named list, with as many elements as there evaluation
  metrics. For each of these components you have a data frame with \emph{N}
  lines, where \emph{N} is the size of the requested rank. Each line includes
  the name of the workflow in the respective rank position and the
  estimated score it got on that particular task / evaluation metric.
}

\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\link{performanceEstimation}},
  \code{\link{topPerformers}},
  \code{\link{topPerformer}},
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
## get a ranking of the top workflows for each task and evaluation metric
rankWorkflows(results)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

