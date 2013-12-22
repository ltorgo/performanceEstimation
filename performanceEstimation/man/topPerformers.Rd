\name{topPerformers}
\alias{topPerformers}

\title{
Obtain the best scores from a performance estimation experiment
}
\description{
  This function can be used to obtain the names of the workflows that obtained
  the best scores (the top performers) on an experimental
  comparison. This information will  be shown for each of the evaluation
  metrics involved in the  comparison and also for all predictive tasks
  that were used. 
}
\usage{
topPerformers(compRes,
           maxs=rep(FALSE,dim(compRes@tasks[[1]][[1]]@iterationsScores)[2]),
           digs=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{compRes}{
    A \code{\linkS4class{ComparisonResults}} object with the results of your experimental comparison.
}
  \item{maxs}{
    A vector of booleans with as many elements are there are statistics measured in
    the experimental comparison. A \code{TRUE} value means the respective
    statistic is to be maximized, while a \code{FALSE} means
    minimization. Defaults to all \code{FALSE} values, i.e. all metrics are to
    be minimized.
}
  \item{digs}{
    The number of digits (defaults to 3) used in the scores column of the results.
}
}
\details{
  This is an utility function to check which were the top performers in a
  comparative experiment for each data set and each evaluation
  metric. The notion of \emph{best performance} depends on the type of
  evaluation metric, thus the need for the second argument. Some
  evaluation statistics are to be maximized (e.g. accuracy), while
  others are to be minimized (e.g. mean squared error). If you have a
  mix of these types on your experiment then you can use the \code{maxs}
  parameter to inform the function of which are to be maximized and
  minimized. 
}
\value{
  The function returns a list with named components. The components
  correspond to the predictive tasks  used in the experimental comparison. For
  each component you get a \code{data.frame}, where the rows represent the
  statistics. For each statistic you get the name of the top performer
  (1st column of the data frame) and the respective score on that
  statistic (2nd column).
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\seealso{
\code{\link{performanceEstimation}}, \code{\link{rankWorkflows}}, \code{\link{metricsSummary}}
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
                                  learner.pars=list(cost=c(1,5),gamma=c(0.1,0.01))
                                 )
               ),
               CvSettings(2,5,1234)
                                 )
## get the top performers for each task and evaluation metric
topPerformers(results)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

