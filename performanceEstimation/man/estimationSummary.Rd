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
## Estimating several evaluation metrics on different variants of a
## regression tree on  a data set, using one repetition of 10-fold CV
data(swiss)

## First the user defined functions 
cv.rpartXse <- function(form, train, test, ...) {
    require(DMwR)
    t <- rpartXse(form, train, ...)
    p <- predict(t, test)
    mse <- mean((p - resp(form, test))^2)
    c(nmse = mse/mean((mean(resp(form, train)) - resp(form, test))^2), 
        mse = mse)
}

results <- experimentalComparison(
               c(dataset(Infant.Mortality ~ ., swiss)),
               c(variants('cv.rpartXse',se=c(0,0.5,1))),
               cvSettings(1,10,1234)
                                 )

## Get the statistics of a specific learner
estimationSummary(results,'cv.rpartXse.v1','swiss')

## Get the statistics of the learner that obtained the best NMSE on the
## swiss data set
estimationSummary(results,bestScores(results)$swiss['nmse','system'],'swiss')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
