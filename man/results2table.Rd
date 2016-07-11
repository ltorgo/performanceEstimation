\name{results2table}
\alias{results2table}

\title{
  Obtains a dplyr data frame table object containing all the results of
  an experiment
}
\description{
  This function produces a dplyr data frame table object with the
  information on all iterations of an experiment. This type of objects
  may be easier to manipulate in terms of querying these results,
  particular for larger experiments involving lots of tasks, workflows
  and metrics.
}
\usage{
results2table(res)
}

\arguments{
  \item{res}{
    This is a \code{\linkS4class{ComparisonResults}} object (type "class?ComparisonResults" for details)
    that contains the results of a performance estimation  experiment obtained
    through the \code{performanceEstimation()} function. 
  }
}
\value{
  The function returns a dplyr data frame table object containing all
  resutls of the experiment. The object has the columns: Task, Workflow,
  nrIt, Metric and Score. Each row is one train+test cycle within the
  experiment, i.e. contains the score of some metric obtained by some
  workflow on one train+test iteration of a task.
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
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
  PredTask(Infant.Mortality ~ .,swiss,"Swiss"),
  workflowVariants(learner="svm",
                   learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
  EstimationTask(metrics=c("mse","nmae"),method=CV(nReps=2,nFolds=5))
  )

## Obtaining a table with the results
library(dplyr)
tbl <- results2table(res)

## Mean and standard deviation of each workflow per task (only one in
## this example) and metric
group_by(tbl,Task,Workflow,Metric) %>%
     summarize_each_(funs(mean,sd),"Score")

## Top 2 workflows in terms of MSE for this task
filter(tbl,Task=="Swiss",Metric=="mse") %>%
    group_by(Workflow) %>%
      summarize_each_(funs(mean),"Score") %>%
        arrange(Score) %>%
          slice(1:2)
}
}
\keyword{models}

