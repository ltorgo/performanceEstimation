\name{getIterationsPreds}
\alias{getIterationsPreds}

\title{
Obtaining the predictions returned by a workflow when applied to a task,
on a particular iteration of the estimation process, or on all iterations
}
\description{
  In the estimation process workflows are applied many times to different
train+test samples of each task. We call these repetitions, the
iterations of the estimation process. On each of these executions of the
workflows they must return the predictions for the test set. This
function allows you to obtain these predictions. The function also
allows you to obtain the predictions on all iterations, instead of a
single iteration.
}
\usage{
getIterationsPreds(obj, workflow = 1, task = 1, rep, fold, it, predsComp="preds")
}
\arguments{
  \item{obj}{
A \code{\linkS4class{ComparisonResults}} object
}
  \item{workflow}{
A string with the ID of a workflow (it can also be an integer). It
  defaults to 1 (the first workflow of the estimation experiment)
}
  \item{task}{
A string with the ID of a task (it can also be an integer). It
  defaults to 1 (the first task of the estimation experiment)
}
  \item{rep}{
An integer representing the repetition, which allows you to identify the iteration you want to
  inspect. You need to specify either this argument together with the
  argument \code{fold}, or only the argument \code{it}
}
  \item{fold}{
An integer representing the fold, which allows you to identify the iteration you want to
  inspect. You need to specify either this argument together with the
  argument \code{rep}, or only the argument \code{it}

}
  \item{it}{
An integer representing the iteration you want to
  inspect. Alternatively, for cross validation experiments, you may
  instead specify the repetition id and the fold id (arguments
  \code{rep} and \code{fold}, respectivily)

}
\item{predsComp}{
  A string with the name of the component of the list returned by the
  workflow that contains the predictions (it defaults to "preds")

}
}

\value{
  The result is either a vector of the predictions of a particular
  iteration or a matrix with the predictions on all iterations
}
\references{Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\link{getScores}},
  \code{\link{getIterationsInfo}},
  \code{\link{performanceEstimation}}
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

## Get the iterations scores of svm.v2 on swiss
getIterationsPreds(res,"svm.v2","swiss.Infant.Mortality",rep=1,fold=2)
## this would get the same
getIterationsPreds(res,"svm.v2","swiss.Infant.Mortality",it=2)

getIterationsPreds(res,"svm.v2","swiss.Infant.Mortality",rep=2,fold=3)
## this would get the same
getIterationsPreds(res,"svm.v2","swiss.Infant.Mortality",it=8)

## Get the results of all iterations
getIterationsPreds(res,"svm.v1","swiss.Infant.Mortality")

}
}
\keyword{models}

