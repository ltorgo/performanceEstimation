\name{getIterationsInfo}
\alias{getIterationsInfo}

\title{
Obtaining the information returned by a workflow when applied to a task,
on a particular iteration of the estimation process or on all iterations
}
\description{
In the estimation process workflows are applied many times to different
train+test samples of each task. We call these repetitions, the
iterations of the estimation process. On each of these executions of the
workflows, they typically return not only the predictions for the test
set but also some extra information, in the form of a list. This
function allows you to inspect this extra information 
}
\usage{
getIterationsInfo(obj, workflow = 1, task = 1, rep, fold, it)
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
}
\value{
A list with the information returned by the workflow on the selected
iteration or a list containing as many components as there are
iterations (i.e. a list of lists) in case you have opted for obtaining
all iterations results
}
\references{Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\link{getIterationPreds}},
  \code{\link{getScores}},
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
getIterationsInfo(res,"svm.v2","swiss.Infant.Mortality",rep=1,fold=2)
## this would get the same
getIterationsInfo(res,"svm.v2","swiss.Infant.Mortality",it=2)

getIterationsInfo(res,"svm.v2","swiss.Infant.Mortality",rep=2,fold=3)
## this would get the same
getIterationsInfo(res,"svm.v2","swiss.Infant.Mortality",it=8)

## Get the results of all iterations
getIterationsInfo(res,"svm.v1","swiss.Infant.Mortality")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

