\name{EstimationResults-class}
\Rdversion{1.1}
\docType{class}
\alias{EstimationResults}
\alias{EstimationResults-class}
\alias{plot,EstimationResults-method}
\alias{summary,EstimationResults-method}
\alias{show,EstimationResults-method}

\title{Class "EstimationResults" }
\description{This is the class of the objects storing the results of
  estimating the  performance of a workflow on a predictive task
  using some estimation method.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{EstimationResults(...)}
  providing the values for the class slots. The objects contain
  information on the predictive task, the workflow, the estimation
  task, the metric scores and optionally also information on results
  obtained at each iteration of the estimation process.
}
\section{Slots}{
  \describe{
    \item{\code{task}:}{Object of class \code{PredTask}  }
    \item{\code{workflow}:}{Object of class \code{Workflow}  }
    \item{\code{estTask}:}{Object belonging to class \code{EstimationTask} }
    \item{\code{iterationsScores}:}{Object of class \code{matrix} }
    \item{\code{iterationsInfo}:}{Object of class \code{list} }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "EstimationResults", y = "missing")}: method
      used to visualize the results of the estimation process. }
    \item{show}{\code{signature(object = "EstimationResults")}: shows the contents of an object in a proper way}
    \item{summary}{\code{signature(object = "EstimationResults")}: method used to
      obtain a summary of the results of the estimation process. }
	 }
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{EstimationMethod}},
  \code{\linkS4class{EstimationTask}},
  \code{\linkS4class{PredTask}},
  \code{\linkS4class{Workflow}},
  \code{\link{performanceEstimation}}
}
\examples{
showClass("EstimationResults")

\dontrun{
library(e1071)
data(swiss)

## Estimating the MAE and NMSE of an SVM on the swiss task
eval.res <- cvEstimates(
                 Workflow(learner="svm",learner.pars=list(cost=10,gamma=0.1)),
                 PredTask(Infant.Mortality ~ .,swiss),
                 EstimationTask(metrics=c("mae","nmse"),method=CV(nReps=2))
                       )

## Check a summary of the results
summary(eval.res)

}
}
\keyword{classes}
