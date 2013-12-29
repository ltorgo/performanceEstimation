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
  independently of the used estimation method.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{EstimationResults(...)}
  providing the values for the class slots. The objects contain
  information on the predictive task, the workflow, the estimation
  settings, the metric scores and optionally also information on the
  true and predictive values for all test cases and any other
  information the author of the workflow decided to include in the
  object returned by the workflow function.
}
\section{Slots}{
  \describe{
    \item{\code{task}:}{Object of class \code{PredTask}  }
    \item{\code{workflow}:}{Object of class \code{Workflow}  }
    \item{\code{settings}:}{Object belonging to class union \code{EstimationSettings} }
    \item{\code{iterationsScores}:}{Object of class \code{matrix} }
    \item{\code{iterationsPreds}:}{Object of class \code{list} }
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
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{EstimationSettings}},  \code{\linkS4class{PredTask}},\code{\linkS4class{Workflow}},\code{\link{performanceEstimation}}
}
\examples{
showClass("EstimationResults")

\dontrun{
library(e1071)
data(swiss)

## Estimating the MAE and NMSE of an SVM on the swiss task
eval.res <- crossValidation(
             Workflow("standardWF",
                      list(learner="svm",
                           learner.pars=list(cost=10,gamma=0.1),
                           evaluator.pars=list(stats=c("mae","nmse"))
                          )
                     ),
             PredTask(Infant.Mortality ~ ., swiss),
             CvSettings(2,10,1234))

## Check a summary of the results
summary(eval.res)

}
}
\keyword{classes}
