\name{workflowPredictions}
\alias{workflowPredictions}
\alias{`workflowPredictions<-`}
\alias{'workflowPredictions<-'}
\title{
The predictions of a workflow run
}
\description{
This function gets or sets a matrix eith the predicted and true values
of the target variable in the context of the application of a certain
workflow to a predictive task.  Workflow runs must return an object of class
\code{\linkS4class{WFoutput}}. This function allows you to set or get
the predictions and true values of the test set involved in this run.

}
\usage{
workflowPredictions(obj)

workflowPredictions(obj) <- value
}

\arguments{
  \item{obj}{
    An object of class \code{\linkS4class{WFoutput}}
  }
  \item{value}{
    A list with three components: the first should be the true values of
    the target variable in the test set; the second the respective
    predictions of the workflow; and the third is optional and it is the
    vector of rownames of the test set data frame that was used.
  }
}
\value{
  A matrix with two columns, the first with the true values of the
  target on the test set, and the second the workflow predictions.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{WFoutput}}, \code{\link{workflowInformation}}, \code{\linkS4class{Workflow}}, \code{\link{runWorkflow}}

}
\examples{
\dontrun{
## An example of obtaining the information
library(e1071)
data(iris)
idx <- sample(1:150,100)
res <- standardWF(Species ~ .,iris[idx,],iris[-idx,],learner='svm')
workflowInformation(res)
res <- standardWF(Species ~ .,iris[idx,],iris[-idx,],learner='svm',.outModel=TRUE)
workflowPredictions(res)

## An example of creating a workflow function implementing a
## ensemble-like approach, that returns the individual models
## as extra information
RLensemble <- function(f,tr,ts,weightRT=0.5,step=FALSE,...) {
  require(DMwR,quietly=F)
  noNAsTR <- knnImputation(tr)
  noNAsTS <- knnImputation(ts)
  r <- rpartXse(f,tr,...)
  l <- lm(f,noNAsTR)
  if (step) l <- step(l,trace=0)
  pr <- predict(r,ts)
  pl <- predict(l,noNAsTS)
  ps <- weightRT*pr+(1-weightRT)*pl
  res <- WFoutput(c(correlation=cor(responseValues(f,ts),ps)))
  workflowPredictions(res) <- list(responseValues(f,ts),ps)
  res
}
}
}
\keyword{models}

