\name{workflowInformation}
\alias{workflowInformation}
\alias{`workflowInformation<-`}
\alias{'workflowInformation<-'}
\title{
Extra information associated with a workflow run
}
\description{
This function gets or sets extra information resulting from runing a
workflow. Workflow runs must return an object of class
\code{\linkS4class{WFoutput}}. This function allows you to set or get
some extra information that can be stored in this class of objects.
}
\usage{
workflowInformation(obj)

workflowInformation(obj) <- value
}
\arguments{
  \item{obj}{
    An object of class \code{\linkS4class{WFoutput}}
  }
  \item{value}{
    A list with free format, where each component may contain whatever
    extra information you want to associate to a workflow output object
  }
}
\value{
  When getting the information the result is a list
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{WFoutput}}, \code{\link{workflowPredictions}}, \code{\linkS4class{Workflow}}, \code{\link{runWorkflow}}

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
workflowInformation(res)

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
  res <- WFoutput(c(correlation=cor(resp(f,ts),ps)))
  workflowInformation(res) <- list(tree=r,linearModel=l)
  res
}
}
}
\keyword{models}

