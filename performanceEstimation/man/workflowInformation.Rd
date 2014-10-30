\name{workflowInformation}
\alias{workflowInformation}
\alias{workflowInformation<-}
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
  \code{\linkS4class{WFoutput}},
  \code{\link{workflowPredictions}},
  \code{\linkS4class{Workflow}},
  \code{\link{runWorkflow}}

}
\examples{
\dontrun{
## An example of obtaining the information
data(iris)
library(e1071)
w <- Workflow(learner="svm",learner.pars=list(cost=10))
r <- runWorkflow(w,Species ~ .,iris[1:100,],iris[101:150,])
workflowInformation(r)
}
}

\keyword{models}

