\name{workflowPredictions}
\alias{workflowPredictions}

\title{
The predictions resulting from running a workflow
}
\description{
This function returns a matrix with the predicted and true values
of the target variable in the context of the application of a certain
workflow to a predictive task.  Workflow runs must return an object of class
\code{\linkS4class{WFoutput}}. This function allows you to set or get
the predictions and true values of the test set involved in this run.

}
\usage{
workflowPredictions(obj)
}

\arguments{
  \item{obj}{
    An object of class \code{\linkS4class{WFoutput}}
  }
}
\value{
  A matrix with at least two columns, the first with the true values of the
  target on the test set, and the second the workflow predictions. In
  the case of probabilistic classifiers it may also include extra
  columns with the probabilities for each class.
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{WFoutput}},
  \code{\link{workflowInformation}},
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
workflowPredictions(r)

}
}
\keyword{models}

