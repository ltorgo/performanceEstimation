\name{runWorkflow}
\alias{runWorkflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a workflow on a predictive task
}
\description{
  This function can be used to apply (run) a workflow.

  It receives in the first argument an object of class
  \code{\linkS4class{Workflow}} and then any other arguments that are
  required to run the workflow, which will typically involve at least a
  formula, a training data set (a data frame), and a test data set
  (another data frame).
}
\usage{
runWorkflow(l, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
 An object of class \code{\linkS4class{Workflow}}
}
  \item{\dots}{
Further arguments that are required by the workflow function
}
}
\value{
  The execution of a workflow should produce a list as result. If you
  plan to apply any of the functions provided in this package to
  calculate standard performance metrics
  (\code{\link{classificationMetrics}} or
  \code{\link{regressionMetrics}}) then your list should contain at
  least two components: one named \emph{trues} with the true values of
  the target variable in the test set, and the other named \emph{preds}
  with the respective predictions of the workflow.
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\linkS4class{Workflow}}

}
\examples{
\dontrun{
## A simple example
data(iris)
w1 <- Workflow("mySolution",par1="gaussian",k=10)
runWorkflow(w1,Species ~ .,iris[1:100,],iris[101:150,])
}
}
\keyword{models}

