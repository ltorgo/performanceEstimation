\name{is.classification}
\alias{is.classification}

\title{
  Check if a certain predictive task is a classification problem
}
\description{
  This function tests if a task defined by a formula over a data set is
  a classification task, which will be the case if the target variable is
  nominal. 
}
\usage{
is.classification(task)
}

\arguments{
  \item{task}{
An object of class \code{\linkS4class{PredTask}}
}
}
\value{
A logical value
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{is.regression}}
}
\examples{
data(iris)
tsk <- PredTask(Species ~ .,iris)
if (is.classification(tsk)) cat("This is a classification task.\n")
}

\keyword{models}

