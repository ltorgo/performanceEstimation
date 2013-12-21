\name{is.regression}
\alias{is.regression}

\title{
  Check if a certain predictive task is a regression problem
}
\description{
  This function tests if a task defined by a formula over a data set is
  a regression task, which will be the case if the target variable is
  numeric. 
}
\usage{
is.regression(formula, data)
}

\arguments{
  \item{formula}{
A formula describing a prediction problem
}
  \item{data}{
The data frame containing the data of the prediction problem
}
}
\value{
A logical value
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{as.formula}}
}
\examples{
data(iris)
if (!is.regression(Species ~ .,iris)) cat("This is not a regression task!\n")
}

\keyword{models}

