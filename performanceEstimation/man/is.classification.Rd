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
is.classification(formula, data)
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
if (is.classification(Species ~ .,iris)) cat("That is a classification task.\n")
}

\keyword{models}

