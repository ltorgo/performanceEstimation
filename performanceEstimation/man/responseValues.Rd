\name{responseValues}
\alias{responseValues}

\title{
Obtain the target variable values of a prediction task
}
\description{
  This function obtains the values in the column whose name is the
  target variable of a prediction problem described by a formula.
}
\usage{
responseValues(formula, data)
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
A vector of values
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
tgt <- responseValues(Species ~ ., iris)
summary(tgt)
}

\keyword{models}

