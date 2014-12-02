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
responseValues(formula, data, na=NULL)
}

\arguments{
  \item{formula}{
A formula describing a prediction problem
}
  \item{data}{
The data frame containing the data of the prediction problem
}
  \item{na}{
A function to handle NAs in the data
}}
\value{
A vector of values
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\examples{
data(iris)
tgt <- responseValues(Species ~ ., iris)
summary(tgt)
}

\keyword{models}

