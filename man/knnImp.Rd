\name{knnImp}
\alias{knnImp}

\title{
  Fill in NA values with the values of the nearest neighbours
}
\description{
  Function that fills in all NA values using the k Nearest
  Neighbours of each case with NA values. It uses the median/most
  frequent value within the neighbours to fill in the NAs.
}
\usage{
knnImp(data, k = 10, scale = TRUE, distData = NULL)
}

\arguments{
  \item{data}{
    A data frame with the data set
  }
  \item{k}{
    The number of nearest neighbours to use (defaults to 10)
  }
  \item{scale}{
    Boolean setting if the data should be scale before finding the
    nearest neighbours (defaults to \code{TRUE})
  }
  \item{distData}{
    Optionally you may sepecify here a data frame containing the data set
    that should be used to find the neighbours. This is usefull when
    filling in NA values on a test set, where you should use only
    information from the training set. This defaults to NULL, which means
    that the neighbours will be searched in \code{data}
  }
}
\details{
  This function uses the k-nearest neighbours to fill in the unknown (NA)
  values in a data set. For each case with any NA value it will search for
  its k most similar cases and use the values of these cases to fill in
  the unknowns.

  The function will use either the median (in case of numeric variables)
  or the most frequent value (in case of factors), of the neighbours to
  fill in the NAs.  
}
\value{
  A data frame without NA values
}
\references{
    Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}

}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{na.omit}}
}
\examples{
\dontrun{
data(algae,package="DMwR")
cleanAlgae <- knnImp(algae)
summary(cleanAlgae)
}
}
\keyword{models}
