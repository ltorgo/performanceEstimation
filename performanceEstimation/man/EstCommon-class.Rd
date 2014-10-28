\name{EstCommon-class}
\Rdversion{1.1}
\docType{class}
\alias{EstCommon-class}

\title{Class \code{"EstCommon"}}
\description{
An auxiliar class defining slots common to all experimental settings classes.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EstCommon", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{seed}:}{Object of class \code{"numeric"}}
    \item{\code{dataSplits}:}{Object of class \code{"OptMatrix"} }
  }
}
\section{Methods}{
No methods defined with class "EstCommon" in the signature.
}
\references{Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{Luis Torgo \email{ltorgo@dcc.fc.up.pt}}
\seealso{
   \code{\linkS4class{CV}},
   \code{\linkS4class{MonteCarlo}},
   \code{\linkS4class{LOOCV}},
   \code{\linkS4class{Bootstrap}},
   \code{\linkS4class{Holdout}},
   \code{\linkS4class{EstimationMethod},
   \code{\linkS4class{EstimationTask}}
}
\examples{
showClass("EstCommon")
}
\keyword{classes}
