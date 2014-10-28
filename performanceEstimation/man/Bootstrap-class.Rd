\name{Bootstrap-class}
\Rdversion{1.1}
\docType{class}
\alias{Bootstrap}
\alias{Bootstrap-class}
\alias{show,Bootstrap-method}

\title{Class "Bootstrap" }
\description{	 This class of objects contains the information
  describing a bootstrap experiment, i.e. its settings.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{Bootstrap(...)}
  providing the values for the class slots.
  The objects contain information on the type of boostrap, the number of
  repetitions, the random number generator seed
  and \emph{optionally} the 
  concrete data splits to use on each iteration of the boostrap
  experiment. Note that most of the times you will not supply these data
  splits as the boostrap routines in this infra-structure will take care of
  building them. Still, this allows you to  replicate some experiment
  carried out with specific train/test splits. 
}
\section{Slots}{
  \describe{
    \item{\code{type}:}{Object of class \code{character} indicating
      the type of boostrap estimates to use: "e0" (default) or ".632". }
    \item{\code{nReps}:}{Object of class \code{numeric} indicating
      the number of repetitions of the bootstrap experiment (defaulting
      to 200). }
    \item{\code{seed}:}{Object of class \code{numeric} with the
      random number generator seed (defaulting to 1234). }
    \item{\code{dataSplits}:}{Object of class \code{list} 
      containing the data splits to use on each bootstrap
      repetition. Each element should be a list with two components:
      \code{test} and \code{train}, on this order. Each of these is a
      vector with the row ids to use as test and train sets of each
      repetition of the bootstrap experiment. }
  }
}
\section{Extends}{
Class \code{\linkS4class{EstCommon}}, directly.
Class \code{\linkS4class{EstimationMethod}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Bootstrap")}: method used to
      show the contents of a \code{Bootstrap} object.  }
	 }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
   \code{\linkS4class{MonteCarlo}},
   \code{\linkS4class{LOOCV}},
   \code{\linkS4class{CV}},
   \code{\linkS4class{Holdout}},
   \code{\linkS4class{EstimationMethod}},
   \code{\linkS4class{EstimationTask}}
}
\examples{
showClass("Bootstrap")

s <- Bootstrap(type=".632",nReps=400)
s

## Small example illustrating the format of user supplied data splits
s2 <- Bootstrap(dataSplits=list(list(test=sample(1:150,50),train=sample(1:150,50)),
                                list(test=sample(1:150,50),train=sample(1:150,50)),
                                list(test=sample(1:150,50),train=sample(1:150,50))
                               ))
s2
s2@dataSplits
}
\keyword{classes}
