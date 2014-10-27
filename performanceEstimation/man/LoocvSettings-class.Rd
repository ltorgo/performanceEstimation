\name{LoocvSettings-class}
\Rdversion{1.1}
\docType{class}
\alias{LoocvSettings}
\alias{LoocvSettings-class}
\alias{show,LoocvSettings-method}

\title{Class "LoocvSettings"  }
\description{	This class of objects contains the information
  describing a leave one out cross validation estimation experiment, i.e. its settings. }
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{LoocvSettings(...)}
  providing the values for the class slots.
  These objects contain infoprmation on the random number generator seed
  and also whether the execution of the experiments should be verbose.
}
\section{Slots}{
  \describe{
    \item{\code{verbose}:}{Object of class \code{logical} indicating
      whether the execution of the experiments should be verbose (defaulting to FALSE). }
    \item{\code{seed}:}{Object of class \code{numeric} with the
      random number generator seed (defaulting to 1234). }
    \item{\code{dataSplits}:}{Object of class \code{matrix} or \code{NULL}
      containing the data splits to use on each repetition of the
      holdout experiment (defaulting to \code{NULL}). This  matrix
      should have as many rows as
      there are cases in the data sample used in the experiment, and as
      many columns as there are repetitions in the experiment. Each column of
      this matrix should contain 1's and 0's, with 1's on rows that are
      part of the test set of the corresponding repetition, and 0's in the
      rows that are to be used as training cases in that repetition.   }
  }
}
\section{Extends}{
Class \code{\linkS4class{EstCommon}}, directly.
Class \code{\linkS4class{EstimationSettings}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "LoocvSettings")}: method used to
      show the contents of a \code{LoocvSettings} object. }
	 }
       }
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
   \code{\linkS4class{McSettings}},
   \code{\linkS4class{CvSettings}},
   \code{\linkS4class{BootSettings}},
   \code{\linkS4class{HldSettings}},
   \code{\linkS4class{EstimationSettings}},
   \code{\linkS4class{EstimationTask}}
}
\examples{
showClass("LoocvSettings")

s <- LoocvSettings(verbose=TRUE)
s
}
\keyword{classes}
