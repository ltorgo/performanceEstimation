\name{BootSettings-class}
\Rdversion{1.1}
\docType{class}
\alias{BootSettings}
\alias{BootSettings-class}
\alias{show,BootSettings-method}

\title{Class "BootSettings" }
\description{	 This class of objects contains the information
  describing a bootstrap experiment, i.e. its settings.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{BootSettings(...)}
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
    \item{\code{dataSplits}:}{Object of class \code{matrix} or \code{NULL}
      containing the data splits to use on each repetition of a
      bootstrap experiment (defaulting to \code{NULL}). This  matrix
      should have as many rows as
      there are cases in the data sample used in the experiment, and as
      many columns as there are repetitions in the experiment. Each column of
      this matrix should contain 1's and 0's, with 1's on rows that are
      part of the test set of the corresponding repetition, and 0's in the
      rows that are to be used as training cases in that repetition.  }
  }
}
\section{Extends}{
Class \code{\linkS4class{EstimationSettings}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "BootSettings")}: method used to
      show the contents of a \code{BootSettings} object.  }
	 }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
   \code{\linkS4class{McSettings}},
  \code{\linkS4class{LoocvSettings}}, \code{\linkS4class{CvSettings}},\code{\linkS4class{HldSettings}}, \code{\linkS4class{EstimationSettings}}
}
\examples{
showClass("BootSettings")

s <- BootSettings(type=".632",nReps=400)
s

## small example illustrating the format of user supplied data splits
## it assumes that the source data is formed by 10 cases and that we
## will be using two repetitions of a boostrap process 
s2 <- BootSettings(dataSplits=matrix(c(sample(0:1,10,replace=TRUE),
                                       sample(0:1,10,replace=TRUE)),
                                     nrow=10,ncol=2))
s2
s2@dataSplits
}
\keyword{classes}
