\name{learner-class}
\Rdversion{1.1}
\docType{class}
\alias{learner}
\alias{learner-class}
\alias{show,learner-method}

\title{Class "learner"}
\description{	Objects of the class learner represent learning systems
  that can be used in the routines designed to carry out experimental
  comparisons within the DMwR package.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{learner( ...)}.
	 The objects contain information on the R function implementing
	 the learning algorithm, and also a list of arguments with
	 respective values, that should be used when calling that function.
}
\section{Slots}{
  \describe{
    \item{\code{func}:}{A character string containing the name of the R
      function that implements the learning algorithm used by the
      learner object.}
    \item{\code{pars}:}{A named list containing the parameters and
      respective values to be used when calling the learner (defaulting
      to the empty list).}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "learner")}: method used to
      show the contents of a learner object. }
	 }
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
CRC Press (ISBN: 9781439810187). \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{\code{\linkS4class{task}}, \code{\linkS4class{dataset}}, \code{\link{runLearner}}
}
\examples{
showClass("learner")
}
\keyword{classes}
