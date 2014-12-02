\name{LOOCV-class}
\Rdversion{1.1}
\docType{class}
\alias{LOOCV}
\alias{LOOCV-class}
\alias{show,LOOCV-method}

\title{Class "LOOCV"  }
\description{	This class of objects contains the information
  describing a leave one out cross validation estimation experiment, i.e. its settings. }
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{LOOCV(...)}
  providing the values for the class slots.
  These objects contain infoprmation on the random number generator seed
  and also whether the execution of the experiments should be verbose.
}
\section{Slots}{
  \describe{
    \item{\code{seed}:}{Object of class \code{numeric} with the
      random number generator seed (defaulting to 1234). }
    \item{\code{dataSplits}:}{Object of class \code{list} 
      containing the data splits to use on each repetition of a
      leave one out cross validation experiment (defaulting to \code{NULL}). This list
      should contain as many elements as there are cases in the task
      data set. Each element should be the row id of the test case of the respective
      iteration.  On all these
      iterations the training set will be formed by the remaining ids. }
  }
}
\section{Extends}{
Class \code{\linkS4class{EstCommon}}, directly.
Class \code{\linkS4class{EstimationMethod}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "LOOCV")}: method used to
      show the contents of a \code{LOOCV} object. }
	 }
       }
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
   \code{\linkS4class{MonteCarlo}},
   \code{\linkS4class{CV}},
   \code{\linkS4class{Bootstrap}},
   \code{\linkS4class{Holdout}},
   \code{\linkS4class{EstimationMethod}},
   \code{\linkS4class{EstimationTask}}
}
\examples{
showClass("LOOCV")

s <- LOOCV()
s

}
\keyword{classes}
