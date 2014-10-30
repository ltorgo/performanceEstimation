\name{Holdout-class}
\Rdversion{1.1}
\docType{class}
\alias{Holdout}
\alias{Holdout-class}
\alias{show,Holdout-method}

\title{Class "Holdout" }
\description{	 This class of objects contains the information
  describing a hold out experiment, i.e. its settings.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{Holdout(...)}
  providing the values for the class slots.
  The objects contain information on the number of repetitions of the
  hold out experiment, the percentage of the given data to set as hold
  out test set, the random number generator seed, information on
  whether stratified sampling should be used and \emph{optionally} the
  concrete data splits to use on each iteration of the holdout
  experiment. Note that most of the times you will not supply these data
  splits as the holdout routines in this infra-structure will take care of
  building them. Still, this allows you to  replicate some experiment
  carried out with specific train/test splits.
}
\section{Slots}{
  \describe{
    \item{\code{nReps}:}{Object of class \code{numeric} indicating
      the number of repetitions of the N folds CV experiment (defaulting
      to 1). }
    \item{\code{hldSz}:}{Object of class \code{numeric} with the
      percentage (a number between 0 and 1) of cases to use as hold out
      (defaulting to 0.3).}
    \item{\code{strat}:}{Object of class \code{logical} indicating
      whether the sampling should or not be stratefied (defaulting to FALSE). }
    \item{\code{seed}:}{Object of class \code{numeric} with the
      random number generator seed (defaulting to 1234). }
    \item{\code{dataSplits}:}{Object of class \code{list} 
      containing the data splits to use on each repetition of a
      \code{nReps} Holdout experiment (defaulting to \code{NULL}). This list
      should contain \code{nReps} elements. Each element should be a
      vector with the row ids of the test set of the respective
      iteration.  On all these
      iterations the training set will be formed by the ids not
      appearing in the test set. }
  }
}
\section{Extends}{
Class \code{\linkS4class{EstCommon}}, directly.
Class \code{\linkS4class{EstimationMethod}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Holdout")}: method used to
      show the contents of a \code{Holdout} object. }
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
   \code{\linkS4class{Bootstrap}},
   \code{\linkS4class{CV}},
   \code{\linkS4class{EstimationMethod}},
   \code{\linkS4class{EstimationTask}}
}
\examples{
showClass("Holdout")

## 10 repetitions of a holdout experiment leaving on each repetition
## 20\% of the cases randomly chosen as test set (the holdout)
h1 <- Holdout(nReps=10,hldSz=0.2,strat=TRUE)
h1

## Small example illustrating the format of user supplied data splits
## in this case for 3 repetitions of a Holdout process where each test
## set has 10 cases
h2 <- Holdout(dataSplits=list(1:10,11:20,21:30))
h2

}
\keyword{classes}
