\name{CV-class}
\Rdversion{1.1}
\docType{class}
\alias{CV}
\alias{CV-class}
\alias{show,CV-method}

\title{Class "CV"}
\description{	This class of objects contains the information
  describing a cross validation experiment, i.e. its settings.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{CV(...)}
  providing the values for the class slots. 
  These objects include information on the number of repetitions of the
  cross validation (CV) experiment, the number of folds, the random number generator seed,
  whether the sampling should or not be stratified and \emph{optionally}, the
  concrete data splits to use on each repetition and iteration of the CV
  experiment. Note that most of the times you will not supply these data
  splits as the CV routines in this infra-structure will take care of
  building them. Still, this allows you to  replicate some experiment
  carried out with specific train/test splits.
}
\section{Slots}{
  \describe{
    \item{\code{nReps}:}{Object of class \code{numeric} indicating
      the number of repetitions of the N folds CV experiment (defaulting
      to 1). }
    \item{\code{nFolds}:}{Object of class \code{numeric} with the
      number of folds on each CV experiment (defaulting to 10). }
    \item{\code{strat}:}{Object of class \code{logical} indicating
      whether the sampling should or not be stratified (defaulting to FALSE). }
    \item{\code{seed}:}{Object of class \code{numeric} with the
      random number generator seed (defaulting to 1234). }
    \item{\code{dataSplits}:}{Object of class \code{list} 
      containing the data splits to use on each repetition of a
      k-folds CV experiment (defaulting to \code{NULL}). This list
      should contain \code{nReps x nFolds} elements. Each element should be a
      vector with the row ids of the test set of the respective
      iteration. For instance, on a 3 x 10-fold CV experiment the 10th
      element should contain the ids of the test cases of the 10th fold
      of the first repetition and the 11th element the ids of the test
      cases on the 1st fold of the 2nd repetition. On all these
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
    \item{show}{\code{signature(object = "CV")}: method used to
      show the contents of a \code{CV} object. }
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
   \code{\linkS4class{Holdout}},
   \code{\linkS4class{EstimationMethod}},
   \code{\linkS4class{EstimationTask}}
}
\examples{
showClass("CV")

## the defaults (1 x 10-fold CV)
s <- CV()

## stratified 2 x 5-fold CV
s1 <- CV(nReps=2,nFolds=5,strat=TRUE)

## Small example illustrating the format of user supplied data splits.
## This could be a 3-fold CV process of a data set with 30 cases
s2 <- CV(dataSplits=list(1:10,11:20,21:30))
s2

}
\keyword{classes}
