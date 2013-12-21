\name{HldSettings-class}
\Rdversion{1.1}
\docType{class}
\alias{HldSettings}
\alias{HldSettings-class}
\alias{show,HldSettings-method}

\title{Class "HldSettings" }
\description{	 This class of objects contains the information
  describing a hold out experiment, i.e. its settings.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{HldSettings(...)}
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
    \item{\code{seed}:}{Object of class \code{numeric} with the
      random number generator seed (defaulting to 1234). }
    \item{\code{strat}:}{Object of class \code{logical} indicating
      whether the sampling should or not be stratefied (defaulting to FALSE). }
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
Class \code{\linkS4class{EstimationSettings}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "HldSettings")}: method used to
      show the contents of a \code{HldSettings} object. }
	 }
       }
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
   \code{\linkS4class{McSettings}},
  \code{\linkS4class{LoocvSettings}}, \code{\linkS4class{BootSettings}},\code{\linkS4class{CvSettings}}, \code{\linkS4class{EstimationSettings}}
}
\examples{
showClass("HldSettings")

h1 <- HldSettings(nReps=10,hldSz=0.2,strat=TRUE)
h1

## small example illustrating the format of user supplied data splits
## it assumes that the source data is formed by 10 cases and that we
## will be using two repetitions of a holdout process were half of the
## rows are used for training and the remaining for testing
h2 <- HldSettings(dataSplits=matrix(c(rep(0,5),rep(1,5),
                                      rep(1,5),rep(0,5)),
                                    nrow=10,ncol=2))
h2
h2@dataSplits
}
\keyword{classes}
