\name{McSettings-class}
\Rdversion{1.1}
\docType{class}
\alias{McSettings}
\alias{McSettings-class}
\alias{show,McSettings-method}

\title{Class "McSettings"  }
\description{	 This class of objects contains the information
  describing a monte carlo experiment, i.e. its settings.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{McSettings(...)}
  providing the values for the class slots.
  These objects contain information on the number of repetitions of the
  experiments, the data used for training the models on each repetition,
  the  data used for testing these models, the random number
  generator seed and \emph{optionally} the 
  concrete data splits to use on each iteration of the Monte Carlo
  experiment. Note that most of the times you will not supply these data
  splits as the Monte Carlo routines in this infra-structure will take care of
  building them. Still, this allows you to  replicate some experiment
  carried out with specific train/test splits.
}
\section{Slots}{
  \describe{
    \item{\code{nReps}:}{Object of class \code{numeric} indicating
      the number of repetitions of the Monte Carlo experiment (defaulting
      to 10). }
    \item{\code{szTrain}:}{Object of class \code{numeric}. If it is a
      value between 0 and 1 it is interpreted as a percentage of the
      available data set, otherwise it is interpreted as the number of
      cases to use. It defaults to 0.25. }
    \item{\code{szTest}:}{Object of class \code{numeric} If it is a
      value between 0 and 1 it is interpreted as a percentage of the
      available data set, otherwise it is interpreted as the number of
      cases to use. It defaults to 0.25. }
    \item{\code{seed}:}{Object of class \code{numeric} with the
      random number generator seed (defaulting to 1234). }
    \item{\code{dataSplits}:}{Object of class \code{matrix} or \code{NULL}
      containing the data splits to use on each repetition of the
      Monte Carlo experiment (defaulting to \code{NULL}). This  matrix
      should have as many rows as
      there are cases in the data sample used in the experiment, and as
      many columns as there are repetitions in the experiment. Each column of
      this matrix should contain 1's,  0's or NA's', with 1's on rows that are
      part of the test set of the corresponding repetition, and 0's in the
      rows that are to be used as training cases in that repetition. All
    rows not being part of neither the training nor the test sets whould
  have the value NA.}
  }
}
\section{Extends}{
Class \code{\linkS4class{EstimationSettings}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "McSettings")}: method used to
      show the contents of a \code{McSettings} object. }
	 }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
   \code{\linkS4class{LoocvSettings}},
  \code{\linkS4class{CvSettings}}, \code{\linkS4class{BootSettings}},\code{\linkS4class{HldSettings}}, \code{\linkS4class{EstimationSettings}}
}
\examples{
showClass("McSettings")

m1 <- McSettings(nReps=10,szTrain=0.3,szTest=0.2)
m1

## Small example illustrating the format of user supplied data splits
## it assumes that the source data is formed by 10 cases and that each
## model is trainined with 3 cases and tested in the following case.
## This is obviously a unrealistic example in terms of size but
## illustrates the format of the data splits
m2 <- McSettings(dataSplits=matrix(c(c(NA,NA,rep(1,3),0,rep(NA,4),
                                     rep(1,3),0,rep(NA,6)),
                                   nrow=10,ncol=2)))
m2
m2@dataSplits
}
\keyword{classes}
