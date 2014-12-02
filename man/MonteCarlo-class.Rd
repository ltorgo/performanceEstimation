\name{MonteCarlo-class}
\Rdversion{1.1}
\docType{class}
\alias{MonteCarlo}
\alias{MonteCarlo-class}
\alias{show,MonteCarlo-method}

\title{Class "MonteCarlo"  }
\description{	 This class of objects contains the information
  describing a monte carlo experiment, i.e. its settings.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{MonteCarlo(...)}
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
    \item{\code{dataSplits}:}{Object of class \code{list} 
      containing the data splits to use on each Monte Carlo
      repetition. Each element should be a list with two components:
      \code{test} and \code{train}, on this order. Each of these is a
      vector with the row ids to use as test and train sets of each
      repetition of the Monte Carlo experiment. }
  }
}
\section{Extends}{
Class \code{\linkS4class{EstCommon}}, directly.
Class \code{\linkS4class{EstimationMethod}}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "MonteCarlo")}: method used to
      show the contents of a \code{MonteCarlo} object. }
	 }
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
   \code{\linkS4class{LOOCV}},
   \code{\linkS4class{CV}},
   \code{\linkS4class{Bootstrap}},
   \code{\linkS4class{Holdout}},
   \code{\linkS4class{EstimationMethod}},
   \code{\linkS4class{EstimationTask}}
}
\examples{
showClass("MonteCarlo")

m1 <- MonteCarlo(nReps=10,szTrain=0.3,szTest=0.2)
m1

## Small example illustrating the format of user supplied data splits
## it assumes that the source data is formed by 10 cases and that each
## model is trainined with 3 cases and tested in the following case.
## This is obviously a unrealistic example in terms of size but
## illustrates the format of the data splits
m2 <- MonteCarlo(dataSplits=list(list(test=sample(1:150,50),train=sample(1:150,50)),
                                   list(test=sample(1:150,50),train=sample(1:150,50)),
                                   list(test=sample(1:150,50),train=sample(1:150,50))
                                  ))
m2

}
\keyword{classes}
