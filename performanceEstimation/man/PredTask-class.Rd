\name{PredTask-class}
\Rdversion{1.1}
\docType{class}
\alias{PredTask}
\alias{PredTask-class}
\alias{show,PredTask-method}

\title{Class "PredTask" }
\description{	 This is the class of objects that represent a predictive task  }
\section{Objects from the Class}{
  Objects can be created by calls to the constructor \code{PredTask(...)} providing the values for the class slots.
  The objects include information on the name of the predictive task
  (which is optional),
  the formula and the data frame with the data used in the task.
}
\section{Slots}{
	 \describe{
    \item{\code{formula}:}{Object of class \code{formula} containing
      the formula representing the predictive task }
    \item{\code{data}:}{Object coercible to class \code{data.frame} containing
      the data of the problem }
    \item{\code{name}:}{Object of class \code{character} containing an
    internal name of the task}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "PredTask")}: method used to
      show the contents of a PredTask object. }
	 }
}

\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
	\code{\linkS4class{Workflow}}
}
\examples{
showClass("PredTask")
data(iris)
t1 <- PredTask(Species  ~ .,iris)
t2 <- PredTask(Species  ~ .,iris[1:20,],"irisSubset")
}
\keyword{classes}
