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
      the formula representing the predictive task. }
    \item{\code{dataSource}:}{Object of class \code{data.frame} or
      \code{character}. This will be used to know the object from which
      data is to be fetched when necessary. }
    \item{\code{taskName}:}{Optional object of class \code{character} containing
      the ID of the predictive task }
    \item{\code{type}:}{Optional object of class \code{character} containing
      the type of the predictive task (it can be "regr", "class" or "ts"). }
    \item{\code{target}:}{Optional object of class \code{character} containing the
     name of the target variable.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "PredTask")}: method used to
      show the contents of a PredTask object. }
	 }
}

\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\linkS4class{Workflow}},
  \code{\linkS4class{EstimationTask}},
 \code{\link{performanceEstimation}}
}
\examples{
showClass("PredTask")
data(iris)
t1 <- PredTask(Species  ~ .,iris)
t2 <- PredTask(Species  ~ .,iris[1:20,],"irisSubset")
}
\keyword{classes}
