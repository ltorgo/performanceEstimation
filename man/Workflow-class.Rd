\name{Workflow-class}
\Rdversion{1.1}
\docType{class}
\alias{Workflow}
\alias{Workflow-class}
\alias{show,Workflow-method}
\alias{summary,Workflow-method}

\title{Class "Workflow"}

\description{
  Objects of the class \code{Workflow} represent a
  solution to a predictive task that typically involve learning a
  prediction model using the given training data and then applying it to
  the provided test set. Still, a workflow function may carry out many
  more steps (e.g. special data pre-processing steps, or some form of
  post-processing of the predictions, etc.)
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{Workflow(
  ...)}. This constructor function can generate 3 types of workflows: i)
  standard workflows; ii) time series standard workflows; and iii)
  user-defined workflows. The \code{wf} parameter of the constructor
  controls which type of workflow is created. If the value of this
  parameter is "standardWF" or absent and no parameter \code{type} is
  included, then a workflow of type i) is created. If the value of
  \code{wf} is "timeseriesWF" or absent but the parameter \code{type} is
  set to either "slide" or "grow" then a workflow of type ii) is
  created. In all other cases a user-defined workflow is created which
  means that the function supplied in the parameter \code{wf} must exist
  and follow the input/output protocol of user-defined workflows. This
  protocol implies accepting a formula in the first argument, a training
  data frame in the second and a testing data frame in the third, with
  any other arguments being workflow-specific parameters. Moreover, the
  user-defined workflow must return a \code{\linkS4class{WFoutput}}
  object as result of its execution. See the Examples section for
  illustrations. The constructor can also be given a workflow ID in
  parameter  \code{wfID}. Finally, the constructor also accepts a
  parameter \code{deps} with a valid value for the \code{deps} class
  slot. 
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{An optional string containing an
      internal name of the workflow (a kind of ID)}
    \item{\code{func}:}{A character string containing the name of the R
      function that implements the workflow.}
    \item{\code{pars}:}{A named list containing the parameters and
      respective values to be used when calling the workflow (defaulting
      to the empty list).}
    \item{\code{deps}:}{An optional named list with components
      "packages" and "scripts" each containing a vector of names of the
      required packages and scripts, respectively, for the workflow to be
      executable. }

  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Workflow")}: method used to
      show the contents of a \code{Workflow} object. }

  \item{summary}{\code{signature(object = "Workflow")}: method used to
    obtain a summary of a \code{Workflow} object. }
  }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\linkS4class{PredTask}},
  \code{\linkS4class{EstimationTask}},
  \code{\link{runWorkflow}}
}
\examples{
showClass("Workflow")

## A simple standard workflow using a default svm
Workflow(learner="svm")

## Creating a standardWF 
Workflow(learner="randomForest",learner.pars=list(ntree=420),wfID="rf420")

## Another one
Workflow(learner="svm",
         pre=c("centralImp","scale"),
         post="onlyPos",
         deps=list(packages=c("e1071"),scripts=c()))

## Another one
Workflow(learner="rpart",.fullOutput=TRUE)

## A user-defined workflow
myWF <- function(form,train,test,wL=0.5,...) {
    require(rpart,quietly=TRUE)
    ml <- lm(form,train)
    mr <- rpart(form,train)
    pl <- predict(ml,test)
    pr <- predict(mr,test)
    ps <- wL*pl+(1-wL)*pr
    WFoutput(rownames(test),responseValues(form,test),ps)
}

wu <- Workflow(wf="myWF",wL=0.6)

}
\keyword{classes}
