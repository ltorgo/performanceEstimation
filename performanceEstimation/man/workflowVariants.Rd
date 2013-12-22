\name{workflowVariants}
\alias{workflowVariants}

\title{
Generate variants of a workflow
}
\description{
The main goal of this function is to facilitate the generation of
different variants of a workflow. The idea is to be able to
supply several possible values for a set of parameters of the workflow,
and then have the function to return a set of \code{\linkS4class{Workflow}} objects,
each consisting of one of the different possible combinations of the
variants. This function finds its use in the context of performance
estimation experiments, where we may actually be interested
in comparing different parameter settings for a workflow.
}
\usage{
workflowVariants(wf,...,varsRootName,as.is=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wf}{
This is the string representing the name of the function of the base
workflow from which variants should be generated.
}
  \item{\dots}{
The function then accepts any number of named arguments, some of which
may have vectors of values. These named arguments  are supposed to be the names of
arguments of the base workflow, and the arguments with sets of values are the
alternatives for the respective parameter that you want to consider in the variants generation (see
examples below).
}
  \item{varsRootName}{
By default the names given to each variant will be formed by
concatenating the base name of the workflow with the terminations: ".v1",
".v2", and so on. This parameter allows you to supply a different base name.
}
  \item{as.is}{
This is a vector of parameter names (defaults to \code{NULL}) that are
not to be used as source for workflow variants. This is useful for workflows
that have parameters that accept as "legal" values sets (e.g. a vector)
and that we do not want the function \code{workflowVariants} to interprete as
source values for generating different workflow variants but instead
being use as they are given.
}

}
\value{
  The result of this function is a list of \code{\linkS4class{Workflow}} objects. Each
  of these objects represents one of the parameter variants of the
  workflow you have supplied.
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\seealso{
\code{\linkS4class{Workflow}},\code{\link{performanceEstimation}}
}
\examples{
\dontrun{
## Generating several variants of the "rpartXse" learner using different
## values of the parameter "se", i.e. different tree pruning levels
library(DMwR)
workflowVariants('rpartXse',se=c(0,0.5,1))
}
}
\keyword{ models }
