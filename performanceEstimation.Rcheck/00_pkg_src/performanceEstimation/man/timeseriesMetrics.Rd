\name{timeseriesMetrics}
\alias{timeseriesMetrics}

\title{
Calculate some standard evaluation metrics for time series forecasting tasks
}
\description{
This function is able to calculate a series of numeric time series evaluation
statistics given two vectors: one with the true target variable values,
and the other with the predicted target variable values. Some of the
metrics may require additional information to be given (see Details section).	
}
\usage{
timeseriesMetrics(trues, preds,stats = "mse",train.y = NULL)
}

\arguments{
  \item{trues}{
A numeric vector with the true values of the target variable.
}
  \item{preds}{
A numeric vector with the predicted values of the target variable.
}
  \item{stats}{
A vector with the names of the evaluation statistics to
  calculate (see Details section). If none is indicated it will
  calculate the mean squared error ("mse"). It is also possible to
  indicate the value "all", and all available metrics will be calculated.
}
\item{train.y}{
  An optional numeric vector with the values
  of the target variable on the set of data used to obtain the model
  whose performance is being tested.
}
}
\details{
In the
  following description of the currently available metrics we denote the
  vector of true target variable values as t, the vector of predictions
  by p, while N denotes the size of these two vectors, i.e. the number
  of test cases.

  The time series evaluation statistics calculated by this function belong
to two different groups of measures: absolute and relative. In terms of
absolute error metrics the function includes currently the following:

  "mae": mean absolute error, which is calculated as sum(|t_i - p_i|)/N

  "mse": mean squared error, which is calculated as sum( (t_i - p_i)^2
  )/N

  "rmse": root mean squared error that is calculated as sqrt(mse)

  The remaining measures ("mape", "theil", "nmse" and "nmae") are relative
  measures, the last three
  comparing the performance of the model with a baseline. They are
  unit-less measures with values always greater than 0. In the case of
  "theil", "nmse" and "nmae" the values are expected to be in the interval [0,1]
  though occasionaly scores can overcome 1, which means that your model
  is performing worse than the baseline model. The baseline used for
  "nmse" and "nmae" is a constant model that always predicts the average
  target variable value, estimated using the values of this variable on
  the training data (data used to obtain the model that generated the
  predictions), which should be 
  provided in the parameter \code{train.y}. The baseline of the "theil"
  metric is the model that forecasts the next value of the time series
  as being the current value. The last value of the training period
  should be given through the argument \code{train.y} as it will be used
  to make the baseline forecast for the first test case.
  The relative error measure
  "mape" does not require a baseline. It simply calculates the average
  percentage difference between the true values and the
  predictions.

  These measures are calculated as follows:

  "mape": sum(|(t_i - p_i) / t_i|)/N

  "theil": sum( (t_i - p_i)^2 ) / sum( (t_i - t_(i-1))^2 ), where
  t_(i-1) is the true value of the series at time i-1

  "nmse": sum( (t_i - p_i)^2 ) / sum( (t_i - AVG(Y))^2 ), where AVG(Y)
  is the average of the values provided in vector \code{train.y}

  "nmae": sum(|t_i - p_i|) / sum(|t_i - AVG(Y)|)

  The user may also indicate the value "all" in the parameter
  \code{stats}. In this case all possible metrics will be
  calculated. This will only include the "theil", "nmse" and "nmae" metrics if
  the value of the \code{train.y} parameter is set, otherwise only the
  other metrics will be returned.
}
\value{
A named vector with the calculated evaluation scores.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\note{
1. In case you require either "nmse" or "nmae" to be calculated you must
  supply a vector of numeric values through the parameter
  \code{train.y}, otherwise the function will return an error
  message. The average of the values in this vector will be used as the baseline
  model against which your model predictions will be compared to.

  2. If you want to calculate the value of "theil" you must supply the
  last observed value of the time series on the training period through
  parameter \code{train.y}
}
\seealso{
 \code{\link{classificationMetrics}}, \code{\link{regressionMetrics}}
}
\examples{
## A few example uses of the function
tr <- rnorm(1000)
true <- rnorm(50)
preds <- rnorm(50)
timeseriesMetrics(true,preds)
timeseriesMetrics(true,preds,stats="all")
timeseriesMetrics(true,preds,stats='theil',train.y=tr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
