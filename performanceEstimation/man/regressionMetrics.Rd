\name{regressionMetrics}
\alias{regressionMetrics}

\title{
Calculate some standard regression evaluation metrics of predictive performance
}
\description{
This function is able to calculate a series of regression evaluation
statistics given two vectors: one with the true target variable values,
and the other with the predicted target variable values. Some of the
metrics may require additional information to be given (see Details section).		
}
\usage{
regressionMetrics(trues, preds,stats = "mse",train.y = NULL)
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

  The regression evaluation statistics calculated by this function belong
to two different groups of measures: absolute and relative. In terms of
absolute error metrics the function includes currently the following:

  "mae": mean absolute error, which is calculated as sum(|t_i - p_i|)/N

  "mse": mean squared error, which is calculated as sum( (t_i - p_i)^2
  )/N

  "rmse": root mean squared error that is calculated as sqrt(mse)

  The remaining measures ("mape", "nmse", "nmae" and "theil") are relative
  measures, the three later
  comparing the performance of the model with a baseline. They are
  unit-less measures with values always greater than 0. In the case of
  "nmse", "nmae" and "theil" the values are expected to be in the interval [0,1]
  though occasionaly scores can overcome 1, which means that your model
  is performing worse than the baseline model. The baseline used in both
  "nmse" and "nmae" is a constant model that always predicts the average
  target variable value, estimated using the values of this variable on
  the training data (data used to obtain the model that generated the
  predictions), which should be 
  provided in the parameter \code{train.y}. The "theil" metric is
  typically used in time series tasks and the used baseline is the last
  observed value of the target variable. The relative error measure
  "mape" does not require a baseline. It simply calculates the average
  percentage difference between the true values and the
  predictions.

  These measures are calculated as follows:

  "mape": sum(|(t_i - p_i) / t_i|)/N

  "nmse": sum( (t_i - p_i)^2 ) / sum( (t_i - AVG(Y))^2 ), where AVG(Y)
  is the average of the values provided in vector \code{train.y}

  "nmae": sum(|t_i - p_i|) / sum(|t_i - AVG(Y)|)

  "theil": sum( (t_i - p_i)^2 ) / sum( (t_i - t_\{i-1\})^2 ), where
  t_\{i-1\} is the last observed value of the target variable

  The user may also indicate the value "all" in the parameter
  \code{stats}. In this case all possible metrics will be
  calculated. This will only include the "nmse", "nmae" and "theil" metrics if
  the value of the \code{train.y} parameter is set, otherwise only the
  other metrics will be returned.
}
\value{
A named vector with the calculated evaluation scores.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\note{
In case you require either "nmse", "nmae" or "theil" to be calculated you must
  supply a vector of numeric values through the parameter
  \code{train.y}, otherwise the function will return an error
  message. These values are required to obtain a fair baseline against
  which your model predictions will be compared to. 
}
\seealso{
 \code{\link{classificationMetrics}}
}
\examples{
\dontrun{
## Calculating several statistics of a regression tree on the Swiss data
data(swiss)
idx <- sample(1:nrow(swiss),as.integer(0.7*nrow(swiss)))
train <- swiss[idx,]
test <- swiss[-idx,]
library(rpart)
model <- rpart(Infant.Mortality ~ .,train)
preds <- predict(model,test)
## by default only mse is calculated
regressionMetrics(test[,'Infant.Mortality'],preds)
## calculate mae and rmse
regressionMetrics(test[,'Infant.Mortality'],preds,stats=c('mae','rmse'))
## calculate all statistics
regressionMetrics(test[,'Infant.Mortality'],preds,stats="all",train.y=train[,'Infant.Mortality'])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
