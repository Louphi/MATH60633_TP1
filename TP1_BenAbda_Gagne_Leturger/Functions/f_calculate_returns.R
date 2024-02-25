f_calculate_returns <- function(y, method="log") {
  # This function calculates the returns from a series of prices using either logarithmic or simple returns method.
  # It leverages the CalculateReturns function from the PerformanceAnalytics package, 
  # excluding the first observation which typically is NA due to calculation.
  
  # Arguments:
  #   y: A numeric vector or a time series object of prices.
  #   method: Method for calculating returns. Defaults to "log" for logarithmic returns. 
  #           Alternative is "simple" for simple returns.
  
  PerformanceAnalytics::CalculateReturns(y, method = method)[-1,]
}