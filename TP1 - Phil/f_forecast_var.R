library("PerformanceAnalytics")
library("xts")

f_calculate_returns <- function(y, method="log") {
  PerformanceAnalytics::CalculateReturns(y, method = method)[-1,]
}

f_forecast_var <- function(y, level) {
  ### Compute the VaR forecast of a GARCH(1,1) model with Normal errors at the desired risk level
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  #   level : [scalar] risk level (e.g. 0.95 for a VaR at the 95# risk level)
  #  OUTPUTS
  #   VaR   : [scalar] VaR forecast 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  #   theta : [vector] GARCH parameters
  #  NOTE
  #   o the estimation is done by maximum likelihood
  
  # Fit a GARCH(1,1) model with Normal errors
  # Starting values and bounds
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  LB     <- c(0, 0, 0)
  # Stationarity condition
  A      <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, -1, -1), #UTILISER EPSILON
                   nrow = 4,
                   byrow = TRUE)
  b <- c(LB, -1) #UTILISER EPSILON
  
  # Run the optimization
  opt <- constrOptim(theta = theta0,
                     f = f_nll,
                     ui = A,
                     ci = b,
                     control = list(trace=0),
                     y = y,
                     grad=NULL)

  # Get estimated parameters
  theta <- opt$par

  # Recompute the conditional variance
  sig2 <- f_ht(theta, y)

  # Compute the next-day ahead VaR for the Normal model
  VaR <- qnorm((1-level),
               mean =0,
               sd = sqrt(tail(sig2, n=1)))
  
  out <- list(VaR_Forecast = VaR, 
              ConditionalVariances = sig2, 
              GARCH_param = theta)
  
  out
}

f_nll <- function(theta, y) {
  ### Fonction which computes the negative log likelihood value 
  ### of a GARCH model with Normal errors
  #  INPUTS
  #   theta  : [vector] of parameters
  #   y      : [vector] (T x 1) of observations
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  
  T <- length(y)
  
  # Compute the conditional variance of a GARCH(1,1) model
  sig2 <- f_ht(theta, y)
  
  # Consider the T values
  sig2 <- sig2[1:T]

  # Compute the loglikelihood
  ll <- - (T - 1) / 2 - 1 / 2 * sum(log(sig2) + y^2 / sig2)

  # Output the negative value
  nll <- -ll
  
  nll
}

f_ht <- function(theta, y)  {
  ### Function which computes the vector of conditional variance
  #  INPUTS
  #   x0 : [vector] (3 x 1)
  #   y     : [vector] (T x 1) log-returns
  #  OUTPUTS 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  
  # Extract the parameters
  a0 <- theta[1]
  a1 <- theta[2]
  b1 <- theta[3]
  
  T <- length(y)
  
  # Initialize the conditional variances
  sig2 <- rep(NA, T + 1)
  
  # Start with unconditional variances
  sig2[1] <- a0 / (1 - a1 - b1)
  
  # Compute conditional variance at each step
  for (i in 1:T+1) {
    sig2[i] <- a0 + a1 * y[i-1]^2 + b1 * sig2[i-1]
  }
  sig2
}

# Function to backtest on a rolling window basis and predict VaR at 95% level
f_backtest_var_rolling <- function(y, window_size = 1000, forecast_steps = 1000, level = 0.95) {

  # Initialize a vector to store VaR predictions
  var_predictions <- numeric(forecast_steps)

  # Loop through the time series data for the specified forecast steps
  for (i in 1:forecast_steps) {
    # Determine the start and end of the current window
    start_index <- i
    end_index <- start_index + window_size - 1

    # Ensure the end index does not exceed the length of the time series
    if (end_index <= length(y)) {
      # Extract the window of data
      window_data <- y[start_index:end_index]

      # Fit the GARCH model and forecast the next step's VaR
      forecast_results <- f_forecast_var(window_data, level)

      # Store the forecasted VaR
      var_predictions[i] <- forecast_results$VaR_Forecast
    } else {
      # Break the loop if the end index exceeds the time series length
      break
    }
  }

  # Return the VaR predictions
  return(var_predictions)
}


library("qrmdata")
library("xts")
data("SP500", package = "qrmdata")
prices <- SP500["2005-01/2015-12"]
returns <- f_calculate_returns(prices)[1:2000]

var <- f_ht(c(0.00001, 0.1, 0.8), returns)
var <- xts(var[1:2000], order.by = index(returns))
png("plot1.png", width=800, height=600)
plot(returns, type="h")
lines(sqrt(var), type="l", col="blue", lwd = 5)
dev.off()

var <- f_backtest_var_rolling(returns)
var <- xts(var, order.by = index(returns[1001:2000]))

png("plot.png", width=800, height=600)
plot(returns[1001:2000], type = "h")
lines(var, type="l")
dev.off()








