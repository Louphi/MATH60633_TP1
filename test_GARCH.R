library("PerformanceAnalytics")
library("qrmdata")
data("SP500", package = "qrmdata")

f_calc_log_return <- function(prices) {
  return(PerformanceAnalytics::CalculateReturns(prices, method = "log")[-1,])
}

f_garch <- function(theta, x) {
  # Set model parameters
  omega <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]


  # Length of log-returns array
  n <- length(x)

  # Initialize empty array
  sigma_2 <- rep(0, n)

  if (alpha + beta >= 1) {
    return(sigma_2)
  }

  else {
    # Filling array
    # Initialize variance as long term variance
    sigma_2[1] <- omega / (1 - alpha - beta)
    for (i in 2:n) {
      sigma_2[i] <- omega + alpha * x[i-1]^2 + beta * sigma_2[i-1]
    }
    return(sqrt(sigma_2))
  }
}

f_nll <- function(theta, x) {
  n <- length(x)
  sigma_2 <- f_garch(theta, x)
  out <- (n - 1) / 2 + 1 / 2 * sum(log(sigma_2^2) + x^2 / sigma_2^2)
  out
}

sp500_prices <- SP500["2008-01/2012-12"]
sp500_log_rets <- f_calc_log_return(sp500_prices)

theta0 <- c(0.00001, 0.1, 0.8)
sigma_2 <- f_garch(theta, sp500_log_rets)

tmp <- optim(par = theta0, fn = f_nll, x = sp500_log_rets)

tmp$par