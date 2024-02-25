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
                     y = as.vector(y),
                     grad=NULL)
  
  # Get estimated parameters
  theta <- opt$par
  
  # Recompute the conditional variance
  sig2 <- f_ht(theta, y)
  
  # Compute the next-day ahead VaR for the Normal model
  VaR <- qnorm((1-level),
               mean =0,
               sd = sqrt(tail(sig2, n=1)))
  
  # Store VaR forecast, cond. variance and GARCH Param's
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