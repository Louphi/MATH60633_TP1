
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
   ## !!! FIXME !!!
  LB <- c(0 , 0 , 0)
  ### 
    
  # Stationarity condition
    
    ### !!! FIXME !!!
  A      <- c(0,-1,-1)
  b      <- -1
  ###
  ## décomposer l égalité en 2 inégalités
  
  # Run the optimization
  ## !!! FIXME !!! 
  fit <- constrOptim(theta0, f = f_nll(theta0,y),grad = NULL, ui = matrix(c(A,1,0,0),nrow = 2, ncol = 3), ci = matrix(c(b,0),nrow = 2, ncol = 1), y = y)
  theta <- fit$par 
  
  ###
  
  # Recompute the conditional variance
  sig2 <- f_ht(theta, y)
  
  # Compute the next-day ahead VaR for the Normal model
   ## !!! FIXME !!! 
  ### T+1 ???? 
  VaR <- qnorm((1 - level)*sqrt(sig2[T + 1]))
  ###  
  
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
   ## !!! FIXME !!! 
  ll <- sum(log(1/sqrt(2*pi*var(sig2))) - (sig2[2:T] - (theta[1] + theta[2]*(y[1:T-1])^2 + theta[3] * sig2[1:T-1])))  
  ###
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
  ### !!! FIXME !!!
  
  for (t in 2:T+1) {
    sig2[t] <- a0 + a1 * y[t-1]^2 + b1 * sig2[t-1]
  }
  ###
  
  sig2
}

