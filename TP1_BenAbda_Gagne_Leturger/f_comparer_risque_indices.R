f_comparer_risque_indices <- function(VaR_sp500, VaR_ftse100) {
  # This function compares the Value at Risk (VaR) of the S&P 500 and FTSE 100 indices to determine which index is riskier at a T+1 horizon. 
  # It prints a statement indicating which index has higher absolute VaR, hence considered riskier, or if both have equivalent risk levels.
  
  # Arguments:
  #   VaR_sp500: The calculated Value at Risk for the S&P 500 index at the T+1 horizon.
  #   VaR_ftse100: The calculated Value at Risk for the FTSE 100 index at the T+1 horizon.
  
  # Compare the absolute values of VaR for both indices to account for negative VaR values.
  if (abs(VaR_sp500) > abs(VaR_ftse100)) {
    # If S&P 500's VaR is greater, it's considered riskier.
    cat("Le S&P 500 est plus risqué à l'horizon T + 1 avec une VaR à", VaR_sp500, "\nalors que le FTSE 100 a une VaR de", VaR_ftse100)
  } else if (abs(VaR_sp500) < abs(VaR_ftse100)) {
    # If FTSE 100's VaR is greater, it's considered riskier.
    cat("Le FTSE 100 est plus risqué à l'horizon T + 1 avec une VaR à", VaR_ftse100, "\nalors que le S&P 500 a une VaR de", VaR_sp500)
  } else {
    # If both VaRs are equal, both indices are considered to have equivalent risk levels.
    cat("Les deux indices présentent un risque équivalent à l'horizon T + 1 avec une VaR de S&P 500 à", VaR_sp500, "et de FTSE 100 à", VaR_ftse100)
  }
}
