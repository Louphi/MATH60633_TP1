f_backtest_var_rolling <- function(y, window_size = 1000, forecast_steps = 1000, level = 0.95) {
  # This function performs a rolling window backtest to predict the Value at Risk (VaR) at the 95% confidence level. 
  # It systematically moves through a time series of returns, using historical data within a specified window to predict VaR for the next period.
  # The function assesses the accuracy of these predictions by counting instances where actual returns fall below the predicted VaR levels (violations).
  
  # Arguments:
  # y: Time series data of returns.
  # window_size: Historical data window size for each prediction.
  # forecast_steps: Number of predictions to perform.
  # level: Confidence level for the VaR calculation.
  
  # Initialize a vector to store VaR predictions for the forecast period
  var_predictions <- numeric(forecast_steps)
  Is_Violation <- numeric(forecast_steps)
  # Initialize a counter for the number of times actual returns fall below the predicted VaR (violations)
  var_violations <- 0
  
  # Loop over the series of returns for the specified number of forecast steps
  for (i in 1:forecast_steps) {
    # Calculate the start and end indices of the current rolling window
    start_index <- i
    end_index <- start_index + window_size - 1
    
    # Ensure the rolling window does not extend beyond the length of the data series
    if (end_index <= length(y)) {
      # Extract the data for the current window
      window_data <- y[start_index:end_index]
      
      # Forecast the next step's VaR using the specified model (e.g., GARCH) and confidence level
      forecast_results <- f_forecast_var(window_data, level)
      
      # Store the forecasted VaR in the prediction vector
      var_predictions[i] <- forecast_results$VaR_Forecast
      
      # Check if the actual return for the next period is less than the forecasted VaR, indicating a violation
      if (y[end_index + 1] < forecast_results$VaR_Forecast) {
        # Increment the violation counter if there's a violation
        var_violations <- var_violations + 1
        Is_Violation[i] <- 1
      }
    } else {
      # Exit the loop if the end of the data series is reached
      break
    }
  }
  
  # Return the predictions and the total number of violations as a list
  list(VaR_Predictions = var_predictions, VaR_Violations = var_violations, Is_Violation = Is_Violation)
}