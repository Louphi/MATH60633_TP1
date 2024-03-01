f_plot_combined_returns_and_var <- function(filepath, rets_sp500, sp500_predictions, rets_ftse100, ftse100_predictions) {
  # This function generates and saves a PNG file containing plots of log returns and Value at Risk (VaR) backtests 
  # for both the S&P 500 and FTSE 100 indices. It uses base R plotting functions to create two subplots in a single 
  # output file, showcasing how the actual returns compare to the VaR predictions over time.
  
  # Arguments:
  #   filepath: The path and filename where the PNG file will be saved.
  #   rets_sp500: Time series (xts object) of S&P 500 log returns.
  #   sp500_predictions: Time series (xts object) of VaR predictions for the S&P 500.
  #   rets_ftse100: Time series (xts object) of FTSE 100 log returns.
  #   ftse100_predictions: Time series (xts object) of VaR predictions for the FTSE 100.
  
  # Open a PNG device to save the plot to the specified filepath with desired dimensions.
  png(filepath, width = 800, height = 1200) # Adjust size as needed for clarity and readability.
  
  # Configure the plotting area to display two plots vertically (2 rows, 1 column).
  par(mfrow = c(2, 1))
  
  # First plot for S&P 500:
  # Convert 'xts' objects to regular vectors for plotting using base R functions.
  # Plot the log returns for a selected time range with blue horizontal lines.
  plot(index(rets_sp500[1001:2000]), coredata(rets_sp500[1001:2000]), type = "h", lwd = 0.5, col = "blue",
       main = "S&P 500 Returns and VaR Backtest", xlab = "Time", ylab = "Log Returns & VaR")
  # Overlay VaR predictions on the same plot with red lines.
  lines(index(sp500_predictions), coredata(sp500_predictions), col = "red", lwd = 2)
  
  # Add a legend to the plot for clarity.
  legend("topright", legend = c("Log Returns", "VaR Backtest"), col = c("blue", "red"), lty = c(1, 1), cex = 0.8)
  
  # Second plot for FTSE 100: Follows a similar approach as the first plot.
  plot(index(rets_ftse100[1001:2000]), coredata(rets_ftse100[1001:2000]), type = "h", lwd = 0.5, col = "blue",
       main = "FTSE 100 Returns and VaR Backtest", xlab = "Time", ylab = "Log Returns & VaR")
  lines(index(ftse100_predictions), coredata(ftse100_predictions), col = "red", lwd = 2)
  
  # Add a legend to the FTSE 100 plot as well.
  legend("topright", legend = c("Log Returns", "VaR Backtest"), col = c("blue", "red"), lty = c(1, 1), cex = 0.8)
  
  # Close the PNG device, finalizing the file output.
  dev.off()
}