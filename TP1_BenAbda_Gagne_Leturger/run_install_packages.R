## AUTOMATICALLY LOAD AND INSTALL THE USED PACKAGES

libraries <- c(
  # Libraries for data analysis
  'PerformanceAnalytics',   # Library for portfolio performance analysis
  'xts',                    # Time series management
  # Libraries for file and path management
  'here'                    # Simplifies file paths by finding the project's root directory
)

# Loop to check, install, and load each library
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

# Remove the variables libraries and lib
rm(libraries, lib)