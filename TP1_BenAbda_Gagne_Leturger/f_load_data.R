f_load_data <- function() {
  # Loads financial indices prices from an R data file. Uses the 'here' package to build a path to "indices.rda" 
  # located in "Data/Raw Data".
  
  # Load the 'prices' data from "indices.rda"
  load(here("Data/Raw Data", "indices.rda"))
  
  # Return the loaded 'prices' object
  prices
}