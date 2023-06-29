options(scipen = 999) #remove scientific notation

#' Calculating Almond Crop Yield Statistics
#' 
#' Computes the maximum, minimum, and average almond yield anomaly (in tons/acres) when given a data frame containing monthly climate observations of: minimum and maximum temperature in °C, and precipitation in mm.
#' @param climate_data 
#'
#' @return Minimum almond yield, mean almond yield, maximum almond yield
#' 
#'
#' @examples calculate_almond_yield(climate_data = climate_data)
#' @export 
calculate_almond_yield <- function(climate_data) {
  
  # Calculate monthly summaries of climate variables
  climate_summary <- climate_data %>%
    group_by(month, year) %>% # Group by month and year
    summarize(total_precip = sum(precip), # Then calculate the summaries
              mean_tmax = mean(tmax_c),
              mean_tmin = mean(tmin_c),
              .groups = "drop") %>% # Drop the grouping levels of the output
    arrange(year) # Order the rows based on the year column
  
  # Extract necessary columns
  precip_month1 <- climate_summary$total_precip[which(climate_summary$month == 1)] # Precipitation for January
  min_temp_month2 <- climate_summary$mean_tmin[which(climate_summary$month == 2)] # Minimum Temperature for Feb
  # These code lines index and extract the total precipitation and temperature values for when the month was January and February (respectively).
  
  # Calculate yield using equation
  yield <- (-0.015 * min_temp_month2) - (0.0046 * min_temp_month2^2) - (0.07 * precip_month1) + (0.0043 * precip_month1^2) + 0.28
  
  # Save and return min, max, and average almond yield
  min_yield = min(yield)
  mean_yield = mean(yield)
  max_yield = max(yield)
  
  # Print the returned values
  print(paste("Minimum Almond Yield =", round(min_yield, 3), "tons/acres"))
  print(paste("Average Almond Yield =", round(mean_yield, 3), "tons/acres"))
  print(paste("Maximum Almond Yield =", round(max_yield, 3), "tons/acres"))
  
}

