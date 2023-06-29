options(scipen = 999) #remove scientific notation

#' Calculating Almond Crop Yield Statistics
#' 
#' Computes the almond yield anomaly (in tons/acres) when given a data frame containing monthly climate observations of: minimum and maximum temperature in °C, and precipitation in mm.
#' @param climate_data 
#'
#' @return Almond yield anomalies for each given year
#' 
#'
#' @examples calculate_almond_yield(climate_data = climate_data)
#' @export 
calculate_almond_yield_anomaly <- function(climate_data, 
                                   t_min_coef_1 = -0.015, 
                                   t_min_coef_2 = -0.0046, 
                                   precip_coef_1 = -0.07, 
                                   precip_coef_2 = 0.0043, 
                                   intercept = 0.28) {
  
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
  yield_anomaly <- (t_min_coef_1 * min_temp_month2) + (t_min_coef_2 * min_temp_month2^2) + (precip_coef_1 * precip_month1) + (precip_coef_2 * precip_month1^2) + intercept
  
  return(yield_anomaly)
}
