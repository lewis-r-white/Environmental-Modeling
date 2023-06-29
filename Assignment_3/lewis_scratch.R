#CALCUALTE ALMOND YIELD

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
  
  # Add year information to the yield anomaly
  yield_with_year <- climate_summary %>%
    filter(month == 2) %>% # Filter to only include February values
    select(year) %>% # Select the year column
    mutate(yield_anomaly = yield_anomaly) # Add the yield anomaly as a new column
  
  return(yield_with_year)
}

print(n = 22, calculate_almond_yield_anomaly(climate_data))




#CALCULATE PROFIT 

calculate_profit <- function (climate_data, 
                              t_min_coef_1 = -0.015, 
                              t_min_coef_2 = -0.0046, 
                              precip_coef_1 = -0.07, 
                              precip_coef_2 = 0.0043, 
                              intercept = 0.28,
                              baseline_profit = 20000, 
                              acres = 300, 
                              price_per_ton = 4000, 
                              cost_per_ton = 3950) {
  
  #calculate yield_anomaly using the almond_script
  yield_anomaly = calculate_almond_yield_anomaly(climate_data, t_min_coef_1, t_min_coef_2, precip_coef_1, precip_coef_2, intercept)
  
  #calculate the total_yield_anomaly of all the acres (in tons)
  total_yield_anomaly = yield_anomaly %>%
    mutate(yield_anomaly = yield_anomaly * acres)
  
  #calculate the expected profit for each year
  expected_profit = total_yield_anomaly %>%
    mutate(expected_profit = baseline_profit + (total_yield_anomaly$yield_anomaly * price_per_ton) - (total_yield_anomaly$yield_anomaly * cost_per_ton))
  
  return(list(expected_profit, mean_profit = mean(expected_profit$expected_profit)))
}

calculate_profit(climate_data)


