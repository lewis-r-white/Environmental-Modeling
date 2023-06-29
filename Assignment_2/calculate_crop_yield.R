options(scipen = 999)

calculate_crop_yield <- function(climate_data) {
  
  #calculate monthly summaries of climate variables
  climate_summary <- climate_data %>%
    group_by(month, year) %>%
    summarize(total_precip = sum(precip),
              mean_tmax = mean(tmax_c),
              mean_tmin = mean(tmin_c),
              .groups = "drop") %>%
    arrange(year)
  
  # Extract necessary columns
  precip_month1 <- climate_summary$total_precip[which(climate_summary$month == 1)] 
  min_temp_month2 <- climate_summary$mean_tmin[which(climate_summary$month == 2)]
  
  # Calculate yield using equation
  yield <- -0.015 * min_temp_month2 - 0.0046 * min_temp_month2^2 - 0.07 * precip_month1 + 0.0043 * precip_month1^2 + 0.28
  
  min_yield = min(yield)
  mean_yield = mean(yield)
  max_yield = max(yield)
  
  print(paste("Minimum Yield =", min_yield))
  print(paste("Average Yield =", mean_yield))
  print(paste("Maximum Yield =", max_yield))
  
}

calculate_crop_yield(climate_data = climate_data)



