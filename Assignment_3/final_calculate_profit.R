source(here("Assignment_3", "final_calculate_almond_yield_anomaly.R"))


#' Calculate Profit
#'
#' @param climate_data a data frame containing daily climate observations of: minimum and maximum temperature in °C, and precipitation in mm 
#' @param t_min_coef_1 Coefficient for the average minimum temperature (in °C) in February of the harvest year
#' @param t_min_coef_2 Coefficient for the average minimum temperature (in °C) squared in February of the harvest year
#' @param precip_coef_1 Coefficient for the total precipitation (in mm) in January of harvest year.
#' @param intercept The intercept of the equation used to estimate almond yield anomaly
#' @param cost_per_ton Coefficient for the total precipitation (in mm) squared in January of harvest year.
#' @param baseline_profit The expected profit (in US dollars) given a typical almond yield (i.e. no anomaly)
#' @param acres The number of acres of almond trees (in acres)
#' @param price_per_ton The selling price of a ton of almonds (dollars/ton)
#' @param cost_per_ton The cost of producing a ton of almonds (dollars/ton)
#' 
#'
#' @return
#' @export
#'
#' @examples calculate_profit(climate_data = climate_data, baseline_profit = 19000, acres = 250)
#' 

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
  total_yield_anomaly = yield_anomaly * acres
  
  #calculate the expected profit
  expected_profit = baseline_profit + (total_yield_anomaly * price_per_ton) - (total_yield_anomaly * cost_per_ton)
  
  return(list(exp_profit = expected_profit, mean_profit = mean(expected_profit)))
}


