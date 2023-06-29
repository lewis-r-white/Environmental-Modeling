#' Photovoltaic Energy Calculation
#'
#' @param area solar panel area (m2)
#' @param yield panel yield (0-1) (default value: 0.2)
#' @param solar_radiation annual average solar radiation (kWh)
#' @param performance_ratio performance ratio (0-1) (default value: 0.75)
#'
#' @return Energy produced (kWh)
#'
#' @examples photovoltaic_energy(area = 100, yield = 20, solar_radiation = 0.25, performance_ratio = 0.7)
#' 
#' @export

photovoltaic_energy <- function(area, yield, solar_radiation = 0.2, performance_ratio = 0.75) {
  
  if (area < 0) {
    stop("It looks like you entered a negative value for area. Please try again and make sure your value for area is input correctly.")
  }
  
  if (yield < 0 | yield > 1) {
    stop("It looks like you entered a value for yield that is outside the accepted range. Pnael yield values are between 0 and 1. Please try again and make sure your value for yeild is input correctly.")
  }
  
  if (solar_radiation < 0) {
    stop("It looks like you entered a negative value for area. Please try again and make sure your value for area is input correctly.")
  }
  
  if (performance_ratio < 0 | performance_ratio > 1) {
    stop("It looks like you entered a value for performance ratio that is outside the accepted range (0 - 1). Please try again and make sure your value for the performance ratio is input correctly.")
  }
  
  energy = area * yield * solar_radiation * performance_ratio
  return(energy)
}