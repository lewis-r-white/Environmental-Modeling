#' nse
#'
#' Compute NSE between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @return nse


nse <- function(m, o) {

  # error: model - observation
  err <- m - o
  
  # mean observation value
  meanobs <- mean(o)
  
  # mean squared error
  mse <- sum(err*err)
  
  # variance in mean observation
  ovar <- sum((o-meanobs)*(o-meanobs))
  
  # Nash Sutcliffe Efficiency 
  nse <- 1.0-mse/ovar

  return(nse)
  
}

