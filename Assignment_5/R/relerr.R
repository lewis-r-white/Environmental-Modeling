#' relerr
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @return relerr


relerr <- function(m,o) {
  
  # error: model - observation
  err <- m-o
  
  #
  meanobs <- mean(o)
  
  # mean error: model - observation
  meanerr <- mean(err)
  
  # relative 
  res <- meanerr/meanobs
  return(res)
}
