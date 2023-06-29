#' highflowmetrics
#'
#' Compute percent error between observation and model
#' @param m  model estimates
#' @param o  observations
#' @param month month
#' @param day day
#' @param year  year
#' @param high_flow_months which to use default (May ~  On average has highest flow)
#' @param wts (vector of 4 for annual_max_err, 
#' annual_max_corr, high_month_cor, high_month_err)
#' @param max_err_annual_max
#' @param max_err_high_month
#' @return annual_min_err, annual_min_corr, high_month_cor, high_month_err

compute_highflowmetrics <- function(m, o, month, day, year, 
                                   wy, high_flow_months = 5,
                                   max_err_annual_max = NULL, 
                                   max_err_high_month = NULL,
                                   wts = c(0.25, 0.25, 0.25, 0.25)) {
  
  #create datafram with model predictions, observations, month, day, year, and water year
  flow <- cbind.data.frame(m, o, month, day, year, wy)
  
  # first lets get maximum yearly values
  tmp_annual <- flow %>% group_by(wy) %>% 
    summarize(max_o = max(o), 
              max_m = max(m))
  
  #average annual error for maximum flow
  annual_max_err <- mean(tmp_annual$max_m - tmp_annual$max_o)
  
  #correlation for max obs and max modeled predictions
  annual_max_cor <- cor(tmp_annual$max_m, tmp_annual$max_o)
  
  if (is.null(max_err_annual_max)) {
    max_err_annual_max <- 0.5*mean(tmp_annual$max_o) 
    } #maximum error in the model predictions should not exceed 50% of the average maximum observed value
  
  # now lets get monthly values
  tmp_monthly <- flow %>% 
    group_by(month, year) %>% 
    summarize(model = sum(m), obs = sum(o))
  
  # now extract high flow month
  high <- subset(tmp_monthly, month %in% high_flow_months)
  high_month_err <- mean(high$model - high$obs)
  high_month_cor <- cor(high$model, high$obs)
  
  # if user doesn't specify maximum errors use 50% of mean observed values
  if (is.null(max_err_high_month)){ 
    max_err_high_month <- 0.5 * mean(high$obs)}
  
  #normalize annual and month error on 0-1 scale
  annual_max_err_trans <- max(0, (1 - abs(annual_max_err/max_err_annual_max)))
  high_month_err_trans <- max(0, (1 - abs(high_month_err/max_err_high_month)))
  
  # apply weight (normalize in case they don't sum to 1)
  wts <- wts/sum(wts)
  
  #calculate combined performance metric across annual and monthly error and correlation
  combined <- wts[1]*annual_max_err_trans + 
    wts[2]*annual_max_cor +
    wts[3]*high_month_cor + 
    wts[4]*high_month_err_trans
  
  #return list of each individual error metric and the combined metric
  return(list(annual_max_err = annual_max_err, 
              annual_max_cor = annual_max_cor,
              high_month_err = high_month_err,
              high_month_cor = high_month_cor,
              combined = combined))
}
