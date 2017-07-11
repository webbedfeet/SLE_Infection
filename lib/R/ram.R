# Function for computing "reliability-adjusted mortality" following Dimick


#' Reliability-adjusted mortality from Bayesian hierarchical models (following Dimick)
#'
#' @param fit The output from a stan_glmer 
#' @param d data.frame object containing data that was used for fit
#' @param qs Quantiles to be reported
#'
#' @return A tibble with the quantiles for the RAM rates 
#' @export
#'
#' @examples
ram <- function(fit, d, qs = c(0,.1, .25, .5,.75,.9,1)) {
  p <- posterior_linpred(fit)
  avg_p <- mean(p)
  
  ram <- plogis(avg_p + ranef(fit)$hospid[,1])
  out <- as.data.frame(quantile(ram*100, qs)) %>% 
    rownames_to_column('Percentile') %>% 
    setNames(c('Percentile', 'Mortality Rate'))
  return(out)
}