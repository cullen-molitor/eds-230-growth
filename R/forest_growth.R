#' forest_growth
#'
#' @param time (Numeric) The number of years to run the model
#' @param forest_size (Numeric) forest size as measured by carbon (C) in kilograms
#' @param params$exp_growth_rate (Numeric) The exponential growth rate (r) that happens prior to reaching the canopy closure threshold. Measured in kg/year.
#' @param params$lin_growth_rate (Numeric) The linear growth rate (g) that happens after a forest reaches the canopy closure threshold. Measured in kg/year.
#' @param params$carrying_capacity (Numeric) The forest's carrying capacity (K) measured in kilograms
#' @param params$canopy_closure_threshold (Numeric) The forests canopy closure threshold measured in kilograms
#'
#' @return dC_dt the change in forest size (kg) over time (t)
#'   
forest_growth <- function(
    time,
    forest_size,
    params
){
  dC_dt <- case_when(
    forest_size < params$canopy_closure_threshold ~ params$exp_growth_rate * forest_size,
    T ~ params$lin_growth_rate * (1 - (forest_size / params$carrying_capacity))
  )
  return(list(dC_dt))
}
