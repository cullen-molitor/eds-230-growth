#' forest_growth
#'
#' @param time
#' @param population
#' @param params$exp_growth_rate r
#' @param params$lin_growth_rate g
#' @param params$forest_size C
#' @param params$carrying_capacity K
#' @param params$canopy_closure_threshold 
#'
#' @return dC_dt
#'   
forest_growth <- function(
    time,
    population,
    params
){
  dC_dt = case_when(
    params$forest_size < params$canopy_closure_threshold ~ params$exp_growth_rate * params$forest_size,
    T ~ params$lin_growth_rate * (1 - (params$forest_size / params$carrying_capacity))
  )
  return(list(dC_dt))
}
