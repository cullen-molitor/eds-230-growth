#' forest_growth
#'
#' @param time
#' @param forest_size C
#' @param params$exp_growth_rate r
#' @param params$lin_growth_rate g
#' @param params$carrying_capacity K
#' @param params$canopy_closure_threshold 
#'
#' @return dC_dt
#'   
forest_growth <- function(
    time,
    forest_size,
    params
){
  dC_dt = case_when(
    forest_size < params$canopy_closure_threshold ~ params$exp_growth_rate * forest_size,
    T ~ params$lin_growth_rate * (1 - (forest_size / params$carrying_capacity))
  )
  return(list(dC_dt))
}
