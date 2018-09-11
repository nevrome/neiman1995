sed <- function(pi, pj) {
  pi <- pi / sum(pi)
  pj <- pj / sum(pj)
  sum((pi - pj)^2)
}

calculate_sed_for_group_drift_simulation_result <- function(pop_devel_sum, sim_run = 1) {

  groups <- pop_devel_sum %>%
    base::split(., .$group)
  
  A <- groups$A %>%
    base::split(., .$time)
  B <- groups$B %>%
    base::split(., .$time)
  
  A <- lapply(A, function(x) {x$individuals_with_variant})
  B <- lapply(B, function(x) {x$individuals_with_variant})
  
  sed_res <- purrr::map2_dbl(A, B, function(a, b) {sed(a, b)})
  
  tibble::tibble(
    t = unique(pop_devel_sum$time),
    sed = sed_res,
    sim_run = sim_run
  )
  
}
