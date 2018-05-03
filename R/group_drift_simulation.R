group_drift_simulation <- function(k, N, t_final, mi) {

  population <- 1:N
  variants <- 1:k
  timesteps <- 2:t_final
  
  popA0 <- tibble::tibble(
    time = as.integer(0),
    individual = 1:N,
    variant = rep_len(1:k, N),
    group = "A"
  )
  popB0 <- tibble::tibble(
    time = as.integer(0),
    individual = 1:N,
    variant = rep_len(1:k, N),
    group = "B"
  )
  
  popA_devel <- list()
  popB_devel <- list()
  popA_devel[[1]] <- popA0
  popB_devel[[1]] <- popB0
  
  for (p1 in timesteps) {
    popA_new <- popA_devel[[p1 - 1]]
    popB_new <- popB_devel[[p1 - 1]]
    
    popA_new$time <- p1 - 1
    popB_new$time <- p1 - 1

    exchange_here <- sample(
      c(TRUE, FALSE), 
      length(popA_new$variant), 
      prob = c(mi, 1 - mi), 
      replace = T
    )
          
    popA_old_variant <- popA_new$variant
    # in group A
    popA_new$variant <- sample(popA_new$variant, length(popA_new$variant), replace = T)
    # ex group A
    popA_new$variant[exchange_here] <- sample(popB_new$variant, sum(exchange_here))
    # in group B
    popB_new$variant <- sample(popB_new$variant, length(popB_new$variant), replace = T)
    # ex group B
    popB_new$variant[exchange_here] <- sample(popA_old_variant, sum(exchange_here))
    
    popA_devel[[p1]] <- popA_new
    popB_devel[[p1]] <- popB_new
  }
  
  pop_devel <- append(popA_devel, popB_devel)
  
  pop_devel_df <- do.call(rbind, pop_devel)
  
  pop_devel_sum <- pop_devel_df %>%
    dplyr::group_by(
      time, variant, group
    ) %>%
    dplyr::summarise(
      individuals_with_variant = n()
    ) %>%
    dplyr::ungroup() %>%
    # that's just to fil gaps in the area plot
    tidyr::complete(
      time, 
      variant, 
      group,
      fill = list(individuals_with_variant = as.integer(0))
    )
  
  return(pop_devel_sum)
}
