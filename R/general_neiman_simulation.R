neiman_simulation <- function(g, k, N_g, t_final, mi, mu, I = matrix()) {

  # define variables
  groups <- 1:g
  population <- 1:N_g
  variants <- 1:k
  timesteps <- 2:t_final
  if (any(is.na(I))) {
    I <- matrix(
      rep(1, g*g), g, g
    )
    diag(I) <- 0
  }
  
  # create starting populations
  pop0 <- lapply(
    groups, function(group, N, k) {
      tibble::tibble(
        time = as.integer(0),
        individual = population,
        variant = rep_len(variants, max(population)),
        group = group
      )
    },
    k, population
  )
  
  # create development list
  pop_devel <- list()
  pop_devel[[1]] <- pop0
  
  # simulation loop
  for (p1 in timesteps) {
    
    # new timestep list
    pop_old <- pop_devel[[p1 - 1]]
    pop_new <- pop_old
    
    # adjust time in new timestep list
    pop_new <- lapply(
      pop_new, function(x, p1) {
        x$time <- p1 - 1
        return(x)
      },
      p1
    )
    
    # intragroup learning
    pop_new <- lapply(
      pop_new, function(x) {
        x$variant <- sample(x$variant, length(x$variant), replace = T)
        return(x)
      }
    )
    
    # intergroup learning
    pop_new <- lapply(
      groups, function(i, pop_new, pop_old, mi, I, groups) {
        exchange_where <- which(sample(c(TRUE, FALSE), nrow(pop_new[[i]]), prob = c(mi, 1 - mi), replace = T))
        exchange_with <- sample(groups, length(exchange_where), prob = I[,i], replace = T)
        pop_new[[i]]$variant[exchange_where] <- unlist(sapply(
          seq_along(exchange_where),
          function(j, pop_old, exchange_with, exchange_where) {
            v <- pop_old[[exchange_with[j]]]$variant
            return(v[exchange_where[j]])
          },
          pop_old, exchange_with, exchange_where
        ))
        return(pop_new[[i]])
      },
      pop_new, pop_old, mi, I, groups
    )
    
    pop_devel[[p1]] <- pop_new
  }
  
  # transform to data.frame
  pop_devel_time_dfs <- lapply(
    pop_devel, function(x) {
      do.call(rbind, x)
    }
  )
  pop_devel_df <- do.call(rbind, pop_devel_time_dfs)
  
  return(pop_devel_df)
}

model_result <- neiman_simulation(8, 5, 20, 1400, 0.1, 0.01)

pop_devel_sum <- model_result %>%
  dplyr::group_by(
    time, variant, group
  ) %>%
  dplyr::summarise(
    number = n()
  ) %>%
  dplyr::ungroup() %>%
  # calculate proportion
  dplyr::group_by(
    time, group
  ) %>%
  dplyr::mutate(
    frequency = number/sum(number)
  ) %>%
  dplyr::ungroup() %>%
  # that's just to fill gaps in the area plot
  tidyr::complete(
    time,
    variant,
    group,
    fill = list(number = as.integer(0), frequency = as.double(0))
  )

