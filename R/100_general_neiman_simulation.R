#### simulation function ####

#' neiman_simulation
#'
#' @param k Integer. Number of variants at t = 0
#' @param N_g Integer. Population per group
#' @param t_final Integer. Final timestep
#' @param mu Double. Innovation rate
#' @param g Integer. Number of groups
#' @param mi Double. Degree of intergroup interaction
#' @param I Doublematrix. Intergroup interaction matrix. Default = NA, that means equal interaction
#' 
neiman_simulation <- function(k, N_g, t_final, mu, g, mi, I = NA) {

  # define variables
  groups <- 1:g
  population <- 1:N_g
  variants <- 1:k
  timesteps <- 2:t_final
  if (is.na(I)) {
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
  
  # determine number of variants
  last_variant <- max(do.call(rbind, pop_devel[[1]])$variant)
  
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
    
    # innovation
    if(mu != 0) {
      for (i in seq_along(groups)) {
        innovate_where <- which(sample(c(TRUE, FALSE), nrow(pop_new[[i]]), prob = c(mu, 1 - mu), replace = T))
        new_variants <- seq(last_variant + 1, last_variant + length(innovate_where))
        last_variant <- last_variant + length(innovate_where)
        pop_new[[i]]$variant[innovate_where] <- new_variants
      }
    }

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



#### output preparation ####

standardize_neiman_output <- function(x) {
  
  x %>%
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
  
}
