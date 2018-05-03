multi_group_matrizes <- function(Ne, t, Mk, mi_3) {
  
  k <- 1
  amount_groups <- nrow(Mk(k, mi_3))

  group_matrix <- matrix(data = rep(0, amount_groups^2), nrow = amount_groups, ncol = amount_groups)

  M <- Mk
    
  U <- group_matrix
  diag(U) <- 1/Ne
  
  matrix_time_list <- list()
  
  for (pk in 1:t) {
    matrix_time_list[[pk]] <- M(k, mi_3)  %*% U  %*% t(M(k, mi_3)) 
  }
  
  V <- Reduce(`+`, matrix_time_list) 
  
  # matrix of squared euclidian distance
  mosed <- group_matrix
  for (p1 in 1:nrow(V)) {
    for (p2 in 1:ncol(V)) {
      mosed[p1, p2] <- V[p1, p1] + V[p2, p2] - 2 * V[p1, p2]
    }
  }
  
  # mean squared distance of a group 
  msd <- group_matrix
  for (p3 in 1:nrow(V)) {
    diag(msd)[p3] <- sum(mosed[p3,] / (amount_groups - 1))  
  }
  
  list(
    longM = reshape2::melt(M(k, mi_3)),
    longMt = reshape2::melt(t(M(k, mi_3))),
    longU = reshape2::melt(U),
    longV = reshape2::melt(V),
    longmosed = reshape2::melt(mosed),
    longmsd = reshape2::melt(msd)
  )

}
