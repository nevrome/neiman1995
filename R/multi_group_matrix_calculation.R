multi_group_matrizes <- function(Ne, t) {
  
  amount_groups <- 5

  group_matrix <- matrix(data = rep(0, amount_groups^2), nrow = amount_groups, ncol = amount_groups)
  
  U <- group_matrix
  diag(U) <- 1/Ne
  
  M <- function(k) {
    M <- group_matrix
    #M[] <- 0.2
    M[1,] <- c(0.4, 0.0, 0.2, 0.0, 0.4)
    M[2,] <- c(0.0, 0.4, 0.2, 0.4, 0.0)
    M[3,] <- c(0.2, 0.2, 0.2, 0.2, 0.2)
    M[4,] <- c(0.2, 0.1, 0.3, 0.2, 0.2)
    M[5,] <- c(0.2, 0.3, 0.1, 0.2, 0.2)
    M
  }
  
  matrix_time_list <- list()
  
  for (k in 1:t) {
    matrix_time_list[[k]] <- M(k)  %*% U  %*% t(M(k)) 
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
    longM = reshape2::melt(M(k)),
    longMt = reshape2::melt(t(M(k))),
    longU = reshape2::melt(U),
    longV = reshape2::melt(V),
    longmosed = reshape2::melt(mosed),
    longmsd = reshape2::melt(msd)
  )

}
