sed <- function(pi, pj) {
  pi <- pi / sum(pi)
  pj <- pj / sum(pj)
  sum((pi - pj)^2)
}
