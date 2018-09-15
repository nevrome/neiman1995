library(magrittr)

empty <- lapply(
  list.files("simulation_code", full.names = TRUE),
  function(y) {
    message("\n###### ", y, " ######\n")
    source(y)
    rm(list = ls())
  }
)
