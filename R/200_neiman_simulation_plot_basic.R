model_result <- neiman_simulation(8, 2, 800, 1400, 0.001, 0)

x <- standardize_neiman_output(model_result)

plot_by_group <- function(x) {
x %>%
ggplot() +
  geom_area(aes(x = time, y = frequency, fill = variant, group = variant)) +
  geom_line(aes(x = time, y = frequency, group = variant), position = "stack") +
  facet_wrap(~group, nrow = 8) +
  theme_bw() +
  xlab("t") +
  ylab("variants and their occurence in the population [%]") + 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
}

config_matrix <- tibble::tibble(
  N = c(10, 100, 300),
  mi = c(0.001, 0.01, 0.1)
) %>%
  tidyr::complete(N, mi)

models <- pbapply::pblapply(
  1:nrow(config_matrix),
  function(i, config_matrix) {
    neiman_simulation(8, 2, config_matrix$N[i], 1400, config_matrix$mi[i], 0) %>% standardize_neiman_output
  },
  config_matrix
)

plots <- cowplot::plot_grid(
  plotlist = lapply(models, plot_by_group),
  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), 
  ncol = 3,
  nrow = 3
)



