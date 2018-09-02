#### setup settings grid ####

config_matrix <- tibble::tibble(
  N = c(10, 50, 100),
  mi = c(0.001, 0.01, 0.1)
) %>%
  tidyr::complete(N, mi) %>%
  dplyr::mutate(
    model_group = 1:nrow(.)
  ) %>%
  tidyr::uncount(5) %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )



#### run simulation ####

models <- pbapply::pblapply(
  1:nrow(config_matrix),
  function(i, config_matrix) {
    neiman_simulation(8, 2, config_matrix$N[i], 1400, config_matrix$mi[i], 0) %>% standardize_neiman_output %>%
      dplyr::mutate(model_id = config_matrix$model_id[i], model_group = config_matrix$model_group[i])
  },
  config_matrix
)

models_groups <- do.call(rbind, models) %>%
  base::split(.$model_group)



#### create plots ####

plots <- cowplot::plot_grid(
  plotlist = lapply(models_groups, plot_by_group),
  labels = "AUTO", 
  ncol = 3,
  nrow = 3,
  align = "v"
)

plots %>%
  ggsave(
    "static_plots/neiman_general.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
