#### setup settings grid ####

config_matrix <- expand.grid(
  k = 2,
  N_g = c(10, 50, 200),
  t_final = 1400,
  mu = 0,
  g = 8,
  mi = c(0, 0.01, 0.1, 0.5 , 1),
  I = NA
) %>%
  tibble::as.tibble() %>%
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
    neiman_simulation(
      config_matrix$k[i], 
      config_matrix$N_g[i], 
      config_matrix$t_final[i], 
      config_matrix$mu[i],
      config_matrix$g[i], 
      config_matrix$mi[i], 
      config_matrix$I[i]
    ) %>% standardize_neiman_output %>%
      dplyr::mutate(
        model_id = config_matrix$model_id[i], 
        model_group = config_matrix$model_group[i],
        region_population_size = config_matrix$N_g[i],
        degree_interregion_interaction = config_matrix$mi[i]
      )
  },
  config_matrix,
  cl = 2
)

models_groups <- do.call(rbind, models) %>%
  base::split(.$model_group)

#### create plots ####

library(ggplot2)
complete_plot <- cowplot::plot_grid(
  plotlist = lapply(models_groups, plot_by_group) %>% 
    matrix(., 3, 5) %>% t %>% c(),
  labels = sapply(
    models_groups, function(x) {
      rps <- x$region_population_size[1]
      cui <- x$degree_interregion_interaction[1]
      paste0(LETTERS[x$model_group[1]], " - ", rps, ", ", cui)
    }
  ) %>% 
    matrix(., 3, 5) %>% t %>% c(),
  label_x = 0,
  hjust = 0,
  label_size = 10,
  ncol = 5,
  nrow = 3,
  align = "hv"
)

complete_plot %>%
  ggsave(
    "static_plots/neiman_general.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
