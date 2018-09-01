plot_by_group <- function(x) {
x %>% 
  dplyr::filter(
    variant == 1
  ) %>%
  ggplot() +
  # geom_area(aes(x = time, y = frequency, fill = variant, group = variant)) +
  geom_line(
    aes(x = time, y = frequency, color = as.factor(model_id), group = model_id),
    size = 0.2
  ) +
  facet_wrap(~group, nrow = 8) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(1.4,0.2,0.2,0), "lines")
  ) +
  guides(color = FALSE) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%")
  ) +
  scale_x_continuous(
    breaks = seq(0, 1400, 200), 
    limits = c(0, 1400)
  ) +
  ggthemes::scale_colour_colorblind()
}

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

