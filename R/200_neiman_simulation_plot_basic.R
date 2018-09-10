#### specialized plot function ####

plot_by_group <- function(x) {
x %>% 
  dplyr::filter(
    idea == 1
  ) %>%
  ggplot() +
  # geom_area(aes(x = timestep, y = proportion, fill = idea, group = idea)) +
  geom_line(
    aes(x = timestep, y = proportion, color = as.factor(model_id), group = model_id),
    size = 0.2
  ) +
  facet_wrap(~region, nrow = 8) +
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
