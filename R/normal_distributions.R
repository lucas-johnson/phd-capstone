library(ggplot2)
library(patchwork)
n <- 1e6
dbh_sample <- rnorm(n = n, mean = -0.004, sd = 0.55)
boleht_sample <- rnorm(n = n, mean = -0.050, sd = 1.52)
cull_sample <- rnorm(n = n, mean = 0.1, sd = 3.5)
plot_location_sample <- rnorm(n = n, mean = 0, sd = 7.05)


map_theme <- function() {
  theme_minimal() +
    theme(
      legend.text=element_text(size=10),
      legend.title = element_text(size=12),
      plot.title = element_text(size = 14),
      strip.text = element_text(size=12),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      plot.background = element_rect(color = 'white')
    )
}

density_plot <- function(data, units, title) {
  ggplot() +
    geom_density(aes(x = data, y = after_stat(scaled))) +
    map_theme() +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linewidth = 1) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, linewidth = 1) +
    ylab('Density (maximum = 1)') +
    xlab(glue::glue("Error ({units})")) +
    theme(panel.grid = element_blank()) +
    ggtitle(title)


}

dbh_plot <- density_plot(dbh_sample, 'cm', 'DBH')
boleht_plot <- density_plot(boleht_sample, 'm', 'Bole height') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())
cull_plot <- density_plot(cull_sample, '%', " % Rotten/missing") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())
loc_plot <- density_plot(plot_location_sample, 'm', "Plot location") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

combo <- (dbh_plot | boleht_plot | cull_plot | loc_plot)

ggsave(here::here("figures/normal_dists.png"), combo, dpi = 300,
       width = 400, height = 100, units = 'mm')
