library(dplyr)
library(yardstick)

plots <- bind_rows(
  lapply(
    list.files(here::here("data/lidar_acc"), full.names = T),
    \(fn) {
      if (grepl('plot_pixel', fn)) {
        return(NULL)
      } else {
        read.csv(fn)
      }
    }
  )
)
ppc <- read.csv("data/lidar_acc/plot_pixel.csv") |>
  left_join(plots |> select(PLOT, lcpri), by = 'PLOT') |>
  dplyr::filter(lcpri != 2)
metrics <- yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::msd, yardstick::rsq_trad)
mean_fia <- mean(ppc$FIA)
metrics(ppc, truth = FIA, estimate = MODEL) |>
  dplyr::mutate(rel_err = .estimate / mean_fia * 100)

slope <- agreeable::gmfr_slope(ppc, MODEL, FIA)
intercept <- agreeable::gmfr_intercept(ppc, MODEL, FIA)

p <- ppc |>
  ggplot() +
  geom_abline(slope = slope, intercept = intercept, color = 'orange', linetype = 'longdash') +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  geom_point(aes(x = MODEL, y = FIA), alpha = 0.4) +
  scale_x_continuous(limits = c(-2, max(c(ppc$MODEL, ppc$FIA)) + 10), expand = c(0.001, 0.001)) +
  scale_y_continuous(limits = c(-2, max(c(ppc$MODEL, ppc$FIA)) + 10), expand = c(0.001, 0.001)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
  ) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1.5) +
  ylab(bquote("FIA AGB (Mg"~ha^-1*")")) +
  xlab(bquote("Predicted AGB (Mg"~ha^-1*")")) +
  coord_equal()

ggsave(here::here("figures/lidar_acc.png"), p,units = 'mm', width = 225, height = 225, dpi = 300)
