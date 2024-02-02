library(ggplot2)
data <- read.csv(here::here("data/fia_nys_report/FRF_net_flux_by_State.csv")) |>
  dplyr::filter(State == 'New York') |>
  dplyr::filter(Carbon.Pools == 'Total Forest Ecosystem') |>
  tidyr::pivot_longer(dplyr::starts_with('X'), values_to = 'AGB') |>
  dplyr::mutate(name = gsub('X', '', name)) |>
  dplyr::rename(year = name) |>
  dplyr::mutate(year = as.numeric(year))


state_flux <- ggplot(data |> dplyr::filter(Net.Flux.Unit == 'MMT CO2 Eq.')) +
  geom_line(aes(x = year, y = AGB), color = 'darkgreen', linewidth = 1) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1.5) +
  scale_x_continuous(limits = c(1990, 2025),
                     breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(expand = c(0.005, 0.005),
                     limits = c(-28, -22)) +
  ylab(bquote('MMT'~CO[2]~'Eq.'~year^-1*'')) +
  xlab('Year') +
  ggtitle("Annual Total Forest Carbon Flux - NYS") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid = element_blank()
  )
ggsave(here::here('figures/state_flux.png'), dpi = 300, width = 190, height = 120, units = 'mm')
