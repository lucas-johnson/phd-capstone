library(ggplot2)
library(ggspatial)
map_theme <- theme_minimal() +
  theme(
    legend.text=element_text(size=10),
    legend.title = element_text(size=12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

hwf_plus <- sf::st_read(here::here("data/hwf_plus.gpkg"), quiet = TRUE)
state_shoreline <- labrador.client::get_region('state_shoreline') |>
  sf::st_transform(sf::st_crs(hwf_plus))

inset <- ggplot() +
  geom_sf(data = state_shoreline, color = 'black', linewidth = 1, fill = 'white') +
  geom_sf(data = hwf_plus, color = 'black', linewidth = 1, fill = 'white') +
  ggspatial::annotation_scale(
    location = "bl",
    text_cex = 1.5
  ) +
  ggspatial::annotation_north_arrow(
    which_north = 'true',
    style = ggspatial::north_arrow_orienteering(
      fill = c("black", "black")
    ),
    location = 'tr'
  ) +
  map_theme


ggsave(here::here('figures/hwf_inset.png'), inset, width = 300, height = 300, units = 'mm', dpi = 300)
