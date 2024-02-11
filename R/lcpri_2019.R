library(terra)
library(raster)
library(ggplot2)
library(ggspatial)

raster_to_table <- function(raster, cols) {
  r_data <- as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  names(r_data) <- c("x", "y", cols)
  return(r_data)
}
state_shoreline <- labrador.client::get_region("state_shoreline") |>
  sf::st_transform('EPSG:26918')

future::plan('multisession')
lcpri_2019 <- terra::rast(labrador.client::get_cafri_data(
  region = "state_shoreline", product = "lcpri_2019"
))

lcpri_2019 <- lcpri_2019 |>
  terra::project('EPSG:26918', method = 'near') |>
  terra::aggregate(lcpri_2019, fact = 3, fun = 'modal', na.rm = TRUE) |>
  terra::crop(state_shoreline) |>
  terra::mask(state_shoreline)
lcpri_data <- raster_to_table(lcpri_2019, "LCPRI")

class_colors <- data.frame(
  numeral = c(1:6, 8),
  classes = c("Developed", "Cropland", "Grass/Shrub", "Tree Cover", "Water", "Wetlands", "Barren")
)
class_fill_scale <- c("#C0392B", "#B9770E", "#FAD7A0", "#00441B", "#21618C", "#AED6F1", "#D0D3D4")
names(class_fill_scale) <- class_colors$classes

lcpri_data$LCPRI <- factor(
  lcpri_data$LCPRI,
  levels = class_colors$numeral,
  labels = class_colors$classes
)



lcmap_lower <- ggplot() +
  geom_raster(data = lcpri_data, aes(x, y, fill = LCPRI)) +
  geom_sf(data = state_shoreline, color = 'black', fill = NA) +
  scale_fill_manual(name = "", values = class_fill_scale, labels = class_colors$classes, na.value = 'transparent') +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  xlab(NULL) +
  ylab(NULL) +
  annotation_scale(
    location = "bl",
    text_cex = .8,
    pad_x = unit(0, "cm")
  ) +
  annotation_north_arrow(
    which_north = 'true',
    location = "tr",
    style = north_arrow_orienteering(
      fill = c("black", "black")
    ),
    height = unit(0.3, "in"),
    width = unit(0.3, "in")
  )

ggsave(
  "figures/lcmap_2019.png",
  lcmap_lower,
  dpi = 300,
  width = 190,
  height = 190,
  units = "mm"
)

fia_plots <- read.csv("~/Documents/CAFRI/data/Inventory/FIA/NY/NYS_CSV_2019/NY_PLOT.csv") |>
  dplyr::filter(INVYR > 2000) |>
  dplyr::group_by(PLOT) |>
  dplyr::filter(INVYR == max(INVYR)) |>
  dplyr::ungroup() |>
  sf::st_as_sf(coords = c('LON', 'LAT'), crs = 'EPSG:4326') |>
  sf::st_transform('EPSG:26918')

lcmap_w_plots <- ggplot() +
  geom_raster(data = lcpri_data, aes(x, y, fill = LCPRI)) +
  geom_sf(data = state_shoreline, color = 'black', fill = NA) +
  geom_sf(data = fia_plots, color = 'black', size = 0.6) +
  scale_fill_manual(name = "", values = class_fill_scale, labels = class_colors$classes, na.value = 'transparent') +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  xlab(NULL) +
  ylab(NULL) +
  annotation_scale(
    location = "bl",
    text_cex = .8,
    pad_x = unit(0, "cm")
  ) +
  annotation_north_arrow(
    which_north = 'true',
    location = "tr",
    style = north_arrow_orienteering(
      fill = c("black", "black")
    ),
    height = unit(0.3, "in"),
    width = unit(0.3, "in")
  )

ggsave(
  "figures/lcmap_x_plots_2019.png",
  lcmap_w_plots,
  dpi = 300,
  width = 190,
  height = 190,
  units = "mm"
)


lcmap_w_plots20 <- ggplot() +
  geom_raster(data = lcpri_data, aes(x, y, fill = LCPRI)) +
  geom_sf(data = state_shoreline, color = 'black', fill = NA) +
  geom_sf(data = fia_plots[sample(1:nrow(fia_plots), .2 * nrow(fia_plots)),], color = 'black', size = 0.6) +
  scale_fill_manual(name = "", values = class_fill_scale, labels = class_colors$classes, na.value = 'transparent') +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  xlab(NULL) +
  ylab(NULL) +
  annotation_scale(
    location = "bl",
    text_cex = .8,
    pad_x = unit(0, "cm")
  ) +
  annotation_north_arrow(
    which_north = 'true',
    location = "tr",
    style = north_arrow_orienteering(
      fill = c("black", "black")
    ),
    height = unit(0.3, "in"),
    width = unit(0.3, "in")
  )

ggsave(
  "figures/lcmap_x_plots20_2019.png",
  lcmap_w_plots20,
  dpi = 300,
  width = 190,
  height = 190,
  units = "mm"
)
