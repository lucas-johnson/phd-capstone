library(ggplot2)
library(ggspatial)
library(terra)
library(RColorBrewer)
library(patchwork)
future::plan('multisession')

raster_to_table <- function(raster, col) {
  r_data <- as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  names(r_data) <- c("x", "y", col)
  return(r_data)
}
map_theme <- theme_minimal() +
  theme(
    legend.text=element_text(size=10),
    legend.title = element_text(size=12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

lcmasked <- c(1, 2, 5, 8)

hwf_idx <- "1706955_2517285"
hwf_chm_file <- here::here("data/hwf_plus_chm.tiff")
if (!file.exists(hwf_chm_file)) {
  labrador.client::get_cafri_data(
    product = "chm_warren_washington_essex",
    indices = c(hwf_idx),
    filename = hwf_chm_file
  )
}

chm <- terra::rast(hwf_chm_file)
cropped_chm_file <- here::here("data/hwf_plus_chm_cropped.tiff")
if (!file.exists(cropped_chm_file)) {
  chm |>
    terra::crop(terra::vect(here::here("data/hwf_plus.gpkg")) |>
                  terra::project(chm),
                filename = cropped_chm_file)
}

hwf_plus <- terra::vect(here::here("data/hwf_plus.gpkg")) |>
  terra::project(labrador.client::get_cafri_crs())

chm <- terra::rast(cropped_chm_file) |>
  terra::aggregate(fact = 3, fun = 'mean', na.rm = TRUE) |>
  terra::crop(hwf_plus)


landsat_agb_2015 <- labrador.client::get_cafri_data(indices = c(hwf_idx),
                                                    filename = tempfile(),
                                                    product = 'landsat_ensemble_agb_2015') |>
  terra::rast() |>
  terra::crop(hwf_plus)
lidar_agb_2015 <- labrador.client::get_cafri_data(indices = c(hwf_idx),
                                                  filename = tempfile(),
                                                  product = 'manuscript_lidar_agb') |>
  terra::rast() |>
  terra::crop(hwf_plus)
lcpri_2015 <- labrador.client::get_cafri_data(indices = c(hwf_idx),
                                              filename = tempfile(),
                                              product = 'lcpri_2015') |>
  terra::rast() |>
  terra::crop(hwf_plus)

landsat_agb_2015[lcpri_2015 %in% lcmasked] <- NA
lidar_agb_2015[lcpri_2015 %in% lcmasked] <- NA

lcpri_chm <- lcpri_2015 |>
  terra::project(chm, method = 'near') |>
  terra::crop(chm)
chm[lcpri_chm %in% lcmasked] <- NA
chm_data <- raster_to_table(chm, "m")


landsat_agb_data <- raster_to_table(landsat_agb_2015, 'agb')
lidar_agb_data <- raster_to_table(lidar_agb_2015, 'agb')
max_agb <- max(c(lidar_agb_data$agb, landsat_agb_data$agb), na.rm = TRUE)

landsat_agb_plot <- ggplot() +
  geom_sf(data = sf::st_as_sf(hwf_plus), color = NA, size = 0.5, fill = 'white') +
  geom_raster(data = landsat_agb_data, aes(x, y, fill = agb)) +
  scale_fill_gradientn(
    colours = brewer.pal(9, "Greens"),
    na.value = "transparent",
    name = bquote("AGB (Mg"~ha^-1*")"),
    limits = c(0, max_agb),
    guide = guide_colorbar(
      title.vjust = .9
    )
  ) +
  map_theme +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.key.width = unit(1.5, 'cm')) +
  ggtitle("Landsat Predictions 2015")

lidar_agb_plot <- ggplot() +
  geom_sf(data = sf::st_as_sf(hwf_plus), color = NA, size = 0.5, fill = 'white') +
  geom_raster(data = lidar_agb_data, aes(x, y, fill = agb)) +
  scale_fill_gradientn(
    colours = brewer.pal(9, "Greens"),
    na.value = "transparent",
    name = bquote("AGB (Mg"~ha^-1*")"),
    limits = c(0, max_agb),
    guide = guide_colorbar(
      title.vjust = .9
    )
  ) +
  map_theme +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.key.width = unit(1.5, 'cm')) +
  ggtitle("LiDAR Predictions 2015")


chm_plot <- ggplot() +
  geom_sf(data = sf::st_as_sf(hwf_plus), color = NA, size = 0.5, fill = 'white') +
  geom_raster(data = chm_data, aes(x, y, fill = m), alpha = 1) +
  scale_fill_viridis_c(
    na.value = "transparent",
    name = "Height (m)",
    guide = guide_colorbar(
      title.vjust = .9
    )
  ) +
  map_theme +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Canopy Height Model 2015")


na <- annotation_north_arrow(
  location = "tr",
  which_north = 'true',
  style = north_arrow_orienteering(
    fill = c("black", "black")
  ),
  height = unit(0.4, "in"),
  width = unit(0.4, "in")
)
scale <- annotation_scale(
  location = "bl",
  text_cex = 1,
  pad_x = unit(0, "cm")
)

compare_agb <- (
  (landsat_agb_plot + scale) |
    (chm_plot + theme(axis.text.y = element_blank())) |
    (lidar_agb_plot + theme(axis.text.y = element_blank()) + na)
) +
  plot_layout(
    guides = "collect"
  ) &
  theme(
    plot.margin = unit(c(0, 0, 0, 0), 'cm'),
    plot.background = element_rect(color = "white"),
    legend.position = 'bottom'
  )


ggsave(
  here::here("figures/hwf_agb_x_chm.png"),
  compare_agb,
  dpi = 300,
  units = "mm",
  width = 450,
  height = 300
)


harvest <- sf::st_read("data/harvest.gpkg")
harv_geom <- geom_sf(data = harvest, color = 'black', linewidth = 1, fill = NA)
compare_agb <- (
  (landsat_agb_plot + harv_geom + scale) |
    (chm_plot + theme(axis.text.y = element_blank())) |
    (lidar_agb_plot + theme(axis.text.y = element_blank()) + na)
) +
  plot_layout(
    guides = "collect"
  ) &
  theme(
    plot.margin = unit(c(0, 0, 0, 0), 'cm'),
    plot.background = element_rect(color = "white"),
    legend.position = 'bottom'
  )


ggsave(
  here::here("figures/hwf_agb_x_chm_harv.png"),
  compare_agb,
  dpi = 300,
  units = "mm",
  width = 450,
  height = 300
)
