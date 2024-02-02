library(terra)
library(ggplot2)
library(ggspatial)
library(patchwork)

map_theme <- function() {
  theme_minimal() +
    theme(
      legend.text=element_text(size=12),
      legend.title = element_text(size=14),
      plot.title = element_text(size = 14),
      strip.text = element_text(size=12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      plot.background = element_rect(color = 'white')
    )
}
raster_to_table <- function(raster, cols) {
  r_data <- as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  names(r_data) <- c("x", "y", cols)
  return(r_data)
}


hwf_plus <- terra::vect(here::here("data/hwf_plus.gpkg")) |>
  terra::project(labrador.client::get_cafri_crs())


gain <- terra::rast("/Volumes/giant_bag/dec_show_and_tell_07282023/agb_ss_gain.tiff") |>
  terra::crop(hwf_plus)

yogl <- terra::rast("/Volumes/giant_bag/dec_show_and_tell_07282023/temp_seg/agb_yogloss_1990_2019.tiff") |>
  terra::crop(hwf_plus)

ploss <- terra::rast("/Volumes/giant_bag/dec_show_and_tell_07282023/temp_seg/agb_ploss_1990_2019.tiff") |>
  terra::crop(hwf_plus)

gain_data <- raster_to_table(gain, 'gain')
yogl_data <- raster_to_table(yogl, 'yogl') |>
  dplyr::mutate(yogl = ifelse(yogl == 0, NA, yogl))
ploss_data <- raster_to_table(ploss, 'ploss')


bound <- terra::vect(terra::ext(gain), crs = terra::crs(gain)) |>
  sf::st_as_sf()

gain_plot <- ggplot() +
  geom_sf(data = bound, color = NA, fill = 'white', size = 0.5) +
  geom_raster(data = gain_data, aes(x, y, fill = gain)) +
  geom_sf(data = bound, color = 'black', fill = NA, linewidth = 0.75) +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(9, "Greens"),
    na.value = 'transparent',
    name = bquote("Growth rate (Mg"~ha^-1*~year^-1*")"),
    guide = guide_colorbar(
      title.vjust = .9
    ),
  ) +
  map_theme() +
  ggspatial::annotation_scale(
    location = "bl",
    text_cex = 0.8,
    # pad_y = unit(15, "mm"),
    width_hint = 0.15
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = 'bottom') +
  scale_y_continuous(expand = c(0.08, 0.08))

yogl_plot <- ggplot() +
  geom_sf(data = bound, color = NA, fill = 'white', size = 0.5) +
  geom_raster(data = yogl_data, aes(x, y, fill = yogl)) +
  geom_sf(data = bound, color = 'black', fill = NA, linewidth = 0.75) +
  scale_fill_viridis_c(na.value ='transparent',
                       name = 'Year of Greatest Loss',
                       guide = guide_colorbar(
                         title.vjust = .9
                       ),
                       limits = c(1990, 2020)) +
  map_theme() +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = 'bottom') +
  scale_y_continuous(expand = c(0.08, 0.08))

ploss_plot <- ggplot() +
  geom_sf(data = bound, color = NA, fill = 'white', size = 0.5) +
  geom_raster(data = ploss_data, aes(x, y, fill = ploss)) +
  geom_sf(data = bound, color = 'black', fill = NA, linewidth = 0.75) +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(9, "Reds"),
    na.value = 'transparent',
    name = "% AGB Loss",
    limits = c(0, 100),
    guide = guide_colorbar(
      title.vjust = .9
    ),
  ) +
  map_theme() +
  ggspatial::annotation_north_arrow(
    which_north = 'true',
    style = ggspatial::north_arrow_orienteering(
      fill = c("black", "black")
    ),
    height = unit(0.25, "in"),
    width = unit(0.25, "in"),
    location = 'tr'
    # pad_x = unit(0, "mm")
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = 'bottom') +
  scale_y_continuous(expand = c(0.08, 0.08))

combo <- (gain_plot |
            (yogl_plot + theme(axis.text.y = element_blank())) |
               (ploss_plot  + theme(axis.text.y = element_blank()))) &
  theme(plot.margin = unit(c(0,0,0,0), 'mm'),
        legend.key.width=unit(1,"cm"))

ggsave(here::here("figures/timeseries_combo.png"), units = 'mm', dpi = 300, width = 450, height = 300)
