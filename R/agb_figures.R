library(terra)
library(ggplot2)
library(ggspatial)

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
raster_to_table <- function(raster, cols) {
  r_data <- as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  names(r_data) <- c("x", "y", cols)
  return(r_data)
}


get_lidar_agb_map <- function(region, write_file, year, height = 190, width = 190,
                              legend = 'right') {
  future::plan('multisession')
  agb <- labrador.client::get_cafri_data(product = 'manuscript_lidar_agb',
                                         region = region,
                                         filename = tempfile()) |>
    terra::rast()

  lcpri <- labrador.client::get_cafri_data(product = glue::glue('lcpri_{year}'),
                                           region = region,
                                           filename = tempfile()) |>
    terra::rast()


  region <- labrador.client::get_region(region) |>
    sf::st_transform('EPSG:26918')

  agb <- terra::mask(agb, lcpri %in% c(3, 4, 6), maskvalues = c(NA, FALSE)) |>
    terra::project('EPSG:26918') |>
    terra::crop(region) |>
    terra::mask(region) |>
    raster_to_table('agb')

  p <- ggplot() +
    geom_sf(data = region, color = NA, fill = 'white', size = 0.5) +
    geom_raster(data = agb, aes(x, y, fill = agb)) +
    geom_sf(data = region, color = 'black', fill = NA, size = 0.5) +
    scale_fill_gradientn(
      colours = RColorBrewer::brewer.pal(9, "Greens"),
      na.value = 'transparent',
      name = bquote("AGB (Mg"~ha^-1*")"),
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
    ggspatial::annotation_north_arrow(
      which_north = 'true',
      style = ggspatial::north_arrow_orienteering(
        fill = c("black", "black")
      ),
      height = unit(0.25, "in"),
      width = unit(0.25, "in"),
      location = 'tr'
      # pad_x = unit(10, "mm")
    ) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle(year) +
    theme(legend.position = legend,
          legend.key.width=unit(1, 'cm'))

  ggsave(write_file, p, dpi = 600, units = 'mm', width = width, height = height)
}

get_region_map <- function(region_name, write_file) {
  region <- labrador.client::get_region(region_name) |>
    sf::st_transform('EPSG:26918')
  nys <- labrador.client::get_region('state_shoreline') |>
    sf::st_transform('EPSG:26918')

  p <- ggplot() +
    geom_sf(data = nys, fill = 'white', color = 'black', size = 1) +
    geom_sf(data = region, fill = 'black', color = 'black', size = 1) +
    map_theme() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank()) +
    scale_x_continuous(expand = c(0.0001, 0.00001)) +
    scale_y_continuous(expand = c(0.0001, 0.00001))
  ggsave(write_file, p, units = 'mm', width = 50, height = 50, dpi = 300)
}

get_lidar_agb_map('allegany_steuben',
                  here::here('figures/lidar_agb_as.png'),
                  2016,
                  legend = 'bottom')
get_region_map('allegany_steuben',
           here::here('figures/as_lidar_inset.png'))
get_lidar_agb_map('warren_washington_essex',
                  here::here('figures/lidar_agb_wwe.png'),
                  2015)
get_region_map('warren_washington_essex',
           here::here('figures/wwe_lidar_inset.png'))
get_lidar_agb_map('cayuga_oswego',
                  here::here('figures/lidar_agb_co.png'),
                  2018)
get_region_map('cayuga_oswego',
           here::here('figures/co_lidar_inset.png'))
get_lidar_agb_map('3_county',
                  here::here('figures/lidar_agb_3cty.png'),
                  2014)
get_region_map('3_county',
           here::here('figures/c3_lidar_inset.png'))



