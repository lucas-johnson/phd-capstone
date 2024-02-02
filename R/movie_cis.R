library(ggspatial)
library(ggplot2)
library(terra)
library(RColorBrewer)
library(patchwork)
make_gif <- function(gif_dir) {
  system(
    sprintf(
      "convert -delay 50 %s -loop 0 %s",
      sprintf("%s/*.png", gif_dir),
      here::here(gif_dir, "agb.gif")
    )
  )
}

raster_to_table <- function(raster, col) {
  r_data <- as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  names(r_data) <- c("x", "y", col)
  return(r_data)
}


make_mp4 <- function(mp4_dir, start_year) {
  system(
    paste(
      sprintf(
        "ffmpeg -r 1.5 -f image2 -s 1920x1080 -start_number %i -i %s",
        start_year,
        mp4_dir
      ),
      "/%04d.png ",
      sprintf(
        "-vcodec libx264 -crf 25  -pix_fmt yuv420p %s/agb.mp4",
        mp4_dir
      ),
      sep = ""
    )
  )
}

map_theme <- theme_minimal() +
  theme(
    legend.text=element_text(size=10),
    legend.title = element_text(size=12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

sum_plot <- function(data, index) {
  plot_data <- dplyr::bind_rows(lapply(seq_len(length(data)), \(i) {
    d <- data[[i]]
    list(
      total = d$total,
      upper_total = d$upper_total,
      lower_total = d$lower_total,
      year = d$year,
      color = ifelse(i == index, "highlight", "regular")
    )
  }))
  min_val <- floor(min(plot_data$lower_total) / 50) * 50
  max_val <- ceiling(max(plot_data$upper_total) / 50) * 50
  ggplot(plot_data, aes(x = year)) +
    geom_line(aes(y = total), color = 'black') +
    geom_line(aes(y = upper_total), color = 'black', linetype='longdash', alpha = 0.7) +
    geom_line(aes(y = lower_total), color = 'black', linetype='longdash', alpha = 0.7) +
    geom_point(
      aes(y = total),
      data = plot_data |> dplyr::filter(color == "highlight"), color = "red",
      size = 3
    ) +
    theme_minimal() +
    ylab("AGB (Thousands Mg)") +
    xlab("Year") +
    scale_x_continuous(expand = c(0.005, 0.005), limits = c(1990, 2020), breaks = c(seq(1990, 2015, 5), 2019)) +
    scale_y_continuous(limits = c(min_val, max_val)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)
    ) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1.5) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1.5)
  
}

agb_plot <- function(agb, year, max_val, bound) {
  message(year)
  agb_data <- raster_to_table(agb, 'agb')
  
  p <- ggplot() +
    geom_sf(data = sf::st_as_sf(bound), color = NA, size = 0.5, fill = 'white') +
    geom_raster(data = agb_data, aes(x, y, fill = agb)) +
    geom_sf(data = sf::st_as_sf(bound), color = 'black', size = 0.5, fill = NA) +
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
    scale_x_continuous(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    ggtitle(year)
  return(p)
}

na <- annotation_north_arrow(
  location = "tr",
  which_north = 'true',
  style = north_arrow_orienteering(
    fill = c("black", "black")
  ),
  height = unit(0.2, "in"),
  width = unit(0.2, "in")
)
scale <- annotation_scale(
  location = "bl",
  text_cex = .8
  # pad_x = unit(0, "cm")
)

lcmasked <- c(1, 2, 5, 8)
harvest <- terra::vect(here::here('data/harvest.gpkg')) |>
  sf::st_as_sf()
future::plan('multisession')
cleaned_data <- lapply(1990:2019, \(yr) {
  print(yr)
  lcpri <- labrador.client::get_cafri_data(product = glue::glue('lcpri_{yr}'),
                                           data = harvest,
                                           filename = tempfile(),
                                           crs = labrador.client::get_cafri_crs()) |>
    terra::rast() |>
    terra::crop(harvest) |>
    terra::mask(harvest)
  
  agb <- labrador.client::get_cafri_data(product = glue::glue('landsat_ensemble_agb_{yr}'),
                                         data = harvest,
                                         filename = tempfile(),
                                         crs = labrador.client::get_cafri_crs()) |>
    terra::rast() |>
    terra::crop(harvest) |>
    terra::mask(harvest)
  
  
  agb[lcpri %in% lcmasked] <- 0
  lcpri[lcpri %in% lcmasked] <- NA
  mean_agb <- terra::global(agb, 'mean', na.rm = FALSE)$mean
  harvest_area <-  (sf::st_area(harvest) |>
                      units::set_units('hectare') |>
                      units::drop_units())
  
  forest_area <- (terra::global(!is.na(lcpri), 'sum', na.rm = FALSE)$sum * 900) |>
    units::set_units('m^2') |>
    units::set_units('hectare') |>
    units::drop_units()
  
  total_agb <- mean_agb * harvest_area
  
  
  return(list(
    agb = agb,
    year = yr,
    total = total_agb / 1000,
    mean = mean_agb,
    area_ha = harvest_area,
    forest_prop = forest_area / harvest_area * 100,
    perimeter = lwgeom::st_perimeter(harvest)
  ))
})


lm <- readRDS("~/Code/lib/model-based-inf/data/total_sd_lm.rds")
predict_data <- lapply(cleaned_data, \(cd) {
  df <- data.frame(area = cd$area_ha,
                   mean = cd$mean,
                   perimeter = units::drop_units(cd$perimeter),
                   veg_prop = cd$forest_prop, 
                   year = cd$year)
  cd$se <- predict(lm, df)
  cd$upper <- cd$mean + (1.96 * cd$se)
  cd$lower <- cd$mean - (1.96 * cd$se)
  cd$total <- cd$mean * cd$area_ha / 1000
  cd$upper_total <- cd$upper * cd$area_ha / 1000
  cd$lower_total <- cd$lower * cd$area_ha / 1000
  return(cd)
  
}) 

max_agb <- max(unlist(lapply(seq_len(length(predict_data)), \(i) {
  terra::global(predict_data[[i]]$agb, 'max', na.rm = TRUE)
})))

results <- lapply(seq_len(length(predict_data)), \(i) {
  ap <- agb_plot(
    predict_data[[i]]$agb, predict_data[[i]]$year,
    max_agb, harvest
  ) + na + scale
  sum_p <- sum_plot(predict_data, i)
  
  combo <- (ap | sum_p) +
    plot_layout(
      guides = "collect"
    ) &
    theme(
      plot.margin = unit(c(0, 0, 0, 0), 'cm'),
      plot.background = element_rect(color = "white"),
      legend.position = 'bottom'
    )
  
  ggsave(
    here::here("movie_cis", paste0(predict_data[[i]]$year, ".png")),
    combo,
    height = 190,
    width = 260,
    units = "mm",
    dpi = 300
  )
  
})

make_mp4(here::here("movie_cis"), 1990)
make_gif(here::here("movie_cis"))

options_plot <- lapply(predict_data, \(pd) {
  data.frame(
    total = pd$total,
    upper_total = pd$upper_total,
    lower_total = pd$lower_total,
    year = pd$year
  )
}) |> 
  dplyr::bind_rows() |>
  ggplot(aes(x = year)) + 
  geom_line(aes(y = total), color = 'black', alpha = 0.5) + 
  geom_line(aes(y = upper_total), color = 'black', alpha = 0.5, linetype = 'longdash') + 
  geom_line(aes(y = lower_total), color = 'black', alpha = 0.5, linetype = 'longdash') +
  theme_minimal() +
  ylab("AGB (Thousands Mg)") +
  xlab("Year") +
  scale_x_continuous(expand = c(0.005, 0.005), limits = c(1990, 2020), breaks = c(seq(1990, 2015, 5), 2019)) +
  scale_y_continuous(limits = c(350, 450)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)
  ) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1.5) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1.5)

ggsave(here::here("figures/options_plot.png"), options_plot, width = 190, height = 125, dpi = 300, units = 'mm')
