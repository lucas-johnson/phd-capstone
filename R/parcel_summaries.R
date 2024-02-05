library(sf)
library(ggplot2)
library(patchwork)

map_theme <- function() {
  theme_minimal() +
    theme(
      legend.text=element_text(size=14),
      legend.title = element_text(size=16),
      plot.title = element_text(size = 14),
      strip.text = element_text(size=12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      plot.background = element_rect(color = 'white')
    )
}

tax_wwe <- sf::st_read("/Volumes/giant_bag/dec_show_and_tell_07282023/wwe_parcels.gpkg") |>
  sf::st_transform('EPSG:26918')
wwe_region <- sf::st_transform(
  labrador.client::get_region("warren_washington_essex"),
  "EPSG:26918"
)

lower_cap <- -50
upper_cap <- 50

the_labels <- unlist(lapply(seq(lower_cap, upper_cap, 25), \(l) {
  ifelse(l == upper_cap, paste0(l, "+"), l)
}))
wwe_mean_delta <- tax_wwe[!is.na(tax_wwe$delta_agb), ] |>
  dplyr::mutate(delta_agb = ifelse(delta_agb < lower_cap,
                                   lower_cap,
                                   delta_agb)) |>
  dplyr::mutate(delta_agb = ifelse(delta_agb > upper_cap,
                                   upper_cap,
                                   delta_agb)) |>
  ggplot() +
  geom_sf(aes(fill = delta_agb), linewidth = 0.05, color = NA) +
  geom_sf(data = wwe_region, fill = NA, size = 2, color = "black") +
  scale_fill_gradientn(
    expression(Delta~" AGB, 2019 - 1990 (Mg"~ha^-1*")"),
    colors = c("#8C510A", "#f7f7f7", "#01665E"),
    values = scales::rescale(c(lower_cap, 0, upper_cap)),
    labels = the_labels,
    breaks = seq(lower_cap, upper_cap, 25)
  ) +
  map_theme() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
        axis.text.y = element_blank(),
        legend.key.width=unit(1,"cm")) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = c(0.005, 0.005))

wwe_mean_stock <- tax_wwe[!is.na(tax_wwe$agb_2019), ] |>
  ggplot() +
  geom_sf(aes(fill = agb_2019), linewidth = 0.05, color = NA) +
  geom_sf(data = wwe_region, fill = NA, size = 2, color = "black") +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(9, "Greens"),
    na.value = 'white',
    name = bquote("AGB, 2019 (Mg"~ha^-1*")"),
    limits = c(0, NA),
    guide = guide_colorbar(
      title.vjust = .9
    ),
  ) +
  map_theme() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
        legend.key.width=unit(1,"cm")) +
  ggspatial::annotation_scale(
    location = "bl",
    text_cex = 0.8,
    # pad_y = unit(15, "mm"),
    width_hint = 0.15
  ) +
  ggspatial::annotation_north_arrow(
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    pad_y = unit(1, "cm"),
    location = 'tr',
    which_north = 'true'
  ) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = c(0.005, 0.005))


nys <- labrador.client::get_region('state_shoreline') |>
  sf::st_transform("EPSG:26918")
inset <- ggplot() +
  geom_sf(data = nys, color = 'black', fill = NA, linewidth=0.7) +
  geom_sf(data = wwe_region, color = 'black', fill = NA, linewidth = 0.7) +
  map_theme() +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = c(0.005, 0.005))


combo <- (wwe_mean_stock | wwe_mean_delta) +
  patchwork::inset_element(inset, 0.8, 0.8, 1, 1)
  theme(
    panel.spacing = unit(c(0, 0, 0, 0), 'cm'),
    plot.margin = unit(c(0, 0, 0, 0), 'cm'),
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.box = "vertical",
    legend.box.just = "right",
  )

ggsave(here::here("figures/parcel_sumamries.png"),
       combo,
       dpi = 300,
       height = 300,
       width = 450,
       units = 'mm')



tax_wwe <- tax_wwe[!is.na(tax_wwe$agb_2019), ]

tax_wwe$area_ha <- sf::st_area(tax_wwe) |> units::set_units('hectare')
tax_wwe$perimeter <- lwgeom::st_perimeter(tax_wwe)

future::plan('multisession')
lcpri_2019 <- labrador.client::get_cafri_data(data = tax_wwe, 
                                              product = 'lcpri_2019', 
                                              filename = tempfile()) |>
  terra::rast()

is_forest <- terra::ifel(lcpri_2019 %in% c(3, 4, 6), 1, 0)

tax_wwe$forest_area <- exactextractr::exact_extract(is_forest, tax_wwe, 'sum')
tax_wwe$veg_prop <- ((tax_wwe$forest_area * 900 / 10000) / tax_wwe$area_ha * 100) |> 
  units::drop_units()

lm <- readRDS("~/Code/lib/model-based-inf/data/total_sd_lm.rds")
tax_wwe$mean <- tax_wwe$agb_2019
tax_wwe$area <- tax_wwe$area_ha
tax_wwe$se <- exp(predict(lm, sf::st_drop_geometry(tax_wwe)))
tax_wwe <- tax_wwe |> 
  dplyr::mutate(se = ifelse(se < 0, 0, se)) |>
  dplyr::mutate(se = ifelse(se > 40, 40, se))
  # dplyr::mutate(rse = se / agb_2019 * 100, 
  #               rse = ifelse(rse > 25, 25, rse))



wwe_mean_se <- tax_wwe[!is.na(tax_wwe$se), ] |>
  ggplot() +
  geom_sf(aes(fill = se), linewidth = 0.05, color = NA) +
  geom_sf(data = wwe_region, fill = NA, size = 2, color = "black") +
  scale_fill_viridis_c(limits = c(0, 40),
                       option = 'inferno',
                       labels = c(seq(0, 30, 10), '40+'),
                       breaks = c(seq(0, 40, 10)),
                       end = 0.925,
                       name = bquote("Standard Error (Mg"~ha^-1*")")) +
  map_theme() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
        legend.key.width=unit(1,"cm")) +
  ggspatial::annotation_scale(
    location = "bl",
    text_cex = 0.8,
    width_hint = 0.15
  ) +
  ggspatial::annotation_north_arrow(
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    pad_y = unit(1, "cm"),
    location = 'tr',
    which_north = 'true'
  ) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = c(0.005, 0.005))



combo <- (wwe_mean_stock | wwe_mean_se) +
  patchwork::inset_element(inset, 0.8, 0.8, 1, 1) + 
theme(
  panel.spacing = unit(c(0, 0, 0, 0), 'cm'),
  plot.margin = unit(c(0, 0, 0, 0), 'cm'),
  legend.position = "bottom",
  axis.title = element_blank(),
  legend.box = "vertical",
  legend.box.just = "right",
)

ggsave(here::here("figures/parcel_stderr.png"),
       combo,
       dpi = 300,
       height = 300,
       width = 450,
       units = 'mm')


tax_wwe$se <- exp(predict(lm, sf::st_drop_geometry(tax_wwe)))
tax_wwe <- tax_wwe |> 
  dplyr::mutate(se = ifelse(se < 0, 0, se)) |>
  # dplyr::mutate(se = ifelse(se > 40, 40, se))
  dplyr::mutate(rse = se / agb_2019 * 100,
                rse = ifelse((rse > 25 & !is.na(rse) & !is.infinite(rse)), 25, rse),
                rse = ifelse(is.infinite(rse), 0, rse))



wwe_mean_rse <- tax_wwe[!is.na(tax_wwe$rse), ] |>
  ggplot() +
  geom_sf(aes(fill = rse), linewidth = 0.05, color = NA) +
  geom_sf(data = wwe_region, fill = NA, size = 2, color = "black") +
  scale_fill_viridis_c(limits = c(0, 25),
                       na.value = 'transparent',
                       option = 'inferno',
                       labels = c(seq(0, 20, 5), '25+'),
                       breaks = c(seq(0, 25, 5)),
                       end = 0.925,
                       name = "rSE %") +
  map_theme() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
        legend.key.width=unit(1,"cm")) +
  ggspatial::annotation_scale(
    location = "bl",
    text_cex = 0.8,
    width_hint = 0.15
  ) +
  ggspatial::annotation_north_arrow(
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    pad_y = unit(1, "cm"),
    location = 'tr',
    which_north = 'true'
  ) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = c(0.005, 0.005))


combo <- (wwe_mean_stock | wwe_mean_se | wwe_mean_rse) +
  plot_layout(ncol = 3) +
  theme(
    panel.spacing = unit(c(0, 0, 0, 0), 'cm'),
    plot.margin = unit(c(0, 0, 0, 0), 'cm'),
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.box = "vertical",
    legend.box.just = "right",
  )

ggsave(here::here("figures/parcel_rse.png"),
       combo,
       dpi = 300,
       height = 300,
       width = 500,
       units = 'mm')

