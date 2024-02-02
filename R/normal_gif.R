library(ggplot2)
library(ggdist)
make_gif <- function(gif_dir, num) {
  system(
    sprintf("convert -delay 50 $(for i in $(seq 1 %i); do echo %s/density_${i}.png; done) -loop 0 %s/norm.gif",
    num,
    gif_dir,
    gif_dir)
  )
}


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

movie_dir <- here::here("normal_movie")
dir.create(movie_dir, showWarnings = FALSE)

sample <- c()

mean <- 100
sd <- 25

num <- 70

sample_sizes <- c(3, rep(1, 17), rep(10, num - 18))

for (i in 1:num) {

  if (i == 1) {
    sample <- rnorm(sample_sizes[i], mean = mean, sd = sd)
  } else {
    sample <- c(sample, rnorm(sample_sizes[i], mean = mean, sd = sd))
  }

  message(length(sample))

  p <- ggplot(data.frame(sample = sample),
              aes(x = sample)) +

    # stat_slab(position = 'dodge', scale = 0.5, fill = NA, color= 'black', adjust = 0.2) +
    geom_dots(position = "dodge", color = 'black', layout = 'swarm', side = 'top', adjust = 0.4) +
    geom_density(aes(x = sample, y = after_stat(scaled))) +
    map_theme() +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linewidth = 1) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, linewidth = 1) +
    ylab('Density') +
    scale_x_continuous(limits = c(10, 190)) +
    # scale_y_continuous(limits = c(0, 1)) +
    xlab("AGB Prediction") +
    theme(panel.grid = element_blank()) +
    ggtitle(glue::glue("N = {length(sample)}, Mean = {round(mean(sample), 2)}, SD = {round(sd(sample), 2)}"))

  ggsave(file.path(movie_dir, glue::glue("density_{i}.png")),
         p,
         units = 'mm',
         width = 225,
         height = 125,
         dpi = 300)
}

make_gif(here::here("normal_movie"), num)
