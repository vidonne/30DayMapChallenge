# package
library(sf)
library(terra)
library(tidyterra)
library(giscoR)
library(tidyverse)
library(rayshader)
library(magick)
library(ragg)

# data
country <- gisco_get_countries(country = "Switzerland", resolution = "03") |> 
  st_transform(crs = 4326)

forest_cover <- terra::rast(
  here::here("18_3d",
             "E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif")
  )

# crop raster and rast to df Milos tuto https://github.com/milos-agathon/3d-forest-map-ggplot2/blob/main/R/main.r
get_forest_cover_cropped <- function() {
  country_borders_vect <- terra::vect(
    country
  )
  forest_cover_cropped <- terra::crop(
    forest_cover, country_borders_vect,
    snap = "in", mask = T
  )
  
  return(forest_cover_cropped)
}

forest_cover_cropped <- get_forest_cover_cropped() |>
  terra::aggregate(fact = 2)

forest_cover_df <- forest_cover_cropped |>
  as.data.frame(xy = T)

names(forest_cover_df)
names(forest_cover_df)[3] <- "percent_cover"

# breaks
# summary(forest_cover_df$percent_cover)
# quantile(forest_cover_df$percent_cover, seq(0, 1, 0.05))
min_val <- min(forest_cover_df$percent_cover)
max_val <- max(forest_cover_df$percent_cover)
limits <- c(min_val, max_val)

breaks <- c(0, 0, 0, 0, 0, 2 ,4.75, 7, 9.5, 12.25, 16, 20.75, 27.25, 35.5, 44,5, 
            53.5, 61.25, 68.5, 75.75, 95, 100)

# Create a function to generate labels with empty strings
create_20_step_labels <- function(breaks) {
  # Initialize all labels as empty strings
  labels <- rep("", length(breaks))
  
  # Find positions where values are closest to multiples of 20
  target_values <- seq(0, 100, by = 20)
  
  for (val in target_values) {
    # Find the position of the break closest to this target value
    closest_pos <- which.min(abs(breaks - val))
    labels[closest_pos] <- as.character(val)
  }
  
  return(labels)
}

# Generate the labels
custom_labels <- create_20_step_labels(breaks)

# camcorder
width <- 10
height <- 7.5

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
p <- 
  ggplot() +
  geom_raster(
    data = forest_cover_df,
    aes(
      x = x, y = y, fill = percent_cover
    )) +
  scale_fill_whitebox_c(palette = "high_relief",
                     direction = -1,
                     breaks = breaks,
                     limits = limits,
                     labels = custom_labels,
                     name = "% of area") +
  theme_void(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.margin = margin(rep(30, 4), unit = "pt"),
  )

p_legend <- ggplot() +
  geom_raster(
    data = forest_cover_df,
    aes(
      x = x, y = y, fill = percent_cover
    )) +
  scale_fill_whitebox_c(palette = "high_relief",
                        direction = -1,
                        breaks = breaks,
                        limits = limits,
                        labels = custom_labels,
                        name = "% of area") +
  theme_void(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.position = "top",
    legend.title.position = "top",
    legend.margin = margin(t =10, b= 0),
    legend.title = element_text(
      size = 12, color = "grey15", hjust = 0, face = "bold",
      margin = margin(b = 5)
    ),
    legend.text = element_text(
      size = 10, color = "grey30", margin = margin(t = 4)
    ),
    legend.ticks = element_blank(),
    plot.margin = margin(rep(30, 4), unit = "pt"),
  ) +
  guides(fill = guide_colourbar(
    direction = "horizontal",
    keywidth = unit(60, units = "mm"),
    keyheight = unit(3, units = "mm"),
  ))

# stop record
camcorder::gg_stop_recording()

# rayshader
rgl::close3d()

plot_gg(
  p,
  width = 10,
  height = 7.5,
  multicore = T,
  phi = 89,
  theta = 0,
  scale = 120,
  shadow_intensity = .98,
  windowsize = c(1000, 750),
  zoom = .8
)

render_camera(zoom = .5)

# render
rayshader::render_highquality(
  filename = "18_3d/default.png",
  preview = T,
  width = width * 160,
  height = height * 160,
  parallel = T,
  interactive = F,
  # lightintensity = 750,
  # lightaltitude = 30,
)

# put together
map <- image_read("18_3d/images/default.png")
legend <- image_read("18_3d/images/legend.png")
