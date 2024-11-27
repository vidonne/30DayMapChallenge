# package
library(tidyverse)
library(sf)
library(osmdata)
library(colorspace)
library(patchwork)
library(ragg)

# data
# set bbox
bbox <- getbb(place_name = "mbera camp")

# set API call
query <- opq(bbox = bbox)

# bnd layer
bnd <- query |> 
  add_osm_feature(
    key = "amenity",
    value = "refugee_site"
  ) |> 
  osmdata_sf()

# shelter
shelter <- query |> 
  add_osm_feature(
    key = "building",
    value = c("yes", "house", "shelter", "hut", "tent")
  ) |> 
  osmdata_sf()

# Roads layer
pedestrian <- query |>
  add_osm_feature(
    key = "highway",
    value = c("footway", "pedestrian", "path")
  ) |>
  osmdata_sf()

minor_roads <- query |>
  add_osm_feature(
    key = "highway",
    value = c(
      "secondary", "tertiary", "unclassified",
      "residential", "trunk_link",
      "primary_link", "motorway_link",
      "secondary_link", "tertiary_link"
    )
  ) |>
  osmdata_sf()

# sanitation
sanitation <- query |>
  add_osm_feature(
    key = "amenity",
    value = c(
      "toilets", "flush",
      "pitlatrine", "shower",
      "lavoir"
    )
  ) |>
  osmdata_sf()

# education
education <- query |>
  add_osm_feature(
    key = "amenity",
    value = "school"
  ) |>
  osmdata_sf()

# health
health <- query |>
  add_osm_feature(
    key = "amenity",
    value = c("hospital", "clinic", "doctors", "pharmacy")
  ) |>
  osmdata_sf()

# CRS
plot_crs <- 3345

bnd <- bnd$osm_polygons |> 
  st_transform(crs = plot_crs)

plot_bbox <- st_bbox(bnd)

shelter <- shelter$osm_polygons |> 
  st_transform(crs = plot_crs)


pedestrian <- pedestrian$osm_lines |>
  st_transform(crs = plot_crs)

minor_roads <- minor_roads$osm_lines |>
  st_transform(crs = plot_crs)

sanitation <- sanitation$osm_points |> 
  st_transform(crs = plot_crs) |> 
  st_intersection(bnd) |> 
  mutate(legend = "Toilets/Shower")

education <- education$osm_points |> 
  st_transform(crs = plot_crs) |> 
  st_intersection(bnd) |> 
  mutate(legend = "School")

health <- health$osm_points |> 
  st_transform(crs = plot_crs) |> 
  st_intersection(bnd) |> 
  mutate(legend = "Health facilities")

services <- bind_rows(sanitation, education, health)

# theme
background_fill <- "#cbbd93"
bg_highlight <- "#fae8b4"

fmain <- "Roboto Condensed"

# camcorder
width <- 10
height <- 6.8

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
main <- ggplot() +
  geom_sf(
    data = bnd,
    fill = alpha(bg_highlight, 0.8),
    color = darken(background_fill, 0.4),
    linewidth = .5
  ) +
  geom_sf(
    data = shelter,
    fill = lighten(bg_highlight, 0.5),
    color = darken(background_fill, 0.8),
    linewidth = 0.1
  ) +
  geom_sf(
    data = minor_roads,
    color = darken(background_fill, 0.4),
    linewidth = 2
  ) +
  geom_sf(
    data = minor_roads,
    color = lighten(bg_highlight, 0.8),
    linewidth = 0.9
  ) +
  geom_sf(
    data = pedestrian,
    color = alpha(lighten(bg_highlight, 0.8), 0.6),
  ) +
  geom_sf(data = services, aes(color = legend),
          alpha = if_else(services$legend == "Toilets/Shower", .5, .75),
          size = if_else(services$legend == "Toilets/Shower", .4, .8),) +
  scale_color_manual(values = c("#e85130", "#bd53bd", "#008dc9")) +
  labs(
    title = "Mbera Refugee Camp - Mauritania",
    subtitle = "Basic infrastructure",
    caption = "Data: OSM · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  coord_sf(xlim = c(plot_bbox$xmin, plot_bbox$xmax), ylim = c(plot_bbox$ymin, plot_bbox$ymax)) +
  theme_void(base_family = fmain, base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = background_fill, color = NA
    ),
    plot.title = element_text(
      family = "Roboto Slab", face = "bold", color = darken(background_fill, 0.8), size = 24, hjust = 0
    ),
    plot.subtitle = element_text(
      family = fmain, color = darken(bg_highlight, 0.7), size = 18, face = "plain",
      margin = margin(t = 5, b = 0), hjust = 0
    ),
    plot.caption = element_text(
      size = 12, hjust = 1, 
      color = alpha(darken(background_fill, 0.8), 0.7)
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.position = "inside",
    legend.position.inside = c(0, .98),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    legend.text = element_text(
      size = 12, color = alpha(darken(background_fill, 0.8), .8), margin = margin(l = 0)
    ),
  )

# inset
mrt <- giscoR::gisco_get_countries(country = "MRT", resolution = "03")
point <- bnd |> 
  st_centroid()


inset <- ggplot() +
  geom_sf(data = mrt, fill = alpha(bg_highlight, 0.8), color = darken(background_fill, 0.8),
          linewidth = 0.3) +
  geom_sf(data = point, color = "#008dc9", size =2) +
  geom_sf_text(data = mrt, aes(label = str_to_upper(NAME_ENGL)),
               family = fmain, fontface = "bold", size = 15/ .pt,
               color = darken(background_fill, 0.9), nudge_x = 0.5, nudge_y = -0.75) +
  geom_sf_text(data = point, aes(label = str_wrap(`name:en`, 15)),
               hjust = 1, family = fmain, color = darken(background_fill, 0.8),
               nudge_x = -0.2, nudge_y = 1.4, lineheight = 0.9, size = 11/.pt) +
  theme_void(base_family = fmain, base_size = 16)

# combine
main +
  inset_element(inset,
                left = 0.8,
                bottom = 0.7,
                top = 0.98,
                right = 0.98,
                align_to = "full")

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("27_micromapping", "27_micromapping.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
