# package
library(tidyverse)
library(sf)
library(osmdata)
library(ragg)

# data
# define your midpoint coordinates and radius
lat <- 46.94688
lon <- 7.4433809
r <- 2000
crs <- 4326

midpoint <- st_point(c(lon, lat)) |>
  st_sfc() |>
  st_set_crs(crs)

circle_mask <- st_buffer(midpoint, dist = r)

# define the bounding box for overpass query
bbox <- st_bbox(circle_mask)

# set API call
query <- opq(bbox = bbox)

#  Water layer
water <- query |>
  add_osm_feature(
    key = "natural",
    value = "water"
  ) |>
  osmdata_sf()

# Roads layer
pedestrian <- query |>
  add_osm_feature(
    key = "highway",
    value = c("footway", "pedestrian")
  ) |>
  osmdata_sf()

minor_roads <- query |>
  add_osm_feature(
    key = "highway",
    value = c(
      "secondary", "tertiary",
      "residential", "trunk_link",
      "primary_link", "motorway_link",
      "secondary_link", "tertiary_link"
    )
  ) |>
  osmdata_sf()

main_roads <- query |>
  add_osm_feature(
    key = "highway",
    value = c("primary", "trunk", "motorway")
  ) |>
  osmdata_sf()

# Woods layer
woods <- query |>
  add_osm_features(list(
    "natural" = "wood",
    "leisure" = "park"
  )) |>
  osmdata_sf()

# CRS
plot_crs <- 2056
midpoint <- midpoint |> st_transform(crs = plot_crs)
circle_mask <- st_buffer(midpoint, dist = r)

water <- water$osm_polygons |>
  st_transform(crs = plot_crs) |>
  st_intersection(circle_mask)

woods <- woods$osm_multipolygons |>
  st_make_valid() |> # add in case you face 'geom is invalid' error
  st_transform(crs = plot_crs) |>
  st_intersection(circle_mask)

pedestrian <- pedestrian$osm_lines |>
  st_transform(crs = plot_crs) |>
  st_intersection(circle_mask)

minor_roads <- minor_roads$osm_lines |>
  st_transform(crs = plot_crs) |>
  st_intersection(circle_mask)

main_roads <- main_roads$osm_lines |>
  st_transform(crs = plot_crs) |>
  st_intersection(circle_mask)

# theme
background_fill <- "#292929"
woods_fill <- "#7cb9a3"
key_color <- "#dcdcdc"
water_fill <- "#39b9b4"

fmain <- "Work Sans"

# camcorder
width <- 10
height <- 10

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
ggplot() +
  geom_sf(
    data = circle_mask,
    fill = background_fill,
    color = NA
  ) +
  geom_sf(
    data = water,
    fill = water_fill,
    color = NA
  ) +
  geom_sf(
    data = woods,
    fill = woods_fill,
    color = NA
  ) +
  geom_sf(
    data = pedestrian,
    color = key_color,
    linewidth = .3,
    alpha = .3
  ) +
  geom_sf(
    data = minor_roads,
    color = key_color,
    linewidth = .4,
    alpha = .8
  ) +
  geom_sf(
    data = main_roads,
    color = key_color,
    linewidth = .8
  ) +
  geom_sf(
    data = circle_mask,
    fill = NA,
    color = key_color,
    linewidth = .8
  ) +
  labs(
    title = "BERN",
    subtitle = "Switzerland",
    caption = "Data: OSM · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_family = fmain, base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = background_fill, color = NA
    ),
    plot.title = element_text(
      family = fmain, face = "plain", color = key_color, size = 36, hjust = .5,
      margin = margin(t = 30)
    ),
    plot.subtitle = element_text(
      family = fmain, color = alpha(key_color, 0.6), size = 18, face = "plain",
      margin = margin(t = 5, b = 0), hjust = .5
    ),
    plot.caption = element_text(
      size = 12, hjust = 0.5, family = fmain,
      color = key_color, margin = margin(b = 15)
    ),
    plot.margin = margin(rep(0, 4), unit = "pt"),
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("20_osm", "20_osm.png"),
  device = agg_png,
  width = width, height = height, units = "in", dpi = 320
)
