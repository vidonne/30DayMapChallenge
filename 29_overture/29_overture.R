# packages
library(tidyverse)
library(arrow)
library(sf)
library(giscoR)
library(ragg)
library(scico)

# data
url <- "s3://overturemaps-us-west-2/release/2024-11-13.0/theme=buildings"

buildings <- open_dataset(url)

# nrow(buildings)

geneva <- giscoR::gisco_get_nuts(country = "Switzerland", resolution = "01") |> 
  filter(NUTS_ID == "CH013")

geneva <- geneva |> 
  st_cast('POLYGON') |> 
  head(1)

gva_bbox <- geneva |> 
  st_bbox() |> 
  as.vector()

sf_buildings <- buildings |> 
  filter(bbox$xmin > gva_bbox[1],
         bbox$ymin > gva_bbox[2],
         bbox$xmax < gva_bbox[3],
         bbox$ymax < gva_bbox[4]) |>
  select(id, geometry, height) |> 
  collect() |>
  st_as_sf(crs = 4326) |> 
  mutate(height = ifelse(is.na(height), 4, height))

# change crs
crs <- 2056

gva_crs <- geneva |> 
  st_transform(crs = crs)

build_crs <- sf_buildings |> 
  st_transform(crs = crs) |> 
  st_intersection(gva_crs)

# camcorder
width <- 10
height <- width * 0.8

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# theme
col_grad <- c("#febb3b", "#de8d65", "#c05b8b", "#7d448b")

# map
ggplot() +
  geom_sf(data = build_crs,
          aes(fill = height), color = NA, show.legend = FALSE) +
  scale_fill_gradientn(colors = col_grad) +
  labs(
    title = 'Geneva Building Footprint',
    caption = "Data: Overture · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_size = 16, base_family = "Fira Code") +
  theme(
    plot.background = element_rect(
      fill = "#24273d", color = NA
    ),
    plot.title = element_text(
      size = 24, color = "#fafafa", face = "bold", hjust = 0
    ),
    plot.caption = element_text(
      size = 12, color = alpha("#fafafa", 0.8), hjust = 0.5
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
  )


# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("29_overture", "29_overture.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
