# package
library(sf)
library(giscoR)
library(tidyverse)
library(ggshadow)
library(ggforce)
library(terra)
library(ragg)

ortho_crs <-'+proj=ortho +lat_0=51 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'


# data
# temp anomaly
# from: https://data.giss.nasa.gov/pub/gistemp/gistemp1200_GHCNv4_ERSSTv5.nc.gz
temp_an <- rast(here::here("25_heat", "input", "gistemp1200_GHCNv4_ERSSTv5.nc"))

temp_an_jul <- terra::subset(
  temp_an,
  time(
    temp_an
  ) == as.Date(
    "2024-07-15"
  )
)

temp_an_ortho <- project(temp_an_jul, ortho_crs)

temp_df <- as.data.frame(temp_an_ortho, xy = TRUE)

# globe
globe <- st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |>
  st_sfc(crs = ortho_crs)

globe_df <- st_cast(globe, "LINESTRING") |> st_coordinates() |> as.data.frame()

# world
world_poly <- gisco_get_countries(epsg = "4326", resolution = "10") 

wrl_ortho <- world_poly |>
  st_intersection(st_transform(globe, 4326)) |>
  st_transform(crs = ortho_crs)

# grid
grid <- st_graticule()

grid_crp <- st_difference(grid, st_union(world_poly))

grid_crp <- st_intersection(grid_crp, st_transform(globe, 4326)) |>
  st_transform(crs = ortho_crs)

# theme
fmain <- "Noto Sans"
fmono <- "Noto Sans Mono"

theme_set(theme_void(base_size = 16, base_family = fmain))
theme_update(
  plot.background = element_rect(
    fill = "grey99", color = NA
  ),
  plot.margin = margin(rep(15, 4), unit = "pt"),
)

# camcorder
width <- 9.5
height <- 9.5

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map

ggplot() +
  geom_glowpath(data = globe_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.05,
                shadowsize = 1.8) +
  geom_glowpath(data = globe_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.02,
                shadowsize = 1) +
  geom_glowpath(data = globe_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey80',
                colour = "white",
                alpha = .01,
                shadowalpha=0.01,
                shadowsize = .5) +
  geom_raster(data = temp_df, 
              aes(x, y, fill = tempanomaly_1735)) +
  geom_sf(data = grid_crp, 
          colour = "white", 
          linewidth = .2) +
  geom_sf(data = wrl_ortho, 
          fill = NA,
          colour = "grey15",
          linewidth = .2) +
  geom_mark_circle(data = filter(temp_df, tempanomaly_1735 == max(tempanomaly_1735)),
                   aes(x, y,
                       description = str_glue('{scales::number(tempanomaly_1735, accuracy = .1, decimal.mark = ".", style_positive = "plus", suffix = "ºC")}')),
                   expand = unit(1, "mm"), 
                   label.buffer = unit(3, "mm"),
                   label.margin = margin(1, 1, 1, 1, "mm"),
                   label.family = fmain,
                   label.fontsize = 18,
                   label.fontface = "bold",
                   label.fill = alpha("grey99", .7),
                   con.type = "straight",
                   con.size = 0.3,
                   con.cap = unit(1, "mm")
                   ) +
  # geom_sf(data = globe, fill = NA, color = "grey15", linewidth = .5) +
  scale_fill_gradient2(
    low = "#85b7ce",
    mid = "#ffffe0",
    high = "#93003a",
    midpoint = 0,
    breaks = seq(from = -1, to= 4, by =1),
    na.value = "grey99"
  ) +
  guides(fill = guide_colourbar(barwidth = 15, 
                                barheight = .5, 
                                title.position = "top",
                                title.hjust = .5)) +
  coord_sf() +
  labs(
    title = 'Temperature anomaly for July 2024',
    fill = "Temperature anomaly in °C\nvs. average for 1950-1980",
    caption = "Data: NASA · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme(legend.position = "bottom",
        legend.title = element_text(
          family = fmono, size = 14, colour = "grey15"
        ),
        legend.text = element_text(
          family = fmono, size = 12, colour = "grey15"
        ),
        plot.caption = element_text(
          colour = "grey50", size = 12, hjust = 0.5
        ),
        plot.title = element_text(
          size = 24, face = "bold", hjust = 0.5
        ))

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("25_heat", "25_heat.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
