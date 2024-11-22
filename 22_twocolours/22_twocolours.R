# package
library(tidyverse)
library(sf)
library(giscoR)
library(ragg)

# data
lake <- st_read(here::here("22_twocolours", "input", "k4seenyyyymmdd11_ch2007Poly.shp"))

adm0 <- gisco_get_countries(country = "Switzerland", resolution = "03") |> 
  st_transform(crs = st_crs(lake))

centroid <- adm0 |> 
  st_centroid() |> 
  st_coordinates()

# theme
fmain <- "Istok Web"
cmain <- "#d8232a"

# camcorder
width <- 10
height <- 7

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

xc <- centroid[1]
yc <- centroid[2]
sw <- 3e4


# map
ggplot() +
  geom_sf(data = adm0, fill = cmain, color = "white", linewidth = 1) +
  # geom_sf(data = lake, fill = "white", color = cmain,) +
  annotate(geom = "rect", xmin = xc - 0.5*sw, xmax = xc + 0.5*sw,
           ymin = yc-1*sw, ymax = yc+2*sw, fill = "white", color = NA)+
  annotate(geom = "rect", xmin = xc - 1.5*sw, xmax = xc + 1.5*sw,
           ymin = yc, ymax = yc+1*sw, fill = "white", color = NA)+
  labs(
    title = "SWITZERLAND",
    subtitle = "Red - White",
    caption = "Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_family = fmain, base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = cmain, color = NA
    ),
    plot.title = element_text(
      family = fmain, face = "bold", color = "white", size = 28, hjust = .5
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      family = fmain, color = "white", size = 16,
      margin = margin(t = 5, b = 0), hjust = .5
    ),
    plot.caption = element_text(
      size = 12, hjust = 0.5, family = fmain,
      color = "white"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("22_twocolours", "22_twocolours.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
