# package
library(terra)
library(sf)
library(giscoR)
library(tidyverse)
library(tidyterra)
library(ragg)

# data
lakes <- st_read(here::here("06_raster", "input", "k4seenyyyymmdd11_ch2007Poly.shp"))

adm0 <- gisco_get_countries(country = "Switzerland", resolution = "03") |> 
  st_transform(crs = st_crs(lakes))

adm1 <- gisco_get_nuts(country = "Switzerland", resolution = "03", nuts_level = 3) |> 
  st_transform(crs = st_crs(lakes))

# data from
# "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2030_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C19.zip"
pop <- rast(here::here("06_raster", "input", "GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))

pop <- project(pop, crs(adm0, proj = TRUE))

pop <- crop(pop, adm0, mask = TRUE)

names(pop) <- "pop"

# theme
fmain <- "Noto Sans"

# camcorder
width <- 10
height <- 7

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
ggplot() +
  geom_spatraster(data = pop, aes(fill = pop)) +
  geom_sf(data = adm1, fill = NA, color = "grey60", linewidth = .2) +
  geom_sf(data = adm0, fill = NA, color = "grey40", linewidth = .5) +
  scale_fill_grass_c(na.value = NA, direction = 1,
                     palette = "population_dens",
                     use_grass_range = TRUE) +
  labs(title = "Population Density in Switzerland",
       subtitle = "The Raster Way",
       caption = "Data: GHSL, BFS · Map: Cédric Vidonne · #30DayMapChallenge",
       fill = "Inhabitants by km²"
       ) +
  theme_void(base_size = 16, base_family = fmain) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    plot.title = element_text(
      family = fmain, size = 22, face = "bold", hjust = .5, colour = "grey5"
    ),
    plot.subtitle = element_text(
      size = 14, family = fmain, hjust = .5, color = "grey15", face = "italic"
    ),
    plot.caption = element_text(
      size = 10, family = fmain, hjust = .5, colour = "grey25"
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.position = 'bottom',
    legend.title.position = "top",
    legend.title = element_text(
      size = 12, face = "bold", colour = "grey20", hjust = .5
    ),
    legend.text = element_text(
      size = 12, colour = "grey20"
    ),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(2, "cm")
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("06_raster", "06_raster.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
