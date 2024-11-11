# package
library(sf)
library(giscoR)
library(tidyverse)
library(ggtext)
library(ragg)

# data
ice_1850 <- st_read(here::here("11_arctic", "input", "SGI_1850.shp")) |> 
  mutate(year = 1850) |> 
  select(year)

ice_2016 <- st_read(here::here("11_arctic", "input", "SGI_2016_glaciers.shp")) |> 
  st_transform(crs = st_crs(ice_1850)) |> 
  mutate(year = 2016) |> 
  select(year)

ice <- ice_1850 |> 
  bind_rows(ice_2016)

adm0 <- gisco_get_countries(country = "Switzerland", resolution = "03") |> 
  st_transform(crs = st_crs(ice_1850)) 

# theme
fmain <- "Noto Sans"

# camcorder
width <- 10
height <- 5

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
ggplot() +
  geom_sf(data = adm0, color = "#878787", fill = NA) +
  geom_sf(data = ice, fill = "#90d3fb", color = NA) +
  labs(title = "Swiss Glaciers: A Century and a Half of Change",
       subtitle = "Comparing Switzerland's glacier extent in 1850 and 2016 to visualize the impact of climate change on glacial landscapes.",
       caption = "Data: Swisstopo · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  facet_wrap(vars(year), ncol = 2) +
  theme_void(base_size = 16, base_family = fmain) +
  theme(
    plot.background = element_rect(
      fill = "#171a33", color = NA
    ),
    plot.title = element_text(
      family = fmain, size = 24, face = "bold", hjust = 0, colour = "#fafafa"
    ),
    plot.subtitle = element_textbox_simple(
      size = 14, family = fmain, hjust = 0, color = "#878787", halign = 0,
      margin = margin(t = 5, b = 15)
    ),
    plot.caption = element_text(
      size = 10, family = fmain, hjust = .5, colour = "#595959"
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
    strip.text = element_text(
      family = fmain, size = 16, face = "bold", colour = "#25b3ef", hjust = 0.5
    ),
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("11_arctic", "11_arctic.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
