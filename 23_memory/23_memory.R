# package
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(ragg)
sf_use_s2(FALSE)
proj <- "+proj=natearth +lon_0=0 +datum=WGS84 +units=m +no_defs"

# data
wrl <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3 != "ATA")

city <- c("Geneva", "New York", "Dakar", "Bogota", "Cúcuta", "Cali", "Riohacha",
          "San Diego", "Nairobi", "Khartoum", "Kinshasa", "Pamplona", "Panama City",
          "København", "Stuttgart", "Amman", "Sanaa", "Budapest", "Oslo", "Berlin")

ne_city <- ne_download(scale = 10, type = "populated_places_simple", category = "cultural")

city_sf <- ne_city |> 
  filter(name %in% city) |> 
  mutate(filter = case_when(
    name == "Pamplona" & sov0name == "Colombia" ~ 0,
    name == "Panama City" & sov0name == "United States" ~ 0,
    TRUE ~ 1
  )) |> 
  filter(filter == 1) |> 
  mutate(name = case_when(
    name == "København" ~ namepar,
    name == "Oslo" ~ "Starum",
    TRUE ~ name
  ),
  type = case_when(
    name %in% c("Geneva", "Bogota", "Dakar", "New York") ~ "work",
    TRUE ~ "mission"
  ))

bbox <- st_bbox(city_sf)
# theme
fmain <- "Lato"

# camcorder
width <- 10
height <- 6.3

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
ggplot() +
  geom_sf(data = wrl, fill = "grey99",
          color = "grey90", linewidth = 0.2) +
  geom_sf(data = city_sf,
          aes(color = type),
          show.legend = FALSE,
          size = if_else(city_sf$type == "work",
                         3, 1.5)) +
  geom_text_repel(data = city_sf,
                   aes(x = longitude, y = latitude, label = name),
                   max.overlaps = 10, 
                  size = if_else(city_sf$type == "work",
                                 12 / .pt, 10 / .pt),
                  color = if_else(city_sf$type == "work",
                                 "grey15", "grey30"),
                  family = fmain) +
  scale_color_manual(values = c("#00B398", "#0072bc")) +
  coord_sf(crs = proj, default_crs = 4326,
           xlim = c(bbox$xmin, bbox$xmax), 
           ylim = c(bbox$ymin, bbox$ymax+ 5)) +
  labs(
    title = "Work Journeys: From Homes to Missions",
    subtitle = "Visualizing my UN experience, places I called <b style='color: #0072bc'>home</b> and <b style='color: #00B398'>missions</b> traveled—all from memory.",
    caption = "Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_family = fmain, base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    panel.background = element_rect(
      fill = "grey90", color = NA
    ),
    plot.title = element_text(
      family = fmain, face = "bold", color = "grey15", size = 24
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      family = fmain, color = "grey30", size = 14,
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = fmain,
      color = "grey60"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
  )


# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("23_memory", "23_memory.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
