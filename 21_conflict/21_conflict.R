# package
library(tidyverse)
library(sf)
library(acled.api)
library(rgeoboundaries)
library(scales)
library(ragg)

# setup
sf_use_s2(FALSE)

# data
ctry <- rgeoboundaries::gb_adm0(country = c(
  "Ukraine", "Moldova", "Romania", "Hungary", "Slovakia",
  "Poland", "Belarus", "Russia", "Bulgaria", "Serbia"))

bbox <- ctry |> 
  filter(shapeGroup == "UKR") |> 
  st_buffer(dist = .5) |> 
  st_bbox()

ctry_crop <- ctry |> 
  st_crop(bbox) |> 
  st_centroid() |> 
  filter(shapeGroup %in% c("BLR", "UKR", "ROU", "POL", "MDA", "RUS")) |> 
  mutate(label = str_wrap(str_to_upper(shapeName), 10))

cap <- rnaturalearth::ne_download(scale = 110, type = "populated_places_simple", category = "cultural") |>
  filter(sov_a3 == "UKR")


events <- acled.api::acled.api(
  country = "Ukraine", start.date = "2024-11-01",
  end.date = "2024-11-21"
)

events_sf <- events |>
  filter(event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence")) |>
  mutate(
    lon = as.double(longitude),
    lat = as.double(latitude),
  ) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

events_size <- events |>
  filter(event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence")) |>
  mutate(
    lon = as.double(longitude),
    lat = as.double(latitude),
  ) |> 
  summarise(n = n(), .by = c(lon, lat)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  arrange(desc(n))

# theme
f1 <- "Poppins"
fmap <- "Work Sans"

# camcorder
width <- 10
height <- 8.5

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

num_events <- events_sf |> 
  count() |> 
  pull(n)

num_events <- floor(num_events / 10) * 10
  
num_events <- scales::number(num_events, big.mark = ",")

subtitle = glue::glue("Mapping the more than <b style='color:#a00016'>", {num_events} , " conflict events</b> reported in <b>Ukraine</b> by ACLED between <b> November 1 and ",
                      day(max(events$event_date)), ", 2024</b>. This map highlights the intensity of ongoing battles, violence against civilians, and explosions, reflecting a situation that continues to evolve.")

ggplot() + 
  geom_sf(data = ctry,
          fill = if_else(ctry$shapeGroup == "UKR", "grey99",
                         "grey90")) +
  # geom_sf(data = cap, size = 3.5) +
  geom_sf(data = events_sf,
          shape = 21, fill = "#da484a", alpha = .4,
          color = "transparent", size = 2) +
  # geom_sf(data = events_size, aes(size = n),
  #         shape = 21, fill = "#da484a", alpha = .2,
  #         color = "grey99") +
  # scale_size_area(max_size = 10,
  #                 breaks = c(1, 5, 10, 20)) +
  geom_sf_label(data = ctry_crop, aes(label = label),
                color = if_else(ctry_crop$shapeGroup == "UKR", "grey10", "grey40"),
                size = if_else(ctry_crop$shapeGroup == "UKR", 20/.pt, 14/.pt),
                fontface = if_else(ctry_crop$shapeGroup == "UKR", "bold", "plain"),
                fill = if_else(ctry_crop$shapeGroup == "UKR", "grey99", "grey90"),
                label.size = NA,
                nudge_y = if_else(ctry_crop$shapeGroup == "RUS", 2.5, 0),
                nudge_x = if_else(ctry_crop$shapeGroup %in% c("MDA", "POL"), -1, 0),) +
  geom_sf_text(data = cap, aes(label = nameascii),
               family = fmap, color = "grey5", size = 16 /.pt,
               nudge_x = 0.3, nudge_y = -0.2) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax),
    crs = 6384, default_crs = 4326
  ) +
  labs(
    title = "Conflict in Motion: Ukraine",
    subtitle = subtitle,
    caption = "Source: ACLED · Map: Cédric Vidonne · #30DayMapChallenge"
  ) +
  theme_void(base_size = 16, base_family = f1) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    panel.background = element_rect(
      fill = "#d7e6f3", color = NA
    ),
    plot.title = ggtext::element_textbox_simple(
      family = f1, face = "bold", color = "grey5", size = 22, halign = 0, hjust =
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = f1, color = "grey25", size = 14, , halign = 0, hjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_text(
      size = 12, hjust = 0.5, family = f1,
      color = "grey35"
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
  )
  

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("21_conflict", "21_conflict.png"),
  device = agg_png,
  width = width, height = height, units = "in", dpi = 320
)
