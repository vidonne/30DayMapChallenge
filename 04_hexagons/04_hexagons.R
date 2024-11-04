# package
library(tidyverse)
library(sf)
library(acled.api)
library(rgeoboundaries)
library(ragg)

# setup
sf_use_s2(FALSE)

# data
sdn <- rgeoboundaries::gb_adm0(country = "Sudan")

sdn_grid <- sdn |> 
  st_coordinates() |> 
  as_tibble()

events <- acled.api::acled.api(
  country = "Sudan", start.date = "2023-04-15",
  end.date = "2024-11-04"
)

View(events)

events |> 
  summarise(n = n(), .by = c(event_type, sub_event_type)) |> 
  arrange(event_type)

events_filter <- events |> 
  filter(!(event_type %in% c("Protests", "Startegic developments"))) |> 
  mutate(lon = as.double(longitude),
         lat = as.double(latitude))

# theme
f1 <- "Work Sans"

# camcorder
width <- 10
height <- 9

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
ggplot() +
  geom_polygon(data = sdn_grid, aes(x=X, y = Y), fill = "grey90", color = "grey") +
  geom_hex(data = events_filter,
           aes(x = lon, y = lat), bins = 40, color = "transparent") +
  coord_fixed() +
  scale_fill_viridis_c(trans = "log", option = "B", direction = -1,
                       breaks = c(1, 10, 50, 400, 3000),
                       name = "Number\nof events") +
  labs(title = "Mapping Violence in Sudan Since April 2023",
       subtitle = "A hexbin map visualizing incidents of violence against civilians, explosions, battles, and riots in Sudan during the ongoing crisis.",
       caption = "Source: ACLED · Map: Cédric Vidonne · #30DayMapChallenge") +
  theme_void(base_size = 16, base_family = f1) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    plot.title = ggtext::element_textbox_simple(
      family = f1, face = "bold", color = "grey5", size = 22,
      margin = margin(b = 8), halign = .5, hjust = .5
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = f1, color = "grey25", size = 14, , halign = .5, hjust = .5,
    ),
    plot.caption = element_text(
      size = 12, hjust = 0.5, family = f1,
      color = "grey35"
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.position = "inside",
    legend.position.inside = c(.95, .15),
    legend.title = element_text(
      size = 14, color = "grey15"
    ),
    legend.text = element_text(
      size = 12, color = "grey25"
    )
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("04_hexagons", "04_hexagons.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
