# package
library(sf)
library(giscoR)
library(tidyverse)
library(bertin)
library(ragg)

# data
adm3 <- st_read(here::here("07_vintage", "input", "K4polg20230101gf_ch2007Poly.shp"))

adm0 <- gisco_get_countries(country = "Switzerland", resolution = "03") |> 
  st_transform(crs = st_crs(adm3))

pop <- read_delim(here::here("07_vintage", "input", "27876_132.csv"), delim = ";") |>
  janitor::clean_names() |> 
  filter(variable == "Habitants par km² de la surface totale")

pop_sf <- adm3 |> 
  left_join(pop, by = c("id" = "geo_id"))

pop_point <- pop_sf |> 
  make_points(n = 60, square = F) |> 
  arrange(desc(value))

# theme
fmain <- "Noto Sans"

# camcorder
width <- 10
height <- 7.5

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
ggplot() +
  geom_sf(data = pop_point, aes(size = value), 
          shape = 21, fill = "black", color = "white") +
  geom_sf(data = adm0, fill = NA, color = "grey40") +
  scale_size(breaks = c(25, 250, 500, 2000, 10000),
             range = c(.1, 8),
             labels = scales::label_number(big.mark = ",")) +
  labs(title = "Population Density in Switzerland",
       subtitle = "The Bertin Way",
       caption = "Data: BFS · Map: Cédric Vidonne · #30DayMapChallenge",
       size = "Inhabitants per km²"
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
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("07_vintage", "07_vintage.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
