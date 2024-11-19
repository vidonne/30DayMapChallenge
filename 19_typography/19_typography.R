# package
library(sf)
library(giscoR)
library(tidyverse)
library(ggtext)
library(ragg)

# data
lake <- st_read(here::here("19_typography", "input", "k4seenyyyymmdd11_ch2007Poly.shp"))

loc <- tibble::tribble(
  ~loc, ~lon, ~lat,
  "Münchenstein", 7.5141965, 47.5237552,
  "Rütli", 8.5904417, 46.9689686
)

loc_sf <- loc |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = st_crs(lake))

adm0 <- gisco_get_countries(country = "Switzerland", resolution = "03") |>
  st_transform(crs = st_crs(lake))


# theme
fmain <- "Helvetica CE 55 Roman"
cmain <- "#d8232a"

# camcorder
width <- 10
height <- 7.5

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
label <- tibble(label = c(
  '<b style="color: #222222; font-size: 24px;">Münchenstein</b><br>Birthplace of the<br><i style="color: #d8232a; font-size: 20px;">Font Helvetica</i><br><span style="color: #666666; font-size: 16px">1957</span>',
  '<b style="color: #222222; font-size: 24px;">Rütli Meadow</b><br>Birthplace of<br><i style="color: #d8232a; font-size: 20px;">Confoederatio Helvetica</i><br><span style="color: #666666; font-size: 16px">1291</span>'
))

label_sf <- loc_sf |>
  st_coordinates() |>
  bind_cols(label)

ggplot() +
  geom_sf(data = adm0, fill = "grey99", color = NA) +
  geom_sf(data = lake, fill = cmain, color = NA, alpha = .4) +
  geom_sf(
    data = loc_sf, fill = cmain, size = 4, shape = 21, color = "grey99",
    linewidth = 3
  ) +
  geom_richtext(
    data = label_sf, aes(x = X, y = Y, label = label),
    color = "grey20", size = 14 / .pt, lineheight = 1.15, family = fmain,
    hjust = 0, vjust = 1, label.color = NA,
    label.padding = grid::unit(rep(5, 4), "pt"),
    nudge_x = 1e3, nudge_y = -1e3
  ) +
  labs(
    title = 'HELVETI<span style="color: #d8232a;">[c]</span>A',
    subtitle = "Swiss Design, *Swiss History*",
    caption = "Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_size = 16, base_family = fmain) +
  theme(
    plot.background = element_rect(
      fill = "grey10", color = NA
    ),
    plot.title = element_markdown(
      family = fmain, size = 28, face = "bold", hjust = .5, colour = "grey99"
    ),
    plot.subtitle = element_markdown(
      family = fmain, size = 16, hjust = .5, colour = "grey92"
    ),
    plot.caption = element_text(
      size = 12, family = fmain, hjust = .5, colour = "grey87"
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("19_typography", "19_typography.png"),
  device = agg_png,
  width = width, height = height, units = "in", dpi = 320
)
