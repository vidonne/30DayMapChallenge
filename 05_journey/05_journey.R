# package
library(tidyverse)
library(sf)
library(ggtext)
library(ragg)

# setup
sf_use_s2(FALSE)
proj <- "+proj=eqdc +lon_0=-40 +lat_1=17.5 +lat_2=47.5 +lat_0=32.5 +datum=WGS84 +units=m +no_defs"

# data
wrl <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3 != "ATA")

wrl_buff <- wrl |> 
  st_buffer(dist = .6)

track <- st_read(here::here("05_journey", "track.geojson"))

loc <- track |> 
  filter(st_is(geometry, "POINT")) |> 
  mutate(accent = case_when(
    name == "Saint-Malo" | name == "Pointe-à-Pitre" ~ "Yes",
    TRUE ~ "No"
  ))

line <- track |> 
  filter(st_is(geometry, "LINESTRING"))
# theme
fmain <- "Source Sans 3"

cpoly <- "#EBE2C8"
cbuff <- "#D8F0F5"

# camcorder
width <- 10
height <- 7.2

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
bbox <- st_bbox(c(xmin = -80, xmax = 0, ymin = 10, ymax = 55), crs = st_crs(4326))

label <- tribble(
  ~x, ~y, ~label, ~hjust,
  -63, 18.5, "**Pointe-à-Pitre**<br><i style='font-size:14px'>20 Nov. 2003</i>", 0,
  -0.5, 48, "**Saint-Malo**<br><i style='font-size:14px;'>11 Oct. 2003</i>", 0,
)

ggplot() +
  geom_sf(
    data = wrl_buff,
    color = "transparent",
    fill = cbuff,
  ) +
  geom_sf(
    data = wrl,
    fill = cpoly,
    color = "grey15"
  ) +
  geom_sf(data = line, color = "#0C120C", linewidth = 1) +
  geom_sf(data = filter(loc, accent == "Yes"),
          size = 4, fill = "#832232", shape = 21, color = "white", stroke = 1) +
  geom_sf(data = filter(loc, accent == "No"),
          size = 3, fill = "#832232", shape = 21, color = "white", stroke = .5) +
  geom_richtext(data = label, aes(x = x, y = y, label = label, hjust = hjust),
                label.size = NA, fill = "grey99", size = 14/ .pt) +
  annotate(geom = "label", label = "NORTH\nATLANTIC\nOCEAN", hjust = .5,
           x = -40, y = 30, lineheight = 1, fontface = "italic", color = colorspace::darken(cbuff, amount = .4),
           family = "Lato", label.size = NA, fill = "grey99", size = 12 / .pt) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax),
    default_crs = 4326,
    crs = proj
  ) +
  scale_x_continuous(breaks = seq(-80, 0, 10)) +
  labs(tag = "**<span style='font-size: 32px; margin-bottom: 12px;'>TRANSAT 2003**<br><i style='font-size: 18px; color: #000000CC;'>Tracing My Sailing Journey Across the Atlantic Ocean</i>",
       caption = "Map: Cédric Vidonne · #30DayMapChallenge") +
  theme_minimal(base_size = 16, base_family = fmain) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    # panel.ontop = TRUE,
    panel.grid = element_line(
      colour = "grey25", linewidth = .2
    ),
    axis.text = element_text(
      color = "grey35", size = 10
    ),
    axis.title = element_blank(),
    plot.tag = element_textbox_simple(
      family = fmain, lineheight = .9, colour = "grey10",
      fill = cpoly, maxwidth = unit(6.25, "cm"), padding = margin(12, 8, 12, 8), halign = .5
    ),
    plot.tag.location = "panel",
    plot.caption = element_text(
      hjust = 0.5, size = 12, color = "grey25"
    ),
    plot.margin = margin(rep(8, 4), unit = "pt")
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("05_journey", "05_journey.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
