# package
library(tidyverse)
library(sf)
library(arcgis)
library(ggtext)
library(ragg)
sf_use_s2(FALSE)
proj <- "+proj=natearth +lon_0=0 +datum=WGS84 +units=m +no_defs"

# data
unhcr_url <- "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_marker_presence_p_unhcr/FeatureServer/0"

unhcr_loc <- arc_open(unhcr_url) |>
  arc_select(
    fields = c("iso3", "gis_name")
  ) |>
  st_transform(crs = 4326)

wrl <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3 != "ATA")

office <- unhcr_loc |> 
  count() |> 
  pull(n)

country <- unhcr_loc |> 
  distinct(iso3) |> 
  count() |> 
  pull(n)

# theme
fmain <- "Lato"

cmain <- "#0072bc"

# camcorder
width <- 10
height <- 6

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
ggplot() +
  geom_sf(data = wrl, fill = "#EAE6E4", color = "#A8AAAD", linewidth = 0.1) +
  geom_sf(data = unhcr_loc, color = cmain, alpha = 0.5, size = .9) +
  coord_sf(crs = proj) +
  labs(
    title = "<strong style='color:#0072bc;font-weight:700'>UNHCR Blue</strong> Planet",
    subtitle = glue::glue("Each point represents a UNHCR office location, with <strong style='color:#0072bc;font-weight:700'>{office}</strong> offices across <strong style='color:#0072bc;font-weight:700'>{country}</strong> countries<br>around the world to support <b>people forced to flee</b>."),
    caption = "Data: UNHCR · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_family = fmain, base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = colorspace::lighten(cmain, amount = .95), color = NA
    ),
    plot.title = ggtext::element_markdown(
      family = fmain, face = "plain", color = "black", size = 28
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      family = fmain, color = "grey20",
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = fmain,
      color = "grey60"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt")
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("28_blueplanet", "28_blueplanet.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
