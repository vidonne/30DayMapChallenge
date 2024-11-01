# package
library(tidyverse)
library(sf)
library(arcgis)
library(ragg)
sf_use_s2(FALSE)
proj <- "+proj=natearth +lon_0=0 +datum=WGS84 +units=m +no_defs"

# data
ref_url <- "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_prp_p_unhcr_PoC/FeatureServer/0"

ref_loc <- arc_open(ref_url) |>
  arc_select(
    fields = c("gis_name", "pop_type", "loc_subtype")
  ) |>
  filter(
    loc_subtype == 38 | loc_subtype == 39,
    pop_type == 52 | pop_type == 55
  ) |>
  st_transform(crs = 4326)

wrl <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3 != "ATA")

# theme
fmain <- "Work Sans"
ftitle <- "Poppins"

cmain <- "#0072bc"
cpoly <- "#E8EADF"
cline <- "#5E6472"
cbg <- "#A7D5D0"
ctext <- "#00203F"

# camcorder
camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = 10,
  height = 5.5, units = "in", dpi = 320
)

# map
ggplot() +
  geom_sf(data = wrl, fill = cpoly, color = cline, linewidth = 0.1) +
  geom_sf(data = ref_loc, color = cmain, alpha = 0.25, size = .8) +
  coord_sf(crs = proj) +
  labs(
    title = "Refugee and Asylum-Seeker Settlements Worldwide",
    subtitle = "Exploring where people seeking safety have found shelter across the globe.",
    caption = "Source: UNHCR · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_family = fmain, base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    plot.title = element_text(
      family = ftitle, face = "bold", color = ctext, size = 24
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_markdown(
      family = fmain, color = colorspace::lighten(ctext, amount = .3),
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = fmain,
      color = colorspace::lighten(ctext, amount = .5)
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt")
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("01_points", "01_points.png"),
  device = agg_png,
  width = 10, height = 5.5, units = "in", dpi = 320
)
