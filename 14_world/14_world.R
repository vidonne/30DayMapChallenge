# package
library(tidyverse)
library(readxl)
library(sf)
library(ggtext)
library(ragg)
sf_use_s2(FALSE)
proj <- "+proj=natearth +lon_0=0 +datum=WGS84 +units=m +no_defs"

# data
wrl <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3 != "ATA")

cod <- read_excel(here::here("14_world", "input", "cods.xlsx")) |> 
  janitor::clean_names() |> 
  filter(!(is.na(ab_quality)))

wrl_cod <- wrl |> 
  left_join(cod, by = c("iso_a3" = "code"))
  
# theme
fmain <- "Roboto"

# camcorder
width <- 10
height <- 6

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
ggplot() +
  geom_sf(data = wrl_cod, 
          aes(fill = ab_quality),
          color = "white", linewidth = 0.2) +
  coord_sf(crs = proj) +
  scale_fill_manual(values = c("#72bf44", "#cee3a0"),
                    labels = c("Enhanced", "Standard"),
                    breaks = c("cod-enhanced", "cod-standard"),
                    na.value = "#dddad7") +
  labs(
    title = "A World of CODs",
    subtitle = "Mapping the <b>Availability of Common Operational Datasets</b> (CODs) for Administrative Boundaries, essential tools in humanitarian response that provide consistent geographic references, enable coordinated action, and support informed decision-making across response efforts.",
    fill = "COD Quality",
    caption = "Data: UNOCHA · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_family = fmain, base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    plot.title = element_text(
      family = fmain, face = "bold", color = "grey15", size = 24
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      family = fmain, color = "grey30", size = 14,
      margin = margin(t = 5, b = 0)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = fmain,
      color = "grey60"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.justification = c(0,0),
    legend.position = "inside",
    legend.position.inside = c(0, 0),
    legend.title = element_text(
      size = 12, face = "bold", color = "grey30"
    ),
    legend.text = element_text(
      size = 12, color = "grey30"
    ),
    legend.key.size = unit(.8, "lines")
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("14_world", "14_world.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
