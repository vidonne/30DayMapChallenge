# package
library(tidyverse)
library(sf)
library(refugees)
library(scales)
library(ragg)

# setup
sf_use_s2(FALSE)
proj <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

# data
wrl <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(adm0_a3 != "ATA")

top_filter <- refugees::population |>
  filter(year == 2024) |> 
  select(coo_iso, coa_iso, refugees, asylum_seekers, oip) |> 
  pivot_longer(cols = refugees:oip) |> 
  summarise(pop_tot = sum(value, na.rm = TRUE), .by = c(coo_iso)) |> 
  slice_max(order_by = pop_tot, n = 5) |> 
  pull(coo_iso)

coo_coa <- refugees::population |>
  filter(year == 2024 & coo_iso %in% top_filter) |> 
  select(coo_iso, coa_iso, refugees, asylum_seekers, oip) |> 
  pivot_longer(cols = refugees:oip) |> 
  summarise(pop_tot = sum(value, na.rm = TRUE), .by = c(coo_iso, coa_iso)) |> 
  filter(pop_tot > 0) |> 
  mutate(coo_coa = paste0(coo_iso, "-", coa_iso))

coo_coa_long <- coo_coa |> 
  pivot_longer(1:2, names_to = "orgdest", values_to = "iso")

iso_filter <- coo_coa_long |> 
  distinct(iso) |> 
  pull(iso)

wrl_p <- wrl |> 
  mutate(iso_a3 = case_when(
    adm0_a3 == "FRA" ~ "FRA",
    adm0_a3 == "NOR" ~ "NOR",
    TRUE ~ iso_a3
  )) |> 
  filter(iso_a3 %in% iso_filter) |> 
  st_centroid() |> 
  select(iso_a3)

coord <- wrl_p |> 
  st_coordinates()

wrl_coord <- wrl_p |> 
  st_drop_geometry() |> 
  bind_cols(coord)

points <- coo_coa_long |> 
  left_join(wrl_coord, by = c("iso" = "iso_a3")) |> 
  st_as_sf(coords = c("X", "Y"), crs = 4326) |> 
  group_by(coo_coa) |> 
  summarise(pop_tot = mean(pop_tot))

lines <- points |> 
  mutate(coo = str_sub(coo_coa, start = 1, end = 3)) |> 
  filter(pop_tot >= 500) |> 
  st_cast("LINESTRING") |> 
  st_segmentize(dfMaxLength = 50000)

# theme
f1 <- "Roboto"
f2 <- "Roboto Slab"

ctry_color <-  c(
  "AFG" = "#39c671",
  "VEN" = "#f26829",
  "UKR" = "#008dc9",
  "SYR" = "#bd53bd",
  "SSD" = "#8a91f3"
)

# camcorder
width <- 10
height <- 6.2

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
ggplot() +
  geom_sf(data = wrl, fill = "#24273d", color = "#878787", linewidth = 0.1) +
  geom_sf(data = filter(lines, pop_tot > 500),
          aes(alpha = pop_tot, linewidth = pop_tot, color = coo)) +
  coord_sf(crs = proj) +
  scale_linewidth(range = c(.5, 5),
                  labels = label_number(scale_cut = cut_short_scale()),
                  breaks = c(1e5, 5e5, 1e6, 3e6)) + 
  scale_alpha(range = c(.25, .75)) +
  scale_color_manual(values = ctry_color) +
  labs(
    title = "Paths of Displacement from the Top 5 Countries of Origin",
    subtitle = "Visualizing the journeys of those fleeing across borders from <span style='color:#fd8750'>Venezuela</span>, <span style='color:#39c671'>Afghanistan</span>, <span style='color:#ee7cee'>Syria</span>, <span style='color:#25b3ef'>Ukraine</span>, and <span style='color:#999ff5'>South Sudan</span> to destinations around the world.",
    caption = "Source: UNHCR · Map: Cédric Vidonne · #30DayMapChallenge",
    linewidth = "Number of displaced across borders"
  ) +
  theme_void(base_size = 16, base_family = f1) +
  theme(
    plot.background = element_rect(
      fill = "#171a33", color = NA
    ),
    plot.title = ggtext::element_textbox_simple(
      family = f1, face = "bold", color = "white", size = 22
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      family = f2, color = "grey85", size = 14,
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = f1,
      color = "#878787"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(
      color = "grey80", size = 12,
    ),
    legend.title.position = "top",
    legend.title = element_text(
      color = "grey80", size = 12, face = "bold",
      hjust = 0.5
    )
  ) +
  guides(
    alpha = "none",
    color = "none",
    linewidth =  guide_legend(
      override.aes = list(
        fill = NULL,
        alpha = .6,
        color = "white"
      ))
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("02_lines", "02_lines.png"),
  device = agg_png,
  width = width, height = height, units = "in", dpi = 320
)
