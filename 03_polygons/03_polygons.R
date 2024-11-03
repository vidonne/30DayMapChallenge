# package
library(tidyverse)
library(sf)
library(refugees)
library(cartogram)
library(rcartocolor)
library(scales)
library(ragg)

# setup
sf_use_s2(FALSE)
proj <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

# data
wrl <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(adm0_a3 != "ATA") |> 
  mutate(iso_a3 = case_when(
    adm0_a3 == "FRA" ~ "FRA",
    adm0_a3 == "NOR" ~ "NOR",
    TRUE ~ iso_a3
  )) 

coo_pop <- refugees::population |> 
  filter(year == 2024) |> 
  select(coo_iso, refugees, asylum_seekers, oip) |> 
  pivot_longer(-coo_iso) |> 
  summarise(pop_tot = sum(value, na.rm = TRUE), .by = coo_iso)

pop_sf <- wrl |> 
  left_join(coo_pop, by = c("iso_a3" = "coo_iso")) |> 
  select(admin, iso_a3, pop_tot) |> 
  filter(!is.na(pop_tot)) |> 
  st_transform(proj)

pop_carto <- cartogram_cont(pop_sf, "pop_tot")

test <- pop_carto |> 
  mutate(class = classInt::classify_intervals(pop_tot, 
                                          n = 5, style = "headtails"),
         pretty = case_when(
           pop_tot <= 200000 ~ "5-200k",
           pop_tot > 200000 | pop_tot <= 1.5e6 ~ ">200k-1.5M",
           pop_tot > 1.5e6 | pop_tot <= 5e6 ~ ">1.5-5M",
           pop_tot > 5e6 ~ ">5M"
         ))

# theme
f1 <- "Noto Sans"
# f2 <- "Roboto Slab"


# camcorder
width <- 10
height <- 6

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# map
ggplot() +
  # geom_sf(data = pop_sf) +
  geom_sf(data = test, 
          aes(fill = class), color = "grey40", linewidth = .2) +
  scale_fill_carto_d(palette = "BrwnYl",
                     name = "Number of people fleeing across borders",
                     label = c("5-227K", "227K-1.6M", "1.6M-5.2M", ">5.2M")) +
  labs(
    title = "Origins of Cross-Border Displacement: A Global Cartogram",
    caption = "Source: UNHCR · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_void(base_size = 16, base_family = f1) +
  theme(
    plot.background = element_rect(
      fill = "grey90", color = NA
    ),
    plot.title = ggtext::element_textbox_simple(
      family = f1, face = "bold", color = "grey5", size = 22,
      hjust = .5, halign = .5, margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = f1,
      color = "grey35"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.position = "top",
    legend.title = element_text(
      hjust = .5, size = 14, color = "grey15"
    ),
    legend.title.position = "top",
    legend.text = element_text(
      hjust = .5, size = 12, color = "grey25"
    ),
    legend.key.width = unit(.5, "cm"),
    legend.key.height = unit(.35, "cm")
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("03_polygons", "03_polygons.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
