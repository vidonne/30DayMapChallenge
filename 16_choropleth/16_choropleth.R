# package
library(tidyverse)
library(sf)
library(ggtext)
library(refugees)
library(biscale)
library(cowplot)
library(ragg)

proj <- "+proj=natearth +lon_0=0 +datum=WGS84 +units=m +no_defs"

# data
wrl <- rgeoboundaries::gb_adm0() |>
  filter(shapeGroup != "ATA")

pop <- refugees::population |> 
  filter(year == 2024) |> 
  select(coo_iso, coa_iso, refugees, asylum_seekers, oip) |> 
  pivot_longer(cols = refugees:oip)

coo_pop <- pop |> 
  summarise(coo_pop = sum(value, na.rm = TRUE), .by = coo_iso)
  
coa_pop <- pop |> 
  summarise(coa_pop = sum(value, na.rm = TRUE), .by = coa_iso)

join <- wrl |> 
  left_join(coo_pop, by = c("shapeGroup" = "coo_iso")) |> 
  left_join(coa_pop, by = c("shapeGroup" = "coa_iso")) |> 
  select(shapeName, coo_pop, coa_pop)

bi_sf <- bi_class(join, x = coo_pop, y = coa_pop, style = "quantile", dim = 3)
  
# theme
fmain <- "Roboto"

# camcorder
width <- 10
height <- 5.8

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
map <- ggplot() +
  geom_sf(data = bi_sf, 
          aes(fill = bi_class),
          color = "grey99", linewidth = 0.15,
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink2", dim = 3,
                na.value = "grey87") +
  coord_sf(crs = proj) +
  labs(
    title = "Global Displacement Patterns in 2024",
    subtitle = "Comparing numbers of people displaced across borders: where they are from and where they are hosted.",
    caption = "Data: UNHCR · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  bi_theme(base_family = fmain, base_size = 16, bg_color = NA) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    plot.title = element_text(
      family = fmain, face = "bold", color = "grey15", size = 24,
      hjust = 0, margin = margin(b = 8)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      family = fmain, color = "grey30", size = 14, face = "plain",
      margin = margin(t = 0, b = 5)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, family = fmain,
      color = "grey60"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
  )

# legend
legend <- bi_legend(pal = "GrPink2",
          dim = 3,
          xlab = "More displaced from ",
          ylab = "More displaced to ",
          size = 9,
          pad_color = "grey99") +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ))


# composition
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, -0.05, 0.05, 0.32, 0.32)

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("16_choropleth", "16_choropleth.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
