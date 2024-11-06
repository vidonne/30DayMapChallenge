# package
library(sf)
library(tidyverse)
library(rhdx)
library(rgeoboundaries)
library(ggtext)
library(ragg)

# data
displ_idmc <- rhdx::pull_dataset("idmc-event-data-for-lbn") |> 
  rhdx::get_resource(1) |> 
  rhdx::read_resource() |> 
  select(latitude, longitude, displacement_date, figure, locations_name)

displ_month <- displ_idmc |> 
  mutate(month = lubridate::month(displacement_date, label = TRUE)) |> 
  group_by(latitude, longitude,locations_name, month) |> 
  summarise(total_displ = sum(figure, na.rm = TRUE)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  

lbn_adm2 <- rgeoboundaries::gb_adm2(country = "LBN")

# theme
fmain <- "Noto Sans"

ctitle <- "#fafafa"
cweak <- "#878787"

caccent <- "#f4a81d"

# camcorder
width <- 10
height <- 10.5

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
ggplot() +
  geom_sf(data = lbn_adm2,
          fill = "#24273d", color = "#595959") +
  geom_sf(data = displ_month, aes(size = total_displ),
          shape = 21, fill = caccent, color = colorspace::darken(caccent, amount = .4),
          alpha = .75) +
  labs(title = "Internal Displacement in Lebanon",
       subtitle = "Tracking the evolution of internal displacement due to conflict across Lebanon’s districts from May to October 2024.",
       caption = "Data: IDMC from HDX · Map: Cédric Vidonne · #30DayMapChallenge",
       size = "Number of people displaced"
  ) +
  scale_size_area(max_size = 20,
                  labels = scales::label_number(
                    scale_cut = scales::cut_short_scale()
                  ),
                  breaks = c(1e3, 50e3, 250e3, 500e3, 750e3)) +
  facet_wrap(vars(month)) +
  theme_void(base_size = 16, base_family = fmain) +
  theme(
    plot.background = element_rect(
      fill = "#171a33", color = NA
    ),
    plot.title.position = "plot",
    plot.title = element_text(
      family = fmain, size = 22, face = "bold", colour = "#f4a81d",
      hjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      size = 14, family = fmain, color = ctitle, margin = margin(t = 5, b = 10)
    ),
    plot.caption.position = "plot",
    plot.caption = element_text(
      size = 10, family = fmain,  colour = cweak, hjust = 0.5
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.position = 'top',
    legend.title.position = "left",
    legend.title = element_text(
      size = 12, colour = ctitle, hjust = .5
    ),
    legend.text = element_text(
      size = 12, colour = cweak
    ),
    strip.text = element_text(
      hjust = 0, colour = "#f4a81d", margin = margin(b = 5)
    )
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("08_hdx", "08_hdx.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
