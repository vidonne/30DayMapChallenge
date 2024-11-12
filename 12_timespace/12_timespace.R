# package
library(tidyverse)
library(refugees)
library(geofacet)
library(scales)
library(ggrepel)
library(ragg)

# data
year_span <- 30

ctry_list <- eu_grid1$code

ref_pop <- refugees::population |> 
  filter(year >= 2024 - year_span) |> 
  mutate(code = countrycode::countrycode(coa_iso, origin = 'iso3c', destination = 'iso2c'),
         code = case_when(
           code == "GR" ~ "EL",
           code == "GB" ~ "UK",
           TRUE ~ code)) |> 
  filter(code %in% ctry_list) |> 
  select(year, code, refugees, asylum_seekers, oip) |> 
  pivot_longer(refugees:oip) |> 
  summarise(pop = sum(value, na.rm = TRUE), .by = c("year", "code")) |> 
  mutate(label = case_when(
    pop >= 1e6 ~ paste0(round(pop/1e6, digits = 1), "M"),
    pop >= 1e3 ~ paste0(round(pop/1e3, digits = 0), "K"),
    TRUE ~ ""
  ))


# theme
fmain <- "Lato"

cmain <- "#0072bc"

# camcorder
width <- 10
height <- 10

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)


# map
ggplot(ref_pop,
       aes(x = year, y = pop)) +
  geom_line(color = cmain, linewidth = .85) +
  geom_label_repel(data = filter(ref_pop, year == min(year) & pop >= 1000),
                   aes(x = year, y = pop, label = label),
             color = "#493f38", label.size = NA, size = 10 / .pt, direction = "y",
             fill = "#FFFFFFD9") +
  geom_label_repel(data = filter(ref_pop, year == max(year) & pop >= 1000),
                   aes(x = year, y = pop, label = label),
                   color = "#493f38", label.size = NA, size = 10 / .pt, direction = "y",
                   fill = "#FFFFFFD9") +
  facet_geo(vars(code), grid = "eu_grid1", label = "name", scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(.2, .2))) +
  scale_x_continuous(breaks = c(min(ref_pop$year), max(ref_pop$year)),
                     labels = c("'94", "'24"),
                     expand = expansion(add = c(10, 10))) +
  labs(
    title = "Trends of Refugee Arrivals to EU Countries",
    subtitle = "The evolution of 30 years of arrivals in the European Union, highlighting changing patterns over time and space.",
    caption = "Data: UNHCR · Map: Cédric Vidonne · #30DayMapChallenge",
  ) +
  theme_minimal(base_family = fmain, base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "grey99", color = NA
    ),
    plot.title = element_text(
      family = fmain, face = "bold", color = "#1b1b1a", size = 22
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      family = fmain, color = "#493f38", size = 14,
      margin = margin(t = 0, b = 15)
    ),
    plot.caption = element_text(
      size = 10, hjust = 0, family = fmain,
      color = "#a99f96", margin = margin(t = 10)
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
    strip.text = element_text(
      size = 12, face = "bold", color = "#71665e",
    ),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      size = 10, color = "#71665e",
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "#a99f96", linewidth = .2
    ),
    panel.grid.minor.x = element_blank(),
  )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("12_timespace", "12_timespace.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
