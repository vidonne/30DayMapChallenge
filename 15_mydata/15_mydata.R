# package
library(tidyverse)
library(rStrava)
library(sf)
library(sfext)
library(ggmap)
library(osmdata)
library(patchwork)
library(ragg)

# data
app_name <- Sys.getenv("STRAVA_APP_NAME")
app_client_id  <- Sys.getenv("STRAVA_CLIENT_ID") 
app_secret <- Sys.getenv("STRAVA_SECRET")

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, 
                                            app_client_id, 
                                            app_secret, 
                                            app_scope="activity:read_all"))

my_acts <- get_activity_list(stoken,
                             before = as.Date('2022-08-23'))

im_ids <- c(
  swim_id = "5836469296",
  t1_id = "5836469205",
  bike_id = "5836470188",
  t2_id = "5836469258",
  run_id = "5836469415"
)

im_data <- get_activity_streams(my_acts,
                                stoken,
                                id = im_ids)

im_data_sf <- im_data |>
  as_tibble() |>
  st_as_sf(coords = c("lng", "lat"),
           crs = 4326, remove = FALSE)

# theme
fmain <- "Noto Sans"
cswim <- "#008dc9"
cbike <- "#f26829"
crun <- "#40bf73"
ctran <- "#cccccc"

# camcorder
width <- 10
height <- 10

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# main map
bb <- st_bbox_ext(im_data_sf, dist = 1e3, asp = 2)
names(bb) <- c("left", "bottom", "right", "top")

bg <- get_map(location = bb,
                    source = "stadia",
                    maptype = "stamen_toner_lite", 
                    color = "bw")

ggmap(bg) +
  geom_point(data = arrange(im_data, desc(id)),
             aes(x = lng, y = lat, colour = id),
             size = .8) +
  scale_color_manual(values = c(ctran, ctran, cswim, crun, cbike))  +
  labs(title = 'A Day in Motion: Ironman Copenhagen 2021',
       subtitle = "Mapping my Ironman experience: <b style='color:#25b3ef'>3.8 km swim</b>, <b style='color:#fd8750'>180 km bike</b>, and <br><b style='color:#39c671'>42.2 km run</b> in Copenhagen.",
       caption = "Map: Cédric Vidonne · #30DayMapChallenge",
       ) +
  theme_void(base_size = 16, base_family = fmain) +
  theme(
    plot.background = element_rect(
      fill = "#171a33", color = NA
    ),
    plot.title = element_text(
      family = fmain, face = "bold", color = "grey95", size = 28
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      family = fmain, color = "grey85", size = 18,
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_text(
      size = 12, hjust = 0.5, family = fmain,
      color = "grey75"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(rep(15, 4), unit = "pt"),
    legend.position = "none"
    )

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("15_mydata", "15_mydata.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
