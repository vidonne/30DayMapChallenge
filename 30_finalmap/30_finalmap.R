library(sf)
library(elevatr)
library(tidyverse)
library(terra)
library(ggnewscale)
library(ggshadow)
library(tidyterra)
library(giscoR)
library(ragg)
library(magick)

### TODO add cities?

# data

# lake and river from BFS
lake <- st_read(here::here("30_finalmap", "input", "k4seenyyyymmdd11_ch2007Poly.shp"))
river_1 <- st_read(here::here("30_finalmap", "input", "k4flusyyyymmdd11_ch2007.shp"))
river_2 <- st_read(here::here("30_finalmap", "input", "k4flusyyyymmdd22_ch2007.shp"))
river_3 <- st_read(here::here("30_finalmap", "input", "k4flusyyyymmdd33_ch2007.shp"))
river_main <- bind_rows(river_1, river_2, river_3)
river_sec <- st_read(here::here("30_finalmap", "input", "k4flusyyyymmdd44_ch2007.shp"))

# Switzerland 
che <- gisco_get_countries(country = "Switzerland", resolution = "03") |> 
  st_transform(crs = st_crs(lake))

# bounding box
bbox <- che |> 
  st_buffer(dist = 3e4) |> 
  st_bbox()

country <- gisco_get_countries(country = c("France", "Italy", "Germany", "Austria", "Liechtenstein"),
                               resolution = "03") |> 
  st_transform(crs = st_crs(lake))

country_mask <- country |> 
  st_union()

# dem
mdt <- get_elev_raster(bbox, z = 10)

mdt <- rast(mdt)

# reproject
mdt <- project(mdt, crs(lake))

mdtdf <- as.data.frame(mdt, xy = TRUE)
names(mdtdf)[3] <- "alt"

# saveRDS(mdtdf, here::here("30_finalmap", "input", "mdtdf.Rda"))

# hillshades
# estimate the slope
sl <- terrain(mdt, "slope", unit = "radians")
# estimate the aspect or orientation
asp <- terrain(mdt, "aspect", unit = "radians")
# calculate the hillshade effect with 45º of elevation
hill_single <- shade(sl, asp, 
                     angle = 45, 
                     direction = 300,
                     normalize= TRUE)

hilldf_single <- as.data.frame(hill_single, xy = TRUE)

# saveRDS(hilldf_single, here::here("30_finalmap", "input", "hilldf_single.Rda"))

# pass multiple directions to shade()
hillmulti <- map(c(270, 15, 60, 330), function(dir){ 
  shade(sl, asp, 
        angle = 45, 
        direction = dir,
        normalize= TRUE)}
)
# create a multidimensional raster and reduce it by summing up
hillmulti <- rast(hillmulti) |> sum()

# convert the hillshade to xyz
hillmultidf <- as.data.frame(hillmulti, xy = TRUE)

# saveRDS(hillmultidf, here::here("30_finalmap", "input", "hillmultidf.Rda"))

# theme
fmain <- "Garamond"
theme_set(theme_void(base_size = 16, base_family = fmain))
theme_update(
  plot.background = element_rect(fill = "white", color = "white"),
  plot.margin = margin(0, 0, 0, 0),)

# colors
che_bnd <- "#FFEBBE"
ctry_bg <- "#45677E"
ctry_bg_stroke <- "#004C73"
ctry_bnd <- "#FFEBAF"
water_fill <- "#48797E"
water_str <- "#D9CE9D"
dem <- c(alpha("#759090", 0.85), alpha("#759090", 0.85), alpha("#FFDFB8", 0.1), alpha("#EDFFFF", 0))
hill_multi <- c("#774246", alpha("#151B1E", 0.9), alpha("#0084A8", 0.5), 
                alpha("#AB6BD4", 0), alpha("#FFC17F", 0.15), alpha("#FFDC73", 0.8))
hill_multi_breaks <- c(0, 0.4, 0.58, 0.7, 0.8, 1)
hill_single <- c("#151B1E", alpha("#151B1E", 0.8), alpha("#0084A8", 0.5), 
                alpha("#AB6BD4", 0), alpha("#FFC17F", 0.25), alpha("#FFE9A9", 0.8), "#FFF2CD")
hill_single_breaks <- c(0, 0.31, 0.55, 0.66, 0.73, 0.86, 1)

# camcorder
width <- 10
height <- 10 * 0.6868096

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

map_bg <- ggplot() +
  #hill multi
  geom_raster(data = hillmultidf,
              aes(x, y, fill = sum),
              show.legend = FALSE) +
  scale_fill_gradientn(colours = hill_multi,
                       breaks = hill_multi_breaks) +
  new_scale_fill() +
  #hill multi
  geom_raster(data = hilldf_single,
              aes(x, y, fill = hillshade),
              show.legend = FALSE) +
  scale_fill_gradientn(colours = hill_single,
                       breaks = hill_single_breaks) +
  new_scale_fill() +
  #dem
  # geom_raster(
  #   data = mdtdf,
  #   aes(x, y, fill = alt),
  #   show.legend = F
  # ) +
  # scale_fill_gradientn(colours = dem,
  #                      breaks = c(0, 0.06, 0.63, 1)) +
  # new_scale_fill() +
  # rivers sec
  geom_sf(data = river_sec,
          color = water_str,
          linewidth = .5) +
  geom_sf(data = river_sec,
          color = water_fill,
          linewidth = .3) +
  # rivers main
  geom_sf(data = river_main,
          color = water_str,
          linewidth = .8) +
  geom_sf(data = river_main,
          color = water_fill,
          linewidth = .5) +
  # lakes
  geom_sf(data = lake,
          color = water_str,
          fill = water_fill,
          linewidth = .2) +
  #dem
  geom_raster(
    data = mdtdf,
    aes(x, y, fill = alt),
    show.legend = F
  ) +
  scale_fill_gradientn(colours = dem,
                       breaks = c(0, 0.06, 0.63, 1))


map <- map_bg +
# ggplot() +
  # country mask
  geom_sf(data = country_mask,
          fill = alpha(ctry_bg, 0.2),
          color = alpha(ctry_bg_stroke, 0.5)) +
  geom_sf(data = country_mask |> st_buffer(-0.75e4),
          fill = alpha(ctry_bg, 0.1),
          color = NA) +
  geom_sf(data = country_mask |> st_buffer(-1.25e4),
          fill = alpha(ctry_bg, 0.1),
          color = NA) +
  geom_sf(data = country_mask |> st_buffer(-1.55e4),
          fill = alpha(ctry_bg, 0.1),
          color = NA) +
  geom_sf(data = country_mask |> st_buffer(-2.25e4),
          fill = alpha(ctry_bg, 0.1),
          color = NA) +
  geom_sf(data = country_mask |> st_buffer(-2.75e4),
          fill = alpha(ctry_bg, 0.1),
          color = NA) +
  geom_sf(data = country_mask |> st_buffer(-3.25e4),
          fill = alpha(ctry_bg, 0.1),
          color = NA) +
  geom_sf(data = country_mask |> st_buffer(-3.75e4),
          fill = alpha(ctry_bg, 0.1),
          color = NA) +
  geom_sf(data = country_mask |> st_buffer(-1e3),
          fill = NA,
          color = alpha(ctry_bg_stroke, 0.5),
          linewidth = 1.5) +
  geom_sf(data = country_mask |> st_buffer(-2e3),
          fill = NA,
          color = alpha(ctry_bg_stroke, 0.25),
          linewidth = 2) +
  geom_sf(data = country_mask |> st_buffer(-3e3),
          fill = NA,
          color = alpha(ctry_bg_stroke, 0.15),
          linewidth = 2.5) +
  geom_sf(data = country_mask |> st_buffer(-4e3),
          fill = NA,
          color = alpha(ctry_bg_stroke, 0.1),
          linewidth = 3) +
  # country borders
  geom_sf(data = country,
          fill = NA,
          color = alpha(ctry_bnd, 0.4),
          linewidth = .5) +
  # che boundaries
  geom_sf(data = che,
          fill = NA,
          color = che_bnd,
          linewidth = 0.8) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax))

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("30_finalmap", "base_map.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)

# use magick for text
img <- image_read(here::here("30_finalmap", "base_map.png"))


img |> 
  image_crop(gravity = "southeast",
             geometry = "3200x2197-30-15") |> 
  image_crop(gravity = "northwest",
             geometry = "3200x2197-30-15") |> 
  image_annotate("SWITZERLAND",
                 gravity = "northwest",
                 location = "+100+80",
                 color = alpha("white", 0.95),
                 size = 136,
                 weight = 700,
                 font = "Garamond") |> 
  image_annotate("Data: BFS · Map: Cédric Vidonne · #30DayMapChallenge",
                 gravity = "southeast",
                 location = "+100+80",
                 font = "Garamond",
                 color = alpha("white", .8),
                 size = 48,
                 weight = 400,) |> 
  image_write(here::here("30_finalmap", "30_finalmap.png"))

  