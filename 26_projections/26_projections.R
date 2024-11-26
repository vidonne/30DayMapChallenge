library(ggplot2)
library(rerddap)
library(ncdf4)
library(tidyterra)
library(patchwork)
library(ragg)

# Largely based on the example here: https://github.com/rtlemos/spilhaus

# theme
fmain <- "IBM Plex Mono"
col_bg <- "#222222"
col_fg <- alpha("white", 0.87)

theme_set(theme_void(base_size = 16, base_family = fmain))
theme_update(
  plot.background = element_rect(fill = col_bg, color = col_bg),
  plot.margin = margin(0, 0, 0, 0),
  legend.title = element_text(color = col_fg, size = 11, face = "bold"),
  legend.text = element_text(color = col_fg, size = 10),
  plot.tag.location = "panel",
  plot.tag.position = "top",
  plot.tag = element_text(colour = "white", margin = margin(t = 10), size = 12, hjust = 0.5, face = "plain")
)

source(here::here("26_projections", "spilhaus.R")) # load the Spilhaus projection script

# auxiliary function to extract netCDF file from Coastwatch
download_sst_data = function(start_date, end_date) {
  myInfo = rerddap::info('NOAA_DHW_monthly', url='https://coastwatch.pfeg.noaa.gov/erddap/')
  myData = rerddap::griddap(datasetx = myInfo,
                            fields = "sea_surface_temperature",
                            latitude = c(-89.975, 89.975),
                            longitude = c(-179.975, 179.975),
                            time = c(start_date, end_date))
  da = ncdf4::nc_open(myData$summary$filename)
  return(da)
}

# auxiliary function to extract a list (length = number of instants) of snapshots (7200x3600 pixels) from the netCDF file
extract_sst_data = function(da, lonlat) {
  lons = sort(ncdf4::ncvar_get(da, "longitude"))
  lats = sort(ncdf4::ncvar_get(da, "latitude"))
  times = ncdf4::ncvar_get(da, "time")
  
  dlon = lons[2] - lons[1]
  dlat = lats[2] - lats[1]
  ln = as.integer(round((lonlat[,1] - lons[1]) / dlon) + 1)
  la = as.integer(round((lonlat[,2] - lats[1]) / dlat) + 1)
  get_chunk = function(timepoint) {
    sst = ncdf4::ncvar_get(da, "sea_surface_temperature",
                           start = c(1,1,timepoint), count = c(7200, 3600, 1))
    sst = sst[,ncol(sst):1]
    chunk = sst[ln + (la - 1) * dim(sst)[1]]
    return(chunk)
  }
  sst_data = mapply(1:length(times), FUN=function(timepoint) {get_chunk(timepoint)})
  return(sst_data)
}

# first we create a data frame with NxN pixels
spilhaus_jan = make_spilhaus_xy_gridpoints(spilhaus_res=1000)
# convert the spilhaus coordinates into Mercator coordinates
lonlat = from_spilhaus_xy_to_lonlat(spilhaus_jan$x, spilhaus_jan$y)

# download the netCDF data from Coastwatch
da_jan = download_sst_data("2024-01-01", "2024-01-01")

# extract the SST data for the required lats and lons
spilhaus_jan$z = extract_sst_data(da_jan, lonlat)
# mask
spilhaus_jan$l = is.na(spilhaus_jan$z)
# prettify
pretty_spilhaus_jan = pretify_spilhaus_df(spilhaus_jan)

# first we crate a data frame with NxN pixels
spilhaus_jul = make_spilhaus_xy_gridpoints(spilhaus_res=1000)
# convert the spilhaus coordinates into Mercator coordinates
lonlat = from_spilhaus_xy_to_lonlat(spilhaus_jul$x, spilhaus_jul$y)

# download the netCDF data from Coastwatch
da = download_sst_data("2024-07-01", "2024-07-01")

# extract the SST data for the required lats and lons
spilhaus_jul$z = extract_sst_data(da, lonlat)
# mask
spilhaus_jul$l = is.na(spilhaus_jul$z)
# prettify
pretty_spilhaus_jul = pretify_spilhaus_df(spilhaus_jul)

# maps
p_jan <- ggplot(data=pretty_spilhaus_jan, aes(x=x, y=y, fill=z)) +
  geom_tile() +
  scale_fill_grass_c(palette = "haxby", name = "SST[°C]") +
  coord_equal() +
  labs(tag = "January '24") +
  theme(legend.position = c(0.1, 0.2))


p_jul <- ggplot(data=pretty_spilhaus_jul, aes(x=x, y=y, fill=z)) +
  geom_tile() +
  scale_fill_grass_c(palette = "haxby") +
  coord_equal() +
  labs(tag = "July '24") +
  theme(legend.position = "none")

# camcorder
width <- 10
height <- 5.8

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

# patchwork
title <- "Seasonal Ocean Temperatures"
caption <- "Data: NOAA · Map: Cédric Vidonne · #30DayMapChallenge"

pgroup <- p_jan + p_jul

pgroup +
  plot_annotation(
    title = title,
    caption = caption,
    theme = theme(
      plot.title = element_text(
      color = "white", size = 24, face = "bold", hjust = 0.5),
      plot.caption = element_text(
        colour = alpha("white", 0.7), size = 12, hjust = 0.5
      ),
      plot.margin = margin(rep(15, 4), unit = "pt"),
    
    )
  )
  

# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("26_projections", "26_projections.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
