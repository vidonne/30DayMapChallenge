library(tidyverse)
library(refugees)
library(sf)
library(cartogram)
library(ggforce)
library(ggtext)
library(scales)
library(ragg)

# tuto from https://r-graph-gallery.com/web-dorling-cartogram-with-R.html
# data 
wrl <- rgeoboundaries::gb_adm0()

iso_afr <- rnaturalearth::ne_countries(continent = "Africa")$iso_a3

afr <- wrl |> 
  filter(shapeGroup %in% iso_afr | shapeGroup == "ESH") |> 
  st_transform(crs="ESRI:102011")

pop <- refugees::population |> 
  filter(year == 2024) |> 
  select(coa_iso, refugees, asylum_seekers, oip, idps) |> 
  summarise(
    ref = sum(refugees, na.rm = TRUE),
    asy = sum(asylum_seekers, na.rm = TRUE),
    oip = sum(oip, na.rm = TRUE),
    inside = sum(idps, na.rm = TRUE),
    .by = coa_iso
  ) |> 
  mutate(
    total = ref + asy + oip +inside,
    outside = ref + asy + oip
  ) |> 
  select(shapeGroup = coa_iso, total, outside, inside)
  
pop_sf <- afr |> 
  left_join(pop) |> 
  drop_na(total)

dorl <- cartogram::cartogram_dorling(
  pop_sf, weight='total', k = 5,
  m_weight = 1, itermax = 1000
)

# Compute area and radius for each circus of the cartogram
dorl<-dorl|>
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl|>
  st_centroid()|>
  st_coordinates()

# Combine data
dorl2 <- tibble(dorl,X=centr[,1],Y=centr[,2])|>
  arrange(-total)

dorl2 <- dorl2 |> 
  mutate(
    ratio_outside = outside/total,
    ratio_inside = inside/total
  ) |> 
  mutate(
    rad_outside=sqrt(rad*rad*ratio_outside),
    rad_inside=sqrt(rad*rad*ratio_inside)
  )

# circle function
circleFun <- function(
    center=c(0,0),   # center of the circle 
    diameter=1,      # diameter 
    npoints=100,     # number of points to draw the circle
    start=0, end=2   # start point/end point
){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  tb <- tibble(
    x = center[1] + diameter / 2 * cos(tt), 
    y = center[2] + diameter / 2 * sin(tt)
  )
  return(tb)
}

# Half circle for outside
half_outside <- bind_cols(
  shapeGroup = rep(dorl2$shapeGroup[1],100),
  circleFun(
    c(dorl2$X[1],dorl2$Y[1]),dorl2$rad_outside[1]*2, start=1.5, end=2.5
  ))

# Half circle for inside
half_inside <- bind_cols(
  shapeGroup = rep(dorl2$shapeGroup[1],100),
  circleFun(
    c(dorl2$X[1],dorl2$Y[1]),dorl2$rad_inside[1]*2, start=0.5, end=1.5
  ))

for (i in 2:dim(dorl2)[1]){
  
  # Draw for outside
  temp_outside <- bind_cols(
    shapeGroup = rep(dorl2$shapeGroup[i],100),
    circleFun(
      c(dorl2$X[i],dorl2$Y[i]),dorl2$rad_outside[i]*2, start=1.5, end=2.5
    ))
  # Draw for inside
  temp_inside <- bind_cols(
    shapeGroup = rep(dorl2$shapeGroup[i],100),
    circleFun(
      c(dorl2$X[i],dorl2$Y[i]),dorl2$rad_inside[i]*2, start=0.5, end=1.5
    ))
  
  half_outside<-half_outside |> 
    bind_rows(temp_outside)
  
  half_inside<-half_inside |> 
    bind_rows(temp_inside)
}

# top 3 label
lab <- dorl2 |> 
  slice_max(order_by = total, n = 3) |> 
  mutate(
    name = case_when(
      shapeGroup == "COD" ~ "DR. of the Congo",
      TRUE ~ shapeName
    ),
    tot_lab = number(total, scale_cut = cut_short_scale()),
    inside_lab = number(inside, scale_cut = cut_short_scale()),
    outside_lab = if_else(shapeGroup == "ETH",paste0(round(outside/ 1e6, 1), "M"), paste0(round(outside/ 1000, 0), "K")),
    label = paste0(name,
                   "<br>", tot_lab, "<br>",
                   "<b style='color:#f4a81d'>", inside_lab, "</b> | <b style='color:#25b3ef'>", outside_lab, "</b>"),
  )


# theme
fmain <- "Fira Sans"
cbg <- "#171a33"
cbgweak <- "#24273d"
cfg <- "#fafafa"
cfgweak <- "#878787"
cfweakest <- "#595959"
cout <- "#008dc9"
cin <- "#f4a81d"



# camcorder
width <- 10
height <- 12

camcorder::gg_record(
  dir = "map-temp", device = agg_png, width = width,
  height = height, units = "in", dpi = 320
)

subtitle <- glue::glue("Each bubble split to show the contrast between ",
                       "<b><span style='color:#f4a81d'>internally displaced people</span> and ",
                       "<span style='color:#25b3ef'>refugees hosted within the country</span></b>.")

# map
ggplot() +
  geom_sf(data = afr, fill = cbgweak, color = cfgweak) +
  ggforce::geom_circle(data = dorl2, aes(x0 = X, y0 =Y, r = rad), 
                       fill = alpha(cfgweak, .5), color = cfg) +
  geom_polygon(
    half_inside,
    mapping=aes(x,y,group=shapeGroup),
    fill=cin,color=NA
  ) +
  geom_polygon(
    half_outside,
    mapping=aes(x,y,group=shapeGroup),
    fill=cout,color=NA
  ) +
  geom_richtext(data = lab,
             aes(x = X, y = Y, label = label),
             color = "white", fill = alpha(cbg, .8), lineheight = 1.1,
             size = 16 / .pt, label.colour = NA,
             nudge_x = c(9e5, -1e6, -1e4),
             nudge_y = c(4e5, -3e5, 0)) +
  labs(
    title = "Displacement Divided: Perspectives Across Africa",
    subtitle = subtitle,
    caption = "Data: UNHCR · Map: Cédric Vidonne · #30DayMapChallenge"
  ) +
  coord_sf(ylim = c(-5205231.2 + 1.5e6, 4156697.4)) +
  theme_void(base_size = 16, base_family = fmain) +
  theme(
    plot.background = element_rect(
      fill = cbg, color = NA
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = fmain, face = "bold", color = cfg, size = 28, hjust = 0,
    ),
    plot.subtitle = element_textbox_simple(
      family = fmain, color = cfgweak, size = 18,
      margin = margin(t = 5, b = 0), hjust = 0
    ),
    plot.caption = element_text(
      size = 14, hjust = 0.5, family = fmain,
      color = cfgweak,
    ),
    plot.margin = margin(rep(15, 4), unit = "pt"),
  )
# stop record
camcorder::gg_stop_recording()

# save
ggsave(here::here("24_onlycircular", "24_onlycircular.png"),
       device = agg_png,
       width = width, height = height, units = "in", dpi = 320
)
