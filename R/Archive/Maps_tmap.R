library(plotly)
library(leaflet)
library(tmap)
library(tmaptools)
library(sf)
library(dplyr)
library(raster)
library(viridis)
library(gisr)
library(baear)
library(mapview)
library(webshot)
cols <- viridis(100)
plot_dir <- file.path("C:/Users/blake/Documents/PhD Program",
  "McGarigal Lab Presentations/Lab Presentation - 2018.09/Figures")

# Landscape file locations
elev_file <- "C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif"
developed_file <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
forest_file <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_file <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"
shrub_file <- "C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif"

# Import rasters
developed_full <- raster(developed_file)
elev_full <- raster(elev_file)
forest_full <- raster(forest_file)
open_water_full <- raster(open_water_file)
shrub_full <- raster(shrub_file)

# Crop rasters
buff <- -6000
ext <- extent(357500 + buff, 377660 - buff, 4936600 + buff, 4956700 - buff)
developed <- crop(developed_full, ext)
elev <- crop(elev_full, ext)
forest <- crop(forest_full, ext)
open_water <- crop(open_water_full, ext)
shrub <- crop(shrub_full, ext)
rm(buff, ext, developed_full, elev_full, forest_full, open_water_full,
  shrub_full)

sf_ext <- st_as_sfc(st_bbox(open_water))

maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = 4326) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

# PLOT mode maps ---------------------------------------------------------------

tmap_mode("plot")

# Maine Overview Map

# osm types
# c("osm",  "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor",
#   "esri", "esri-topo",  "nps", "osm-transport", "osm-public-transport")
osm_maine <- read_osm(maine, ext=1.15, type = "esri")

tmap_style("natural")

maine_outline <- tm_shape(osm_maine) +
  tm_rgb() +
  tm_shape(maine) + # setting this as master sets lat/long
  tm_borders(col = "black") +
  tm_shape(sf_ext) +
  tm_borders(col = "red")

maine_open_water <- maine_outline +
  tm_shape(open_water) +
  tm_raster("open_water_30mc", title = "Open Water", style = "cat",
    palette = viridis(2), alpha = .75)

maine_outline_final <- maine_outline +
  tm_layout(main.title = " Maine Overview", main.title.position = "center",
    main.title.size = 1.15, # title = "Maine", #title.position = c(.65, .02),
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = FALSE, position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = .75, position = c(.25, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.85, .9))+
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
  tm_xlab("") +
  tm_ylab("")

tmap_save(tm = maine_outline_final, filename = file.path(plot_dir,
  "Maine_TEST.png"), unit = "in", dpi = 300, height = 7.5, width = 10)

tmap_options(bg.color = "white")
# Multiple Maps
osm_ext <- read_osm(sf_ext, height = 1.5, width = 2, relative = TRUE,
  type = "esri")

tm_shape(osm_ext) +
  tm_rgb()

tmap_options(legend.title.size = 1, legend.text.size = .85,
  legend.outside = FALSE)

ma_a <- tm_shape(osm_ext) + tm_rgb(saturation = .5) +
  tm_shape(open_water) +
  tm_layout(title = "Open Water", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water", style = "cat",
    palette = viridis(2, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8)) +
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center",
    "bottom"))

ma_b <- tm_shape(osm_ext) + tm_rgb(saturation = 0) +
  tm_shape(developed) +
  tm_layout(title = "Developed", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma_c <- tm_shape(osm_ext) + tm_rgb(saturation = 0) +
  tm_shape(forest) +
  tm_layout(title = "Forest", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("forest_30mc", title = "Forest",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma_d <- tm_shape(osm_ext) + tm_rgb(saturation = 0) +
  tm_shape(elev) +
  tm_layout(title = "Elevation", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("elev_30mc", title = "Elevation",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma_e <- tm_shape(osm_ext) + tm_rgb(saturation = 0) +
  tm_shape(shrub) +
  tm_layout(title = "Shrub and Herb", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("shrub_herb_30mc", title = "Elevation",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma_a_d <- tmap_arrange(ma_a, ma_b, ma_c, ma_d, asp = 1.33, outer.margins = 0.01)
tmap_save(tm = ma_a_d, filename = file.path(plot_dir, "Tmap_4Layers.png"),
  unit = "in", dpi = 300, height = 7.5, width = 10)

ma0 <- tm_shape(osm_open_water) + tm_rgb(saturation = .5) +
  tm_shape(open_water) +
  tm_layout(title = "Open Water (original)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water", style = "cat",
    palette = viridis(2, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8)) +
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma1 <- tm_shape(osm_open_water) + tm_rgb(saturation = 0) +
  tm_shape(covar_ras_smoothie1) +
  tm_layout(title = "Open Water (sigma = 1)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma2 <- tm_shape(osm_open_water) + tm_rgb(saturation = 0) +
  tm_shape(covar_ras_smoothie2) +
  tm_layout(title = "Open Water (sigma = 2)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma3 <- tm_shape(osm_open_water) + tm_rgb(saturation = 0) +
  tm_shape(covar_ras_smoothie3) +
  tm_layout(title = "Open Water (sigma = 3)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma0_3 <- tmap_arrange(ma0, ma1, ma2, ma3, asp = 1.33, outer.margins = 0.01)
tmap_save(tm = ma0_3, filename = file.path(plot_dir,
  "Tmap_OpenWater_0-3.png"), unit = "in", dpi = 300, height = 7.5, width = 10)

ma10 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie10) +
  tm_layout(title = "Open Water (sigma = 10)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma20 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie20) +
  tm_layout(title = "Open Water (sigma = 20)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma30 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie30) +
  tm_layout(title = "Open Water (sigma = 30)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma40 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie30) +
  tm_layout(title = "Open Water (sigma = 40)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("open_water_30mc", title = "Open Water",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma10_40 <- tmap_arrange(ma10, ma20, ma30, ma40, sync = TRUE, asp = 1.33,
  outer.margins = 0.01)
ttm()
ma10_40
tmap_save(tm = ma10_40, filename = file.path(plot_dir,
  "Tmap_OpenWater_10-40.png"), unit = "in", dpi = 300, height = 7.5, width = 10)

tmap_mode("plot")

tmap_options(legend.title.size = 1, legend.text.size = .85,
  legend.outside = FALSE)

ma0 <- tm_shape(osm_open_water) + tm_rgb(saturation = .5) +
  tm_shape(developed) +
  tm_layout(title = "Developed (original)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed", style = "cat",
    palette = viridis(2, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8)) +
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma1 <- tm_shape(osm_open_water) + tm_rgb(saturation = 0) +
  tm_shape(covar_ras_smoothie1) +
  tm_layout(title = "Developed (sigma = 1)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma2 <- tm_shape(osm_open_water) + tm_rgb(saturation = 0) +
  tm_shape(covar_ras_smoothie2) +
  tm_layout(title = "Developed (sigma = 2)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma3 <- tm_shape(osm_open_water) + tm_rgb(saturation = 0) +
  tm_shape(covar_ras_smoothie3) +
  tm_layout(title = "Developed (sigma = 3)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma0_3 <- tmap_arrange(ma0, ma1, ma2, ma3, asp = 1.33, outer.margins = 0.01)
tmap_save(tm = ma0_3, filename = file.path(plot_dir,
  "Tmap_Developed_0-3.png"), unit = "in", dpi = 300, height = 7.5, width = 10)

ma10 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie10) +
  tm_layout(title = "Developed (sigma = 10)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma20 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie20) +
  tm_layout(title = "Developed (sigma = 20)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma30 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie30) +
  tm_layout(title = "Developed (sigma = 30)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma40 <- tm_shape(osm_open_water) + tm_rgb() +
  tm_shape(covar_ras_smoothie30) +
  tm_layout(title = "Developed (sigma = 40)", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("developed_30mc", title = "Developed",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center", "bottom"))

ma10_40 <- tmap_arrange(ma10, ma20, ma30, ma40, sync = TRUE, asp = 1.33,
  outer.margins = 0.01)
ma10_40
tmap_save(tm = ma10_40, filename = file.path(plot_dir,
  "Tmap_Developed_10-40.png"), unit = "in", dpi = 300, height = 7.5, width = 10)


# VIEW map maps ----------------------------------------------------------------

tmap_mode("view")
opts <- tmap_options(
  basemaps = c(Canvas = "Esri.WorldGrayCanvas", Imagery = "Esri.WorldImagery"),
  overlays = c(Labels = paste0(
    "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
    "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")))

# Single Map
tm_layout(title = "Maine") +
tm_shape(maine) +
  tm_polygons("state_name", palette = "blue", title = "State",) +
tm_shape(metro, bbox = "Maine") +
  tm_dots(col = "red", group = "Metropolitan areas") +
  tm_scale_bar(position=c("left", "bottom"))

# Multiple Maps
ma1 <- tm_shape(open_water) +
  tm_raster("open_water_30mc", title = "Open Water", style = "cat")
ma2 <- tm_shape(developed) +
  tm_raster("developed_30mc", title = "Developed", style = "cat") +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)

tmap_arrange(ma1, ma2)

tm_shape(maine) +
  tm_polygons("state", fill = "blue")

developed_map <- tm_shape(developed) +
  tm_raster("developed_30mc", n = 2, palette = viridis(100, option = "A"))
developed_map

maine_developed_map <- developed_map +
  tm_shape(maine) +
  tm_borders(col = "red")
maine_developed_map

leaflet::providers

# Interactive Viewing
tmap_mode("view")
tm_basemap(leaflet::providers$Esri.NatGeoWorldMap) +
  tm_shape(maine) +
  tm_borders(col = "red") +
  tm_shape(p_sf) +
  tm_borders(col = "blue") +
  tm_shape(open_water) +
  tm_raster("open_water_30mc")

# Static Mapping
tmap_mode("plot")

osm_maine <- read_osm(maine, ext=1.1, type = "nps")
tm_shape(osm_maine) +
  tm_rgb() +
  tm_shape(maine, is.master = TRUE) +
  tm_borders(col = "red") +
  tm_shape(open_water) +
  tm_raster("open_water_30mc", palette = cols, n = 2) +
  tm_shape(p_sf) +
  tm_borders(col = "white") +
  tm_style(style = "cobalt")

osm_open_water <- read_osm(sf_ext, ext = 2, type = "esri")

tm_shape(osm_open_water) +
  tm_rgb() +
  tm_shape(sf_ext) +
  tm_borders(col = "red")
SavePlot("Maine_Location", plot_dir)

tm_shape(osm_open_water) +
  tm_rgb() +
  tm_shape(open_water) +
  tm_raster("open_water_30mc", palette = cols, n = 2, alpha = .75) +
  tm_shape(sf_ext) +
  tm_borders(col = "white")

SavePlot("Maine_Location_Open_Water", plot_dir)
