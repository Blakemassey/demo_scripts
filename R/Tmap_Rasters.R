pacman::p_load(plotly, leaflet, tmap, tmaptools, sf, dplyr, raster, viridis,
  mapview,webshot)
library(baear)
library(gisr)
cols <- viridis(100)
plot_dir <- file.path("C:/Work/Figures")

# Landscape file locations
elev_file <- "C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif"
developed_file <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
forest_file <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_file <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"

# Import rasters
developed_full <- raster(developed_file)
elev_full <- raster(elev_file)
forest_full <- raster(forest_file)
open_water_full <- raster(open_water_file)

# Crop rasters
buff <- -6000
ext <- extent(357500 + buff, 377660 - buff, 4936600 + buff, 4956700 - buff)
developed <- crop(developed_full, ext)
elev <- crop(elev_full, ext)
forest <- crop(forest_full, ext)
open_water <- crop(open_water_full, ext)
rm(buff, ext, developed_full, elev_full, forest_full, open_water_full)

sf_ext <- st_as_sfc(st_bbox(open_water))


maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = 4326) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

# PLOT mode maps ---------------------------------------------------------------

tmap_mode("plot")
tmap_style("natural")

# Maine Overview Map

# Use "Tmap_baselayers.R" script to get other baselayers
osm_maine <- read_osm(maine, ext=1.15, type = "stamen-terrain")

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

maine_outline_final

tmap_save(tm = maine_outline_final, filename = file.path(plot_dir,
  "Maine_TEST.png"), unit = "in", dpi = 300, height = 7.5, width = 10)

tmap_options(bg.color = "white")
# Multiple Maps
osm_ext <- read_osm(sf_ext, height = 1.5, width = 2, relative = TRUE,
  type = "esri-topo")

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
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center",
    "bottom"))

ma_c <- tm_shape(osm_ext) + tm_rgb(saturation = 0) +
  tm_shape(forest) +
  tm_layout(title = "Forest", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("forest_30mc", title = "Forest",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center",
    "bottom"))

ma_d <- tm_shape(osm_ext) + tm_rgb(saturation = 0) +
  tm_shape(elev) +
  tm_layout(title = "Elevation", title.size = 1.2,
    title.position = c("center", "top")) +
  tm_raster("elev_30mc", title = "Elevation",  style = "cont",
    palette = viridis(100, option = "D"), alpha = .75) +
  tm_compass(type = "4star",  show.labels = 1, size = 3, position = c(.82, .8))+
  tm_scale_bar(breaks = c(0, 5, 10, 15), size = .65, position = c("center",
    "bottom"))

ma_a_d <- tmap_arrange(ma_a, ma_b, ma_c, ma_d, asp = 1.33, outer.margins = 0.01)
ma_a_d

tmap_save(tm = ma_a_d, filename = file.path(plot_dir, "Tmap_4Layers.png"),
  unit = "in", dpi = 300, height = 7.5, width = 10)

# VIEW map maps ----------------------------------------------------------------

tmap_mode("view")
opts <- tmap_options(
  basemaps = c(Canvas = "Esri.WorldGrayCanvas", Imagery = "Esri.WorldImagery"),
  overlays = c(Labels = paste0(
    "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
    "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")))

# Single Map
tm_basemap(leaflet::providers$Esri.NatGeoWorldMap) +
tm_layout(title = "Maine") +
tm_shape(developed) +
  tm_raster("developed_30mc", n = 2, palette = viridis(100, option = "A"))
tm_shape(maine) +
  tm_borders(col = "red")

# Multiple Maps
ma1 <- tm_shape(open_water) +
  tm_raster("open_water_30mc", title = "Open Water", style = "cat")
ma2 <- tm_shape(developed) +
  tm_raster("developed_30mc", title = "Developed", style = "cat") +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)
tmap_arrange(ma1, ma2, sync = TRUE)
