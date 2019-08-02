# Working script to download basemaps (including Thunder Forest!) data and
# use it in tmap()!
# ---------------------------------------------------------------------------- #
suppressPackageStartupMessages(library(rosm))
suppressPackageStartupMessages(library(cartography))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(prettymapr))
suppressPackageStartupMessages(library(tmap))
suppressPackageStartupMessages(library(tmaptools))
suppressPackageStartupMessages(library(OpenStreetMap))
suppressPackageStartupMessages(library(prettymapr))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(mapview))

############################ BASEMAP SOURCES  ##################################

# CARTO ------------------------------------------------------------------------
# Must use osm.raster() with 'rosm_type' argument
carto_url <- "https://cartodb-basemaps-a.global.ssl.fastly.net/"
carto_tile <- "/${z}/${x}/${y}${r}.png"
rosm_type <- paste0(carto_url, "dark_all", carto_tile)
rosm_type <- paste0(carto_url, "dark_nolabels", carto_tile)
rosm_type <- paste0(carto_url, "dark_only_labels", carto_tile)
rosm_type <- paste0(carto_url, "light_all", carto_tile)
rosm_type <- paste0(carto_url, "light_nolabels", carto_tile)
rosm_type <- paste0(carto_url, "light_only_labels", carto_tile)
rosm_type <- paste0(carto_url, "rastertiles/voyager", carto_tile)
rosm_type <- paste0(carto_url, "rastertiles/voyager_nolabels", carto_tile)
rosm_type <- paste0(carto_url, "rastertiles/voyager_only_labels", carto_tile)
rosm_type <- paste0(carto_url, "rastertiles/voyager_labels_under", carto_tile)

# ESRI -------------------------------------------------------------------------
# Must use openmap() with 'om_type' argument
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)
om_type <- paste0(esri_url, "Ocean_Basemap", esri_tile)
om_type <- paste0(esri_url, "Canvas/World_Light_Gray_Base", esri_tile)
om_type <- paste0(esri_url, "World_Imagery", esri_tile)
om_type <- paste0(esri_url, "World_Physical_Map", esri_tile)
om_type <- paste0(esri_url, "World_Shaded_Relief", esri_tile)
om_type <- paste0(esri_url, "World_Street_Map", esri_tile)

# MAPBOX -----------------------------------------------------------------------
# Must use openmap() with 'om_type' argument
mapbox_url <- "https://api.mapbox.com/styles/v1/mapbox/"
mapbox_tile <- "/tiles/256/{z}/{x}/{y}"
mapbox_key <- paste0("?access_token=pk.eyJ1IjoiYmxha2VtYXNzZXkiLCJhIjoi",
  "Y2pseTYxYW56MDE4eDNwcXZxdmNtNmJ1eiJ9.cguQx1N8bIpciBnc2h3v_w")
om_type <- paste0(mapbox_url, "streets-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "outdoors-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "light-v9", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "dark-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "satellite-v9", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "satellite-v10", mapbox_tile, mapbox_key)

# OPENSTREETMAP ----------------------------------------------------------------
# Must use osm.raster() with 'rosm_type' argument
osm_url <- "https://"
osm_tile <- "/${z}/${x}/${y}.png"
rosm_type <- paste0(osm_url, "a.tile.openstreetmap.org", osm_tile) # default
rosm_type <- paste0(osm_url, "tiles.wmflabs.org/bw-mapnik", osm_tile) # bw base
rosm_type <- paste0(osm_url, "a.tile.opentopomap.org", osm_tile) # topo

# STAMEN -----------------------------------------------------------------------
# Must use osm.raster() with 'osm_type' argument
stamen_url <- "http://c.tile.stamen.com/"
stamen_tile <- "/${z}/${x}/${y}"
rosm_type <- paste0(stamen_url, "terrain", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "terrain-background", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toner", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toner-background", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toner-lite", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toposm-color-relief", stamen_tile, ".jpg")
rosm_type <- paste0(stamen_url, "watercolor", stamen_tile, ".jpg")

# THUNDERFOREST ----------------------------------------------------------------
# Must use osm.raster() with 'rosm_type' argument
tf_url <- "https://a.tile.thunderforest.com/"
tf_tile <- "/${z}/${x}/${y}.png"
tf_key <- "?apikey=9b0b8a66a3e74b47be31f38597e7f9e7"
register_tile_source(tf_landscape = paste0(tf_url, "landscape", tf_tile,tf_key))
register_tile_source(tf_outdoor = paste0(tf_url, "outdoors", tf_tile, tf_key))
register_tile_source(tf_pioneer = paste0(tf_url, "pioneer", tf_tile, tf_key))
register_tile_source(tf_spinal = paste0(tf_url, "spinal-map", tf_tile, tf_key))
register_tile_source(tf_transport = paste0(tf_url, "transport", tf_tile,tf_key))
register_tile_source(tf_transport_dark = paste0(tf_url, "transport-dark",
  tf_tile, tf_key))
rosm_type = "tf_landscape"
rosm_type = "tf_outdoors"
rosm_type = "tf_pioneer"
rosm_type = "tf_spinal"
rosm_type = "tf_transport"
rosm_type = "tf_transport_dark"

############################# DOWNLOAD TILES ###################################

# Download basemap tiles and convert to RasterLayer

# Use coordinates to make extent bb
om_bb <- makebbox(47, -67, 43.5, -71)
om_ullr <- list(c(om_bb[2,2], om_bb[1,1]), c(om_bb[2,1], om_bb[1,2]))

# Use 'sf' object to create extent bb
sf_obj <- baea_i # set st_obj
sf_coords <- as.numeric(rev(st_bbox(sf_obj %>% st_transform(crs = 4326))))
om_bb <- makebbox(sf_coords[1], sf_coords[2], sf_coords[3], sf_coords[4])
om_ullr <- list(c(om_bb[2,2], om_bb[1,1]), c(om_bb[2,1], om_bb[1,2]))

# 'OpenStreeMap' Package -------------------------------------------------------

om = openmap(om_ullr[[1]], om_ullr[[2]], minNumTiles = 9, type = om_type)
cols <- as.factor(om[1][[1]][[1]]$colorData) # cols = vec of hexdecimal colors
oms <- raster::raster(om) # convert OpenStreetMap to RasterStack
omr <- raster::raster(oms) # Conver RasterStack to RasterLayer
omr <- raster::setValues(omr, as.integer(cols) - 1L) # Raster w/values 1-255
raster::colortable(omr) <- levels(cols)
attr(omr, "is.OSM") <- TRUE
tm_shape(omr) +
  tm_raster()

# 'rosm' Package ---------------------------------------------------------------
# TRICK TO BETTER RESOLUTION!?!  Maximize 'Plots' window

osm.plot(om_bb, type = rosm_type, stoponlargerequest=FALSE)
  # use osm.plot() to find appropriate zoom level
om2 <- rosm::osm.raster(om_bb, crop = T, res = 300, zoom = 8, type = rosm_type)
oms2 <- trim(stack(om2)) # Convert to Stack, then trim off NA cells
oms2[is.na(oms2)] <- 0
tab2 <- as.factor(rgb(oms2[], maxColorValue = 255))
omr2 <- raster::raster(oms2)
omr2 <- raster::setValues(omr2, as.integer(tab2) - 1L)
raster::colortable(omr2) <- levels(as.factor(tab2))
tm_shape(omr2) +
  tm_raster()




CreateOSMBaseBB <- function(sf_obj,
                          type = c("om_type", "rosm_type")){
  sf_coords <- as.numeric(rev(sf::st_bbox(sf_obj %>%
    sf::st_transform(crs = 4326))))
  rosm_bb <- prettymapr::makebbox(sf_coords[1], sf_coords[2], sf_coords[3],
    sf_coords[4])
  om_bb <- list(c(rosm_bb[2,2], rosm_bb[1,1]), c(rosm_bb[2,1], rosm_bb[1,2]))
  if(type == "rosm_type") return(rosm_bb)
  if(type == "om_type") return(om_bb)
}

RasterizeOsMDownload <- function(osm_download){
  osm_d <- osm_download
  cols <- as.factor(osm_d[1][[1]][[1]]$colorData) # vec of hexdecimal colors
  osm_s <- raster::raster(osm_d) # convert OpenStreetMap to RasterStack
  osm_r <- raster::raster(osm_s) # Conver RasterStack to RasterLayer
  osm_r <- raster::setValues(osm_r, as.integer(cols) - 1L) # Ras w/values 1-255
  raster::colortable(osm_r) <- levels(cols)
  attr(osm_r, "is.OSM") <- TRUE
  return(osm_r) # RasterLayer with colortable
}

RasterizeROSMDownload <- function(rosm_download){
  rosm_d <- rosm_download
  rosm_s <- raster::trim(raster::stack(rosm_d)) # Make Stack, then trim NA cells
  rosm_s[is.na(rosm_s)] <- 0
  cols <- as.factor(grDevices::rgb(rosm_s[], maxColorValue = 255))
  rosm_r <- raster::raster(rosm_s)
  rosm_r <- raster::setValues(rosm_r, as.integer(cols) - 1L)
  raster::colortable(rosm_r) <- levels(as.factor(cols))
  return(rosm_r)
}


