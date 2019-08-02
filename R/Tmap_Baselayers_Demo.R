# Working script to download basemaps (including Thunder Forest) data and
# use it in tmap()

################################# PACKAGES #####################################

library(dplyr)
library(FedData)
library(leaflet)
library(mapview)
library(OpenStreetMap)
library(prettymapr)
library(raster)
library(rosm)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)

################################ FUNCTIONS #####################################

#' Create bounding box needed for downloading an 'OpenStreetMap' or 'rosm'
#'     baselayer
#' @param sf_obj a 'sf' object, for determing bounding box coordinates
#' @param type character, either "om_type" or "rosm_type", default = "om_type"
#' @return a vector (for type = "rosm_type") or list (for type = "om_type")
#' @export
CreateOSMBaseBB <- function(sf_obj,
                            type = c("om_type")){
  sf_coords <- as.numeric(rev(sf::st_bbox(sf_obj %>%
    sf::st_transform(crs = 4326))))
  rosm_bb <- prettymapr::makebbox(sf_coords[1], sf_coords[2], sf_coords[3],
    sf_coords[4])
  om_bb <- list(c(rosm_bb[2,2], rosm_bb[1,1]), c(rosm_bb[2,1], rosm_bb[1,2]))
  if(type == "rosm_type") return(rosm_bb)
  if(type == "om_type") return(om_bb)
}

#' Creates an 'sf' polygon based an 'sf' object's extent and aspect ratio
#' @usage CreateMapExtentBB(sf_object, ext, asp)
#' @param sf_object 'sf' object
#' @param ext extension factor of the extent, default = 1.15.
#' @param asp ratio of the extent (width/height), default = 1
#' @return 'sf' object
#' @details Useful for making a main-map extent box within a overview map. Not
#'     guaranteed to be identical to a tmap's actual main map extent.
#' @export
CreateMapExtentBB <- function(sf_object,
                              ext = 1.15,
                              asp = 1){
  map_asp <- asp
  sf_asp <- tmaptools::get_asp_ratio(tmaptools::bb(sf_object, ext = ext))
  if (sf_asp >= map_asp){
    bb_width = ext     # width should not be reduced
    bb_height = (sf_asp/map_asp)*ext
  } else {
    bb_width = (map_asp/sf_asp)*ext
    bb_height = ext # height should not be reducted
  }
  map_bb <- sf::st_as_sfc(tmaptools::bb(sf_object, relative = TRUE,
    width = bb_width, height = bb_height))
  sf::st_crs(map_bb) <- sf::st_crs(sf_object)
  return(map_bb)
}

#' Convert basemaps download from 'OpenStreetMaps' to Raster for use in tm_map
#' @param osm_download download from OpenStreetMaps::openmap()
#' @return a RasterLayer
#' @export
RasterizeOMDownload <- function(om_download){
  osm_d <- om_download
  cols <- as.factor(osm_d[1][[1]][[1]]$colorData) # vec of hexdecimal colors
  osm_s <- raster::raster(osm_d) # convert OpenStreetMap to RasterStack
  osm_r <- raster::raster(osm_s) # Conver RasterStack to RasterLayer
  osm_r <- raster::setValues(osm_r, as.integer(cols) - 1L) # Ras w/values 1-255
  raster::colortable(osm_r) <- levels(cols)
  attr(osm_r, "is.OSM") <- TRUE
  return(osm_r) # RasterLayer with colortable
}

#' Convert basemaps download from 'rosm' to Raster for use in tm_map
#' @param rosm_download download from rosm::osm.raster()
#' @return a RasterLayer
#' @export
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

############################ BASEMAP SOURCES  ##################################

# CARTO ------------------------------------------------------------------------
# Must be used with osm.raster() and 'rosm_type' argument
carto_url <- "https://cartodb-basemaps-a.global.ssl.fastly.net/"
carto_tile <- "/${z}/${x}/${y}${r}.png"
# Select one
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
# Must be used with openmap() and 'om_type' argument
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
# Select one
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)
om_type <- paste0(esri_url, "Ocean_Basemap", esri_tile)
om_type <- paste0(esri_url, "Canvas/World_Light_Gray_Base", esri_tile)
om_type <- paste0(esri_url, "World_Imagery", esri_tile)
om_type <- paste0(esri_url, "World_Physical_Map", esri_tile)
om_type <- paste0(esri_url, "World_Shaded_Relief", esri_tile)
om_type <- paste0(esri_url, "World_Street_Map", esri_tile)

# MAPBOX -----------------------------------------------------------------------
# Must be used with openmap() and 'om_type' argument
mapbox_url <- "https://api.mapbox.com/styles/v1/mapbox/"
mapbox_tile <- "/tiles/256/{z}/{x}/{y}"
mapbox_key <- paste0("?access_token=pk.eyJ1IjoiYmxha2VtYXNzZXkiLCJhIjoi",
  "Y2pseTYxYW56MDE4eDNwcXZxdmNtNmJ1eiJ9.cguQx1N8bIpciBnc2h3v_w")
# Select one
om_type <- paste0(mapbox_url, "outdoors-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "light-v9", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "dark-v9", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "satellite-v9", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "satellite-streets-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "streets-v10", mapbox_tile, mapbox_key)

# OPENSTREETMAP ----------------------------------------------------------------
# Must be used with osm.raster() and 'rosm_type' argument
osm_url <- "https://"
osm_tile <- "/${z}/${x}/${y}.png"
# Select one
rosm_type <- paste0(osm_url, "a.tile.openstreetmap.org", osm_tile) # default
rosm_type <- paste0(osm_url, "tiles.wmflabs.org/bw-mapnik", osm_tile) # bw base
rosm_type <- paste0(osm_url, "a.tile.opentopomap.org", osm_tile) # topo

# STAMEN -----------------------------------------------------------------------
# Must be used with osm.raster() and 'osm_type' argument
stamen_url <- "http://c.tile.stamen.com/"
stamen_tile <- "/${z}/${x}/${y}"
# Select one
rosm_type <- paste0(stamen_url, "terrain", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "terrain-background", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toner", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toner-background", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toner-lite", stamen_tile, ".png")
rosm_type <- paste0(stamen_url, "toposm-color-relief", stamen_tile, ".jpg")
rosm_type <- paste0(stamen_url, "watercolor", stamen_tile, ".jpg")

# THUNDERFOREST ----------------------------------------------------------------
# Must be used with osm.raster() and 'rosm_type' argument
tf_url <- "https://a.tile.thunderforest.com/"
tf_tile <- "/${z}/${x}/${y}.png"
tf_key <- "?apikey=9b0b8a66a3e74b47be31f38597e7f9e7"
register_tile_source(tf_landscape = paste0(tf_url, "landscape", tf_tile,tf_key))
register_tile_source(tf_outdoors = paste0(tf_url, "outdoors", tf_tile, tf_key))
register_tile_source(tf_pioneer = paste0(tf_url, "pioneer", tf_tile, tf_key))
register_tile_source(tf_spinal = paste0(tf_url, "spinal-map", tf_tile, tf_key))
register_tile_source(tf_transport = paste0(tf_url, "transport", tf_tile,tf_key))
register_tile_source(tf_transport_dark = paste0(tf_url, "transport-dark",
  tf_tile, tf_key))
# Select one
rosm_type = "tf_landscape"
rosm_type = "tf_outdoors"
rosm_type = "tf_pioneer"
rosm_type = "tf_spinal"
rosm_type = "tf_transport"
rosm_type = "tf_transport_dark"

# IMPORTANT:
# A user has to decide on which of the 'om_type' or 'rosm_type' baselayers to
# use from all of the options above by selecting one to have as the current
# object of the type.
# For example, if you wanted to use ESRI 'National Geographic' run the line:
  om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)
# Or, if you want to use Thunderforest's "Pioneer' run the line:
  rosm_type = "tf_pioneer"
# Once the baselayer type is selected, use the appropriate code below for that
# type.

########## CREATE BASEMAP EXTENT FROM COORDS, POLYGON, OR RASTER ###############

# Use coordinates to create baselayer extent
in_coords <- extent(-71.2, -66.5, 42.9, 47.6)
sfc_coords <- st_as_sfc(st_bbox(in_coords)) %>% st_set_crs(4326)
ext_coords <- st_as_sfc(bb(sfc_coords, relative = TRUE, height = 1.15,
  width = 1.15))
mapview(sfc_coords) + mapview(st_bbox(ext_coords), color = "yellow")

# Use polygon to create baselayer extent
all_states <- states(cb = TRUE) %>% st_as_sf(.)
in_polygon <- all_states[which(all_states$NAME == "Maine"), ]
ext_polygon <- st_as_sfc(tmaptools::bb(in_polygon, relative = TRUE,
  height = 1.15, width = 1.15))
mapview(in_polygon) + mapview(st_bbox(ext_polygon), color = "yellow")

# Use raster to create baselayer extent
ned_ext <- st_as_sfc(st_bbox(raster::extent(-68.31, -68.18, 44.28, 44.415))) %>%
  st_set_crs(4326) # Acadia NP (extent used for downloading NED data)
in_raster <- get_ned(template = as(ned_ext, "Spatial"), label='Acadia',
  force.redo = TRUE) # downloads raster tiles into dir './RAW/NED/'
sfc_raster <- st_as_sfc(st_bbox(in_raster)) %>% st_transform(crs = 4326)
ext_raster <- st_as_sfc(bb(sfc_raster, relative = TRUE, height = 1.15,
  width = 1.15))
mapview(in_raster) + mapview(st_bbox(ext_raster), color = "yellow")

# Selection which 'sf' object to create extent bb
i <- 1
ext_sf <- c(ext_coords, ext_polygon, ext_raster)[i]

# Make Baselayer Bounding Boxes

om_bb <- CreateOSMBaseBB(ext_sf, type = "om_type") # need to specify type
rosm_bb <- CreateOSMBaseBB(ext_sf, type = "rosm_type") # need to specify type

############################# DOWNLOAD TILES ###################################

# Download basemap tiles and convert to RasterLayer

# 'OpenStreeMap' Package -------------------------------------------------------

om_down = OpenStreetMap::openmap(om_bb[[1]], om_bb[[2]], minNumTiles = 9,
  type = om_type) # 'om_type' is selected ABOVE
om_r <- RasterizeOMDownload(om_down)
tm_shape(om_r) +
  tm_rgb() + tm_layout()

# 'rosm' Package ---------------------------------------------------------------
# Trick for better resolution: Maximize 'Plots' window

rosm::osm.plot(rosm_bb, type = rosm_type, stoponlargerequest=FALSE)
# use osm.plot() to find appropriate zoom level
rosm_down <- rosm::osm.raster(rosm_bb, crop = TRUE, res = 300, zoom = 7,
  type = rosm_type)  # 'rosm_type' is selected ABOVE
rosm_r <- RasterizeROSMDownload(rosm_down)
tm_shape(rosm_r) +
  tm_rgb()

##################### CREATE MAP WITH SPECIFIC RATIO ###########################

# Putting together a map with a specific ratio (2 height/1 width) with
# background that fills in whole area.

all_states <- tigris::states(cb = TRUE) %>% st_as_sf(.)
in_polygon <- all_states[which(all_states$NAME == "Maine"), ]
ext_polygon <- CreateMapExtentBB(in_polygon, ext = 1.15, asp = .5)
# Arguments for CreateMapExtentBB():
# ext = numeric,  extension factor of the extent, default = 1.15.
# asp = numeric,  ratio of the extent (width/height), default = 1
mapview(ext_polygon) + mapview(in_polygon)

om_bb <- CreateOSMBaseBB(ext_polygon, type = "om_type")
rosm_bb <- CreateOSMBaseBB(ext_polygon, type = "rosm_type")

# Baselayer of 'om_type'
om_down = OpenStreetMap::openmap(om_bb[[1]], om_bb[[2]], minNumTiles = 9,
  type = om_type)
om_r <- RasterizeOMDownload(om_down)

om_map <- tm_layout(asp = .5) + # 'asp' should match CreateMapExtentBB()
  tm_shape(om_r) +
    tm_rgb() +
  tm_shape(in_polygon, is.master = TRUE,
    bbox = bb(in_polygon, ext = 1.15)) + # 'ext' should match CreateMapExtentBB
    tm_borders(col = "red")
om_map

# Baselayer of 'rosm_type'
rosm::osm.plot(rosm_bb, type = rosm_type, stoponlargerequest=FALSE)
# use osm.plot() to find appropriate zoom level
rosm_down <- rosm::osm.raster(rosm_bb, crop = TRUE, res = 300, zoom = 7,
  type = rosm_type)
rosm_r <- RasterizeROSMDownload(rosm_down)

rosm_map <- tm_layout(asp = .5) + # 'asp' should match CreateMapExtentBB()
  tm_shape(rosm_r) +
    tm_rgb() +
  tm_shape(in_polygon, is.master = TRUE,
    bbox = bb(in_polygon, ext = 1.15)) + # 'ext' should match CreateMapExtentBB
  tm_borders(col = "red")
rosm_map

# Arrange maps side-by-side
arrange_2_maps <- tmap_arrange(om_map, rosm_map, nrow = 1, ncol = 2)
arrange_2_maps

########################## SAVING MAPS #########################################

# Saving demo basemaps
tmap_save(tm = om_map, filename = "om_map.png", unit = "in", dpi = 300,
  height = 5, width = 5)

tmap_save(tm = rosm_map, filename = "rosm_map.png", unit = "in", dpi = 300,
  height = 5, width = 5)

tmap_save(tm = arrange_2_maps, filename = "arrange_2_maps.png", unit = "in",
  dpi = 300, height = 5, width = 5)

## State Overview Map ------------------------------------------------------- ##

all_states <- tigris::states(cb = TRUE) %>% st_as_sf(.)
in_polygon <- subset(all_states, NAME == "Colorado") # try other states
ext_polygon <- CreateMapExtentBB(in_polygon, ext = 1.15, asp = 1)
om_bb <- CreateOSMBaseBB(ext_polygon, type = "om_type")

# Baselayer of 'om_type'
om_down = OpenStreetMap::openmap(om_bb[[1]], om_bb[[2]], minNumTiles = 9,
  type = om_type)
om_r <- RasterizeOMDownload(om_down)

state_map <- tm_layout(asp = 1) +
  tm_shape(om_r) +
    tm_rgb() +
  tm_scale_bar(size = .65, width = .2, position = c(.7, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 1.9,
    position = c(.85, .88)) +
  tm_shape(in_polygon, is.master = TRUE,
    bbox = bb(in_polygon, ext = 1.15)) +
    tm_borders(col = "blue", lwd = 2)
state_map

tmap_save(tm = state_map, filename = "state_map.png",
  unit = "in", dpi = 300, height = 5, width = 5)
