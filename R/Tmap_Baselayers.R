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
register_tile_source(tf_outdoors = paste0(tf_url, "outdoors", tf_tile, tf_key))
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

########### CREATE EXTENT FROM COORDS, POLYGON, OR RASTER ######################

# Use coordinates to make extent bb
in_coords <- extent(-71.5, -66.5, 43, 47.5)
in_coords <- extent(-72, -67, 43, 48)
sfc_coords <- st_as_sfc(st_bbox(in_coords)) %>% st_set_crs(4326)
ext_coords <- st_as_sfc(bb(sfc_coords, relative = TRUE, height = 1, width = 2))
mapview(ext_coords)

# Use raster to make extent
elev_full <- raster("C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif")
buff <- -6000
ext_ras <- extent(357500 + buff, 377660 - buff, 4936600 + buff, 4956700 - buff)
in_raster <- crop(elev_full, ext_ras)
sfc_raster <- st_as_sfc(st_bbox(in_raster)) %>% st_transform(., crs = 4326)
ext_raster <- st_as_sfc(bb(sfc_raster, relative = TRUE, height = 1.15,
  width = 1.15))
mapview(sfc_raster)

# Use polygon to make extent
in_polygon <- read_sf("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp") %>%
  st_transform(., crs = 4326) %>% mutate(state = "Maine")  %>%
  dplyr::select(state)
ext_polygon <- st_as_sfc(bb(in_polygon, relative = TRUE, height = 1.15,
  width = 1.15))

#ext_polygon <- gisr::CreateExtentSF(sfc_polygon, 1.15, 1)
mapview(ext_polygon)

# Set which 'sf' object to create extent bb
i <- 1
ext_sf <- c(ext_coords, ext_polygon, ext_raster)[i]

# Make Baselayer Bounding Boxes

om_bb <- CreateOSMBaseBB(ext_sf, type = "om_type")
rosm_bb <- CreateOSMBaseBB(ext_sf, type = "rosm_type")

############################# DOWNLOAD TILES ###################################

# Download basemap tiles and convert to RasterLayer

# 'OpenStreeMap' Package -------------------------------------------------------

om_down = OpenStreetMap::openmap(om_bb[[1]], om_bb[[2]], minNumTiles = 9,
  type = om_type)
om_r <- RasterizeOsMDownload(om_down)
tm_shape(om_r) +
  tm_rgb() + tm_layout(asp = 1)

# 'rosm' Package ---------------------------------------------------------------
# TRICK TO BETTER RESOLUTION!?!  Maximize 'Plots' window

rosm::osm.plot(rosm_bb, type = rosm_type, stoponlargerequest=FALSE)
  # use osm.plot() to find appropriate zoom level
rosm_down <- rosm::osm.raster(rosm_bb, crop = TRUE, res = 300, zoom = 7,
  type = rosm_type)
rosm_r <- RasterizeROSMDownload(rosm_down)
tm_shape(rosm_r) +
  tm_rgb()


## Make map with specifc aspect ratio ------------------------------------------

# Putting together a map with a specific ratio (2 width/1 height) with
# background that fills in whole area. Requires sf_ext be large enough to
# cover the whole background.

in_coords <- extent(-71.5, -66.5, 43, 47.5)
sfc_coords <- st_as_sfc(st_bbox(in_coords)) %>% st_set_crs(4326)
ext_coords <- st_as_sfc(bb(sfc_coords, relative = TRUE, height = 2, width = 1))
om_bb <- MakeOSMBaseBB(ext_coords, type = "om_type")

om_down = OpenStreetMap::openmap(om_bb[[1]], om_bb[[2]], minNumTiles = 9,
  type = om_type)
om_r <- RasterizeOsMDownload(om_down)

tm_layout(asp = .5) +
tm_shape(om_r) +
  tm_rgb() +
tm_shape(in_polygon,
  bbox = bb(in_polygon, ext = 1.15), is.master = TRUE) +
  tm_borders(col = "red")
