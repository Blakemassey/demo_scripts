suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(OpenStreetMap))
suppressPackageStartupMessages(library(tmap))
suppressPackageStartupMessages(library(tmaptools))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(webshot))
library(gisr)
library(baear)
cols <- viridis(100)
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

baea_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/BAEA")
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")

nests_study_org <- readRDS(file.path(nests_dir, "nests_study.rds"))
nests_study <- st_as_sf(x = nests_study_org, coords = c("long", "lat"),
  crs = "+proj=longlat +datum=WGS84")

baea_org <- readRDS(file.path(baea_dir, "baea.rds"))
baea <- st_as_sf(x = baea_org, coords = c("long_utm", "lat_utm"),
  crs = 32619) #  crs = "+proj=longlat +datum=WGS84")

maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = 4326) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

## ----------------------------- STATIC Maps -------------------------------- ##

tmap_mode("plot")

## Maine Overview Map ------------------------------------------------------- ##

omr <- read_osm(maine, ext=1.15, type = "esri")
# Use "Tmap_baselayers.R" script to get other baselayers

maine_overview <- tm_shape(omr) +
  tm_rgb() +
  tm_shape(maine) + # setting this as master sets lat/long
  tm_borders(col = "black")

maine_overview

## Nests Overview Map ------------------------------------------------------- ##

nests_overview <- maine_overview +
  tm_layout(
    main.title = " Trapping Sites Overview",
    main.title.position = "center",
    main.title.size = 1.15,
    title.position = c(.65, .02),
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85, outside = FALSE,
    position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = .65, position = c(.7, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 1.9,
    position = c(.85, .88))+
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
  tm_shape(nests_study) +
  tm_symbols("red", size = .5) +
	tm_text("name", auto.placement = TRUE, size = .5) +
  tm_xlab("") + tm_ylab("")
nests_overview

tmap_save(tm = nests_overview, filename =
  "C:/Work/R/Projects/baea_ibm/Products/Maps/Nests/Trapping_Sites_Overview.svg",
  unit = "in", dpi = 300, height = 5, width = 5)

## BAEA Data Maps ----------------------------------------------------------- ##

# Select id and year
table(baea$id, baea$year) # See available locations
id_i <- "Sandy"
year_i <- 2015

# Filter data, create fightpaths
baea_i <- baea %>% filter(id == id_i) %>% filter(year == year_i)
baea_i_lines <- baea_i %>% group_by(id) %>%
  arrange(datetime) %>%
  summarize(m = mean(year), do_union = FALSE) %>%
  st_cast("LINESTRING")

# Create 2d kernel density data - isopleth lines, polygons, and rasters
baea_i_smooth <- smooth_map(baea_i, cover = as(CreateExtentSF(baea_i, 1),
  "Spatial"), nlevels = 10)

# Drop lowest density polygon
baea_i_smooth_polys <- st_intersection(baea_i_smooth$polygons,
  baea_i_smooth$polygons %>% arrange(level) %>% slice(-1))

# Isolate highest density polygon
baea_i_smooth_poly1 <- st_intersection(baea_i_smooth$polygons,
  baea_i_smooth$polygons %>% arrange(rev(level)) %>% slice(1))

# Get extents of isopleths
polys_ext <- CreateExtentSF(baea_i_smooth_polys, 1, 1)
mapview(polys_ext) + mapview(baea_i_smooth_polys)
poly1_ext <- CreateExtentSF(baea_i_smooth_poly1, 3, 1)
mapview(poly1_ext) + mapview(baea_i_smooth_poly1)

# Get basemaps
# c("osm",  "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor",
#   "esri", "esri-topo",  "nps", "osm-transport", "osm-public-transport")
osm_polys <- read_osm(polys_ext, ext = 1, type = "osm")
osm_poly1 <- read_osm(poly1_ext, ext = 1, type = "osm")

# All but lowest density isopleth
tm_shape(osm_polys) + tm_rgb() +
tm_shape(baea_i_smooth$raster) +
tm_raster("count", alpha = .5) +
tm_shape(baea_i_smooth$iso) +
tm_iso("black", size = .5, fontcolor="black")

# Highest density isopleth
tm_shape(osm_poly1) + tm_rgb() +
tm_shape(baea_i_smooth_polys) +
tm_fill("level", alpha = .5) +
tm_borders()

# All flight paths and points
tm_shape(omr) + tm_rgb() +
  tm_shape(baea_i_lines) +
  tm_lines("purple", lwd = .5, alpha = .5) +
  tm_shape(baea_i) +
  tm_dots(col = "black") +
  tm_layout(main.title = paste0(id_i, " Locations"),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = TRUE, position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 2, 4), size = .75,
    position = c(.15, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2,
    position = c(.85, .85)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")

baea_i_ext <- st_as_sfc(st_bbox(baea_i))
maine_outline <- tm_shape(osm_maine) +
  tm_rgb() +
  tm_shape(maine) + # setting this as master sets lat/long
  tm_borders(col = "black") +
  tm_shape(baea_i_ext) +
  tm_borders(col = "red")

vp_me <- viewport(x = 0.9, y = 0.25, width = 0.2, height = 0.2)
print(maine_outline, vp = vp_me)

tmap_save(tm = baea_i_overview, filename =
  "C:/Work/R/Projects/baea_ibm/Products/Maps/Nests/BAEA_i_Overview.svg",
  insets_tm = maine_outline, insets_vp = vp_me,
  unit = "in", dpi = 300, height = 5, width = 5)

## INTERACTIVE Maps ------------------------------------------------------------

tmap_mode("view")

# Create interactive map
tm_basemap(leaflet::providers$Stamen.Watercolor) +
tm_shape(nests_study, bbox = "Maine, USA") +
  tm_dots(col = "red")

# Set new options
opts <- tmap_options(
  basemaps = c(Canvas = "Esri.WorldGrayCanvas", Imagery = "Esri.WorldImagery"),
  overlays = c(Labels = paste0("http://services.arcgisonline.com/arcgis/rest/",
    "services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")))

# Same data, but using new tmap options
tm_shape(nests_study, bbox = "Maine, USA") +
  tm_dots(col = "red")

## -------------------------------------------------------------------------- ##
###############################   OLD CODE   ###################################
## -------------------------------------------------------------------------- ##

# baea_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
#   "Data/BAEA")
# baea_org <- readRDS(file.path(baea_dir, "baea.rds"))
# baea <- st_as_sf(x = baea_org, coords = c("long_utm", "lat_utm"),
#   crs = 32619) #  crs = "+proj=longlat +datum=WGS84")
# id_i <- "Sandy"
# year_i <- 2017
# baea_i <- baea %>% filter(id == id_i) %>% filter(year == year_i)
# <- CreateExtentSF(baea_i)
#
# (osm_ext <- CreateExtentSF(baea_i, .5, 1))
# osm_baea_i <- read_osm(osm_ext, ext=1, type = "esri")
#
# sf_object <- baea_i
# ext = .5
# ratio = 1.5
# #bb_i_ext <- st_as_sfc(st_bbox(baea_i))
#
# CreateExtentSF <- function(sf_object,
#                            ext = 1,
#                            ratio = 1){
#   bb_i <- sf::st_bbox(extent(sf_object)*ext)
#   bb_x_mid <- as.numeric(floor((bb_i$xmax + bb_i$xmin)/2))
#   bb_y_mid <- as.numeric(floor((bb_i$ymax + bb_i$ymin)/2))
#   if (((bb_i$xmax + bb_i$xmin)/2) >= ((bb_i$ymax + bb_i$ymin)/2)){ # x longer
#     dist <- as.numeric(bb_i$xmax) - bb_x_mid
#   } else {
#     dist <- as.numeric(bb_i$ymax) - bb_y_mid
#   }
#   bb_i[1] <- bb_x_mid - dist
#   bb_i[2] <- bb_y_mid - dist*ratio
#   bb_i[3] <- bb_x_mid + dist
#   bb_i[4] <- bb_y_mid + dist*ratio
#   bb_sf <- st_as_sfc(bb_i, crs = crs(sf_object))
#   st_crs(bb_sf) <- st_crs(sf_object)
#   return(bb_sf)
# }
#
