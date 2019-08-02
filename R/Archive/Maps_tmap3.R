geo_object <- CreateExtentSF(maine) %>% st_transform(., crs = 4326) %>%
  as(., "Spatial")
geo_object <- CreateExtentSF(baea_i_smooth_polys, 1, 1) %>%
  st_transform(., crs = 4326) %>% as(., "Spatial")

mapview(geo_object)

om <- openmap(c(extent(geo_object)[4], extent(geo_object)[1]),
	c(extent(geo_object)[3],extent(geo_object)[2]), minNumTiles=15L,type = map_url
)
tab <- as.factor(om[1][[1]][[1]]$colorData)  # MUST BE DONE FOR ESRI LAYERS
omr <- raster::raster(om)
omr <- raster::setValues(omr, as.integer(tab) - 1L)
raster::colortable(omr) <- levels(tab)
attr(omr, "is.OSM") <- TRUE

baea_i_test <- tm_shape(omr) + tm_rgb() +
 tm_shape(baea_i_smooth_polys) +
 tm_fill("level", alpha = .5) +
 tm_borders()

baea_i_test

tmap_save(tm = baea_i_test, filename =
  "C:/Work/R/Projects/baea_ibm/Products/Maps/Nests/BAEA_i_Overview.svg",
#  insets_tm = maine_outline, insets_vp = vp_ME,
  unit = "in", dpi = 300, height = 5, width = 5)

install.packages("rcanvec")
rosm::osm.types()

library(rosm)
library(rcanvec)
library(prettymapr)
library(sp)
# basic ploting
osm.plot(nsbox)
osm.plot(nsbox, type="thunderforestoutdoors", res = 300)
bmaps.plot(nsbox)
bmaps.plot(nsbox, type="Road")
# use {prettymapr} to add scalebar and north arrow
prettymap(osm.plot(nsbox))
prettymap(bmaps.plot(nsbox, type="Road"))
# increase res argument to plot to file
pdf(height=8, width=10.5)
prettymap(osm.plot(nsbox, type="stamenbw", res=300, stoponlargerequest=FALSE),
scale.label.col="white", arrow.text.col = "white",
scale.linecol = "white", arrow.border = "white")
dev.off()

d <- geocode("Maine")
nsbox <- makebbox(d$bbox_n, d$bbox_e, d$bbox_s, d$bbox_w)
as.tile_source


# register a custom tile source

register_tile_source(dark = "http://a.basemaps.cartocdn.com/dark_all/${z}/${x}/${y}.png")
register_tile_source(thunder =
    "https://tile.thunderforest.com/outdoors/${z}/${x}/${y}.png?apikey=9b0b8a66a3e74b47be31f38597e7f9e7")

library(prettymapr)
d <- geocode("Maine") # , source="google")
nsbox <- makebbox(d$bbox_n, d$bbox_e, d$bbox_s, d$bbox_w)
osm.plot(nsbox, "thunder", zoomin = 6, zoom = 1) # THIS IS THE BEST BET!!!!!

test_osm_download <- osm.raster(nsbox, "thunder", zoomin = 1, zoom = 10, res = 300,
  filename = "C:/ArcGIS/Data/Temporary/OSM_Downloads/Maine.tif", overwrite = TRUE) # THIS IS THE BEST BET!!!!!
plotRGB(test_osm_download)


str(test_image)
plotRGB(test_image)

test_osm_raster <- raster("C:/ArcGIS/Data/Temporary/OSM_Downloads/Maine.tif")
plot(test_osm_raster)

plot(test_raster)
tm_shape(test_raster) + tm_raster()
plotRGB(test_osm_raster, 1, 2, 3)

tm_rgb
tm_raster
