
# Working Script to download Thunderforest Data and and use it in tmap()!

library(rosm)
library(cartography)
library(raster)
library(prettymapr)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(prettymapr)
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(mapview))
register_tile_source(thunder = paste0("https://a.tile.thunderforest.com/",
  "outdoor/${z}/${x}/${y}.png?apikey=9b0b8a66a3e74b47be31f38597e7f9e7"))
register_tile_source(pioneer = paste0("https://a.tile.thunderforest.com/",
  "pioneer/${z}/${x}/${y}.png?apikey=9b0b8a66a3e74b47be31f38597e7f9e7"))
me_bb <- makebbox(47, -67, 43.5, -71)

om = openmap(c(lat= 47, lon= -72), c(lat= 44, lon= -68), minNumTiles = 9,
  type="stamen-terrain")
oms <- raster::raster(om) # convert OpenStreetMap to RasterStack
tab <- tmaptools:::raster_colors(raster::values(oms)) # tab = vec of hexdecimals
omr <- raster::raster(oms)
omr <- raster::setValues(omr, as.integer(tab) - 1L) # omr = Raster w/1-255 value
raster::colortable(omr) <- levels(tab)
attr(omr, "is.OSM") <- TRUE
tm_shape(omr) +
  tm_raster()

osm.plot(me_bb, type = "pioneer")
om2 <- osm.raster(me_bb, zoom = 7, type = "pioneer", overwrite=TRUE)
plotRGB(om2)
oms2 <- trim(stack(om2)) # Convert to Stack, then trim off NA cells
oms2[is.na(oms2)] <- 0
tab2 <- as.factor(rgb(oms2[], maxColorValue = 255))
omr2 <- raster::raster(oms2)
omr2 <- raster::setValues(omr2, as.integer(tab2) - 1L)
raster::colortable(omr2) <- levels(as.factor(tab2))
tm_shape(omr2) +
  tm_raster()
