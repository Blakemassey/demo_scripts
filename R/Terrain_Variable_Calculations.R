# Terrain
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(spatialEco))

# Roughness: difference between the maximum and the minimum value of a cell
#  and its surrounding cells.

# Topographic Position Index (TPI): is the difference between the value of a
#  cell and the mean value of its surrounding cells.

# Terrain Ruggedness Index (TRI): the mean of the absolute differences between
#  the value of a cell and the value of its surrounding cells.

# Compare raster::terrain() and my function, CalculateTerrainMetric()

elevation <- getData('alt', country='CHE')
tri03A <- terrain(elevation, opt=c('tri'), unit='degrees')
tri03B <- CalculateTerrainMetric(elevation, 3, "tri")
plot(tri03A, main = "TRI A")
plot(tri03B, main = "TRI B")

tpi03A <- terrain(elevation, opt=c('tpi'), unit='degrees')
tpi03B <- CalculateTerrainMetric(elevation, 3, "tpi")
plot(tpi03A, main = "TPI A")
plot(tpi03B, main = "TPI B")

rough03A <- terrain(elevation, opt=c('roughness'), unit='degrees')
rough03B <- CalculateTerrainMetric(elevation, 3, "roughness")
plot(rough03A, main = "Rough A")
plot(rough03B, main = "Rough B")


# Testing mosaic vs. merge

elev_1 <- crop(elevation, extent(elev_swz, 100, 145, 100, 120))
elev_2 <- crop(elevation, extent(elev_swz, 140, 160, 100, 120))
elev_3 <- crop(elevation, extent(elev_swz, 155, 200, 100, 120))
plot(elev_123)
#elev_2[] <- 5000

mosaic_123 <- mosaic(elev_1, elev_2, elev_3, fun=max)
plot(mosaic_123)

merge_123 <- merge(elev_1, elev_2, elev_3)
plot(merge_123)

merge_132 <- merge(elev_1, elev_3, elev_2)
plot(merge_132)


# Get center cell value only
library(gisr)
library(ibmr)
library(tictoc)

elev_30mc <- raster("C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif")

df <- data.frame(x = 506060, y = 5083430) # Mt. Katahdin
xy <- CenterXYWithBase(df, elev)
buffer <- 21000
ext <- c(xmin = (xy[1,1]-15)-buffer, xmax = (xy[1,1]+15)+buffer,
  ymin=(xy[1,2]-15)-buffer, ymax=(xy[1,2]+15)+buffer)

elev <- crop(elev_30mc, ext)
plot(elev)

tic()
tpi_21B <- CalculateTerrainMetric(elev, 21, "tri")
tri_21B <- CalculateTerrainMetric(elev, 21, "tpi")
roug_21B <- CalculateTerrainMetric(elev, 21, "roughness")
toc() # 65 seconds

plot(tpi_21B)
plot(tri_21B)
plot(roug_21B)





