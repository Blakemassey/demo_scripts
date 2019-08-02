# Load libraries, scripts, and input parameters
suppressPackageStartupMessages(library(AICcmodavg))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(optimx))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(reproducible))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(smoothie))
suppressPackageStartupMessages(library(spatialfil))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(velox))
library(baear)
library(gisr)
library(ibmr)

# Landscape file locations
      base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
# kernel class
 developed_file <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
    forest_file <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_file <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"
   pasture_file <- "C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif"
shrub_herb_file <- "C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif"
   wetland_file <- "C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif"
# terrain class
      elev_file <- "C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif"
# extract class
hydro_dist_file <- "C:/ArcGIS/Data/R_Input/BAEA/hydro_dist_30mc.tif"

# Import rasters
elev <- raster(elev_file) # all other layers' extent are set to this layer
developed <- raster(developed_file)
forest <- raster(forest_file)
open_water <- crop(raster(open_water_file), elev)
pasture <- crop(raster(pasture_file), elev)
shrub_herb <- crop(raster(shrub_herb_file), elev)
wetland <- crop(raster(wetland_file), elev)
hydro_dist <- raster(hydro_dist_file)

extract_class <- c("hydro_dist")
kernel_class <- c("developed", "forest", "open_water", "pasture", "shrub_herb",
  "wetland")
terrain_class <- c("tpi", "tri", "roughness")

covar_stack <- stack(developed, forest, open_water, pasture, shrub_herb,
  wetland, elev, hydro_dist)
names(covar_stack) <- str_replace_all(names(covar_stack), "_30mc", "")
covar_types <- c(extract_class, kernel_class, terrain_class)

cell_size <- 30
bandwidths <- c(0, 30, 60, 300, 900, 1500, 2100) # radius (meters)

extent_ij <- extent(357590, 377660, 4936570, 4956670)

rm(developed, forest, open_water, pasture, shrub_herb, wetland, hydro_dist,
  elev)

#k <- 2; m <- 3

for (k in seq_along(covar_types)){
  covar_type_k <- covar_types[k]
  print(paste0("Covar: ", covar_type_k))
  if (covar_type_k %in% kernel_class){
    covar_layer_k <- which(names(covar_stack) == covar_type_k)
    covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),
      extent_ij)
    covar_matrix_k <- as.matrix(covar_raster_k)
    bandwidths_k <- bandwidths
  } else if (covar_type_k %in% extract_class){
    covar_layer_k <- which(names(covar_stack) == covar_type_k)
    covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),
      extent_ij)
    covar_matrix_k <- as.matrix(covar_raster_k)
    bandwidths_k <- 0
  } else if (covar_type_k %in% terrain_class){
    covar_layer_k <- which(names(covar_stack) == "elev")
    covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),
      extent_ij)
    bandwidths_k <- bandwidths
  }
  for(m in seq_along(bandwidths_k)){
    bw_meters <- bandwidths_k[m]
    print(paste0("Bandwidth: ", bw_meters))
    if(bw_meters == 0){
      covar_raster_calc_m <- covar_raster_k
    } else {
      if (covar_type_k %in% kernel_class){
        tic("Smoothie")
        covar_matrix_smooth_m <- kernel2dsmooth(covar_matrix_k,
          kernel.type="gauss", nx=nrow(covar_matrix_k),
          ny=ncol(covar_matrix_k), sigma = bandwidths_k[m]/cell_size)
        toc()
        covar_raster_calc_m <- raster(covar_matrix_smooth_m,
          template = covar_raster_k)
        plot(covar_raster_calc_m)
        rm(covar_matrix_smooth_m)
      } else if (covar_type_k %in% terrain_class){
        covar_raster_calc_m <- CalculateTerrainMetric(elev = covar_raster_k,
        size = ((bandwidths_k[m]/cell_size)*2 + 1), metric = covar_type_k)
      } else if (covar_type_k %in% extract_class){
        covar_raster_calc_m <- covar_raster_k
      }
    }
  }
}
