# Load Packages
suppressPackageStartupMessages(library(CircStats))
suppressPackageStartupMessages(library(circular))
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fitdistrplus))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(movMF))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(texmex))
suppressPackageStartupMessages(library(zoo))
options(stringsAsFactors = FALSE)
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# ALSO BE SURE TO LOAD FUNCTIONS AT BOTTOM OF SCRIPT!!!

## Start of script to get mixed Von Mises parameters and generate kernel ------

# load baea.rds
baea <- readRDS("C:/Users/blake/downloads/baea.rds") # Change to your location

baea_steps <- AddStepLengthAndAngles(baea) %>%
  filter(!is.na(turn_angle)) %>%
  filter(step_length > 0)

baea_flights <- baea_steps  %>% filter(speed > 2)

cauchy_pars <- mledist(baea_flights$turn_angle,
  distr = "wrappedcauchy", silent=TRUE,
  start=list(mu=pi/2, rho=.5),
  lower=c(mu=0, rho=0),
  upper=c(mu=2*pi, rho = 1))

pareto_pars <- mledist(baea_flights$step_length, silent = TRUE,
  distr = "gpd", start = list(sigma = 1, xi = .5))

# Kernel with Cauchy and Pareto components
move_kernel <- CreateMoveKernel(max_r = 1500, cellsize = 30,
    mu = cauchy_pars$estimate["mu"],
    rho = cauchy_pars$estimate["rho"],
    shape = pareto_pars$estimate["xi"], #xi=shape
    scale = pareto_pars$estimate["sigma"], #sigma=scale
    ignore_cauchy = FALSE, ignore_pareto = FALSE)

r <- (30*((nrow(move_kernel)-1)/2))+(30/2)
move_raster <- raster::raster(move_kernel, xmn=-r, xmx=r, ymn=-r, ymx=r)
plot(move_raster)

# Kernel with only the Cauchy Component
cauchy_kernel <- CreateMoveKernel(max_r = 1500, cellsize = 30,
    mu = cauchy_pars$estimate["mu"],
    rho = cauchy_pars$estimate["rho"],
    shape = pareto_pars$estimate["xi"], #xi=shape
    scale = pareto_pars$estimate["sigma"], #sigma=scale
    ignore_cauchy = FALSE, ignore_pareto = TRUE) # Notice 'ignore_pareto = TRUE'

r <- (30*((nrow(cauchy_kernel)-1)/2))+(30/2)
cauchy_raster <- raster::raster(cauchy_kernel, xmn=-r, xmx=r, ymn=-r, ymx=r)
plot(cauchy_raster)

######################## FUNCTIONS BELOW #######################################

#' AngleToPoint
#'
#' Calculates the angle (based on a unit circle, where due East is 0 and due
#' West is pi) between an origin (x,y) and a target (x,y).
#'
#' @usage CenterXYInCell(x, y, xmin, ymin, cellsize)
#'
#' @param origin_x starting location x value
#' @param origin_y starting location y value
#' @param target_x finishing location x value
#' @param target_y finishing location y value
#'
#' @return numeric
#' @export
#'
AngleToPoint <- function(origin_x,
                         origin_y,
                         target_x,
                         target_y){
  dx <- c(target_x - origin_x)
  dy <- c(target_y - origin_y)
  abs_angle <- atan2(dy, dx)
  abs_angle <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
}


#' Add step length and angle to location data
#'
#' Calculates step length, absolute angle (N=0), and turn angle between
#'   successive point locations
#'
#' @usage AddStepLengthAndAngles(df, by, datetime, long, lat)
#' @param df dataframe with locations and datetime data
#' @param by column name to use to split, analyze, and merge data, default is
#'   "id"
#' @param long longitude, must be in UTM, default is "long_utm"
#' @param lat latitude, must be in UTM, default is "lat_utm"
#'
#' @return dataframe with "step_length", "abs_angle", and "turn_angle" columns
#' @export
#'
#' @details Coordinates must be in identically-scaled units (e.g. UTM meters).
#'   If lat and long are in degrees, project coordinates to UTM with 'rgdal'
#'   package before running this function.
#'
AddStepLengthAndAngles <- function(df,
                                   by = "id",
                                   long = "long_utm",
                                   lat = "lat_utm"){
  df <- df
  ifelse(is.null(by), df$by <- "all", df$by <- df[,by])
  StepLengthAndAngles <- function(df=df, lat=lat, long=long){
    xy <- data.frame(x = df[,long], y = df[,lat])
    xy1 <- xy[-1, ]
    xy2 <- xy[-nrow(xy), ]
    step_length <- c(sqrt((xy1$x - xy2$x)^2 + (xy1$y - xy2$y)^2), NA)
    dx <- c(xy1$x - xy2$x, NA)
    dy <- c(xy1$y - xy2$y, NA)
    abs_angle <- ifelse(step_length < 1e-07, NA, (atan2(dy, dx)))
    abs_angle <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
    turn_angle <- c(abs_angle, 0) - c(0, abs_angle)
    turn_angle <- ifelse(turn_angle < 0, (2*pi) + turn_angle, turn_angle)
    turn_angle <- turn_angle[-length(turn_angle)]
    turn_angle[1] <- NA
    out <- cbind.data.frame(dx=dx, dy=dy, step_length=step_length,
      abs_angle=abs_angle, turn_angle=turn_angle) #,
  }
  uniques <- unique(df[,"by"])
  out <- data.frame()
  for (j in uniques){
    sv = df[,by] %in% j
    data <- subset(df, by==j)
    df2<- cbind(data, StepLengthAndAngles(df=data, lat=lat, long=long))
    out <- rbind(out, df2)
  }
  out$by<-NULL
  return(out)
}

#' CreateMoveKernel
#'
#' Create a movement kernel matrix based on a wrapped Cauchy distribution
#'   for direction and a Pareto distribution for distance.
#'
#' @usage CreateMoveKernel(max_r, cellsize, mu, rho, shape, scale,
#'    ignore_cauchy, ignore_pareto)
#'
#' @param max_r maximum radius of kernel in meters, default = 300
#' @param cellsize cell size in meters, default = 30
#' @param mu mu parameter of wrapped Cauchy distribution, 0 radians is due east
#'   because everything is based on the Unit Circle
#' @param rho rho parameter of wrapped Cauchy distribution
#' @param shape shape parameter of Pareto distribution
#' @param scale scale parameter of Pareto distribution
#' @param ignore_cauchy logical, removes cauchy kernel's contribution to output
#'   raster. Default is FALSE.
#' @param ignore_pareto logical, removes pareto kernel's contribution to output
#'    raster. Default is FALSE.
#'
#' @return matrix
#' @export
CreateMoveKernel <- function(max_r = 300,
                               cellsize = 30,
                               mu,
                               rho,
                               shape,
                               scale,
                               ignore_cauchy = FALSE,
                               ignore_pareto = FALSE) {
  # Create the empty kernel objects
  max_r_cells <- ceiling(max_r/cellsize)
  size <- max_r_cells * 2 + 1
  center <- max_r_cells + 1
  wrpc_kernel <- new("matrix", 0, size, size)
  gpd_kernel <- new("matrix", 0, size, size)
  for (i in 1:size) {
    for (j in 1:size) {
      r = sqrt((i - center)^2 + (j - center)^2) * cellsize
      b = AngleToPoint(center, center, j, i)
      if(r <= max_r){
        wrpc_kernel[i, j] <- round(suppressWarnings(circular::dwrappedcauchy(b,
          mu=mu, rho=rho)), 5)
        gpd_kernel[i, j] <- texmex::dgpd(r, sigma=scale, xi=shape, log=FALSE)
      }
    }
  }
  wrpc_kernel <- apply(wrpc_kernel, 2, rev)
  gpd_kernel[center, center] <- 1/scale
  # This last part deletes the cells at the edge if they are all zero
  if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
    wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
    wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
      - 1)]
  if (all(gpd_kernel[1, ] == 0, gpd_kernel[, 1] == 0,
    gpd_kernel[nrow(gpd_kernel),] == 0, gpd_kernel[, ncol(gpd_kernel)] == 0))
    gpd_kernel <- gpd_kernel[2:(nrow(gpd_kernel) - 1), 2:(ncol(gpd_kernel) - 1)]
  # Multiply the two kernels together and re-normalize
  if (ignore_cauchy) wrpc_kernel <- 1
  if (ignore_pareto) gpd_kernel <- 1
  move_kernel <- gpd_kernel*wrpc_kernel
  move_kernel <- move_kernel/sum(move_kernel)
  return(move_kernel)
}

