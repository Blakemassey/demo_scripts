# Load libraries, scripts, and input parameters
suppressPackageStartupMessages(library(AICcmodavg))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(optimx))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(smoothie))
#suppressPackageStartupMessages(library(spatialfil))
suppressPackageStartupMessages(library(spatialEco))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(velox))
suppressPackageStartupMessages(library(viridis))
library(baear)
library(gisr)
library(ibmr)
source("R/Functions/FUN_Bandwidth_Optimization.R")
source("R/Functions/FUN_Kernel_Smoothing_2D.R")
cols <- colorRampPalette(rev(brewer.pal(9, "Spectral")))(100)
cols <- viridis(100)

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
buff <- 0
ext <- extent(357500 + buff, 377660 - buff, 4936600 + buff, 4956700 - buff)
developed <- crop(developed_full, ext)
elev <- crop(elev_full, ext)
elev[elev <= cellStats(elev, median)] <- 0
elev[elev > cellStats(elev, median)] <- 1
forest <- crop(forest_full, ext)
open_water <- crop(open_water_full, ext)

# Plot layers
# plot(elev)
# plot(developed)
# plot(forest)
# plot(open_water)

# Select covariate and bandwith
covar_ras = elev
covar_name <- str_replace(names(covar_ras), "_30mc", "")
nrow(covar_ras); ncol(covar_ras)
plot(covar_ras, main = covar_name)

# Inputs
in_bandwidth = 90
in_intercept = -3
in_beta1 = 6
in_sigma <- in_bandwidth/xres(covar_ras)

# Smooth layer
covar_ras_smooth <- raster(covar_ras) # creates blank raster
values(covar_ras_smooth) <- gauss2dsmooth(as.matrix(covar_ras),
  lambda = in_sigma, nx = nrow(covar_ras), ny = ncol(covar_ras))

# Predict UA layer
prob_ras <- raster(covar_ras) # creates blank raster
prob_vec <- in_intercept + in_beta1 * values(covar_ras_smooth)
values(prob_ras) <- plogis(prob_vec)
ua_ras <- raster(covar_ras) # creates blank raster
values(ua_ras) <- rbinom(length(values(prob_ras)), 1, values(prob_ras))
plot(prob_ras)  # CHANGE to more consistent GGPLOT graph!!

# Move to be included in other set of "Input" Graphs
gkern_raster_30m <- CreateGaussKernRaster(sigma = in_sigma, shift = TRUE,
  cell_size = 30)
plot(gkern_raster_30m)
Plot3DRaster(gkern_raster_30m, col = viridis(100), x_lab = "Longitude",
  y_lab = "Latitude", main = "Gaussian Kernel (30m cells)")
PlotGaussKernSlice(sigma = in_sigma, nrow = nrow, shift_n = 0, cell_size = 30,
  F, F)

# cut out some points and limit the analysis to a subset of locations
ua_full <- data.frame(coordinates(ua_ras), case = values(ua_ras))
ua_data <- bind_rows(ua_full %>% filter(case == 1) %>% sample_n(200),
  ua_full %>% filter(case == 0) %>% sample_n(200))

PlotGaussKernSlice(sigma = 1, nrow = 21, shift_n = 0, cell_size = 30, T, T)

gg <- ggplot(ConvertRasterForGGPlot(covar_ras_smooth, 10000), aes(x, y)) +
  geom_tile(aes(fill = value)) +
#  scale_fill_distiller(name = "Value", palette="Greens", direction = 1) +
  geom_point(data=ua_data, aes(x=x, y=y, color=factor(case))) +
#  scale_fill_viridis_d(option= "A") +
  scale_color_manual(name = "Case", values = c("1"= "magenta", "0" = "black"),
    labels = c("0", "1")) +
#  scale_color_manual(name = "Case", values = c("1"= "green", "0" = "red"),
#    labels = c("0", "1")) +
#  scale_color_viridis_d(option= "B") +
  scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
  coord_fixed(ratio = 1)

# Best visual combo seems to be Greens, Magenta/Black

grid.arrange(
  gg + scale_fill_viridis_c(option = "A"),
  gg + scale_fill_viridis_c(option = "B"),
  gg + scale_fill_viridis_c(option = "C"),
  gg + scale_fill_viridis_c(option = "D"),
  nrow = 2, ncol = 2
)


grid.arrange(
  gg + scale_fill_distiller(name = "Value", palette="Blues", direction = 1) ,
  gg + scale_fill_distiller(name = "Value", palette="Greens", direction = 1),
  gg + scale_fill_distiller(name = "Value", palette="Reds", direction = 1),
  gg + scale_fill_distiller(name = "Value", palette="Greys", direction = 1),
  nrow = 2, ncol = 2
  )




hist(covar_ras_smooth)
case <- ua_data$case
cell_nums <- cellFromXY(ua_ras, ua_data[,c("x","y")])
temp_ras <- raster(covar_ras) # blank raster needed for nll_kern_bw() internals
covar_mat <- as.matrix(covar_ras) # matrix needed for nll_kern_bw() internals

start_intercept = -2
start_beta1 = 4
start_sigma = 2
parms = c(start_intercept, start_beta1, log(start_sigma))

# Run model optimization procedures
tic("optimx", quiet = TRUE, func.tic = tic_msg)
optimx_results <- optimx(par = parms, fn = nll_kern, case = case,
  cell = cell_nums, temp_ras = temp_ras, covar_mat = covar_mat,
  control = list(trace=3))
toc(quiet = FALSE, func.toc = toc_msg, info = "Finished")
tic("nlm", quiet = TRUE, func.tic = tic_msg)
nlm_results <- nlm(nll_kern, p=parms, case = case, cell = cell_nums,
  temp_ras = temp_ras, covar_mat = covar_mat, print.level = 2,
  hessian = TRUE)
toc(quiet = FALSE, func.toc = toc_msg, info = "Finished")


# Compare input, start values, and optimization procedures results
print(paste0("In values: intercept = ", in_intercept, ", beta1 = ", in_beta1,
  ", sigma = ", in_sigma))
print(paste0("Start values: intercept = ", start_intercept, ", beta1 = ",
  start_beta1, ", sigma = ", start_sigma))
print(paste0("nlm fit results: intercept = ", signif(nlm_results$estimate[1],2),
  ", beta1 = ", signif(nlm_results$estimate[2], 2), ", sigma = ",
  signif(exp(nlm_results$estimate[3] + 1),2)))
print(paste0("optimx method1 results: intercept = ", signif(optimx_results[1,1],
  2), ", beta1 = ", signif(optimx_results[1,2],2), ", sigma = ",
  signif(exp(optimx_results[1,3]) + 1, 2)))
print(paste0("optimx method2 results: intercept = ", signif(optimx_results[2,1],
  2), ", beta1 = ", signif(optimx_results[2,2],2), ", sigma = ",
  signif(exp(optimx_results[2,3]) + 1,2)))

nll_kern <- function(parms,
                     case,
                     cell,
                     temp_ras = temp_ras,
                     covar_mat = covar_mat){
  # Parameters
  b0 <- parms[1]          # intercept
  beta1 <- parms[2]       # slope
  sigma <- exp(parms[3])+1   # sigma parameter
  # Gaussian-weighted smooth of matrix
  values(temp_ras) <- gauss2dsmooth(covar_mat, lambda=sigma, nx=nrow(temp_ras),
    ny=ncol(temp_ras))
  # Compute binomial success probabilities
  probs <- plogis(b0 + beta1*(temp_ras[cell]))
  # Evaluate log of binomial pmf
  lik <- dbinom(case, 1, probs, log=TRUE)
  nll <- -1*sum(lik)
  return(nll)
}

##  Display Logistic Function --------------------------------------------------
PlotLogisticZeroOneRange(in_intercept, in_beta1)

## Fit univariate scales -------------------------------------------------------
## Create covariate layer at various bandwidths then fit models for each

bandwidths <- c(seq(0, 270, by=30), seq(300, 1350, by=150),
  seq(1500, 4500, by=300))  # rafdius (meters)
covariate_cols <- paste0(rep(covar_name, each=length(bandwidths)),
    rep(bandwidths, times=1))
ua_data[, covariate_cols] <- NA

for(m in seq_along(bandwidths)){
  sigma <- bandwidths[m]/xres(covar_ras)
  col_name <- paste0(covar_name, bandwidths[m])
  print(paste0("Starting:", col_name))
  if(bandwidths[m] == 0){
    ua_data[, col_name] <- covar_ras[cell_nums]
  } else {
    values(temp_ras) <- gauss2dsmooth(covar_mat, lambda=sigma,
      nx=nrow(temp_ras), ny=ncol(temp_ras))
    ua_data[, col_name] <- temp_ras[cell_nums]
    temp_ras[] <- NA
  }
}

all_models <- data.frame(covar_type = covar_name,
  bw = bandwidths, aic = NA, coef = NA, opt_bw = NA)
opt_models <- cbind(data.frame(covar_type = unique(covar_name)), bw = NA)

colnames_i <- str_subset(colnames(ua_data), covar_name)
bandwidths_i <- unique(str_replace_all(colnames_i, "[:^digit:]", ""))
covar_models_list <- vector("list", length(bandwidths_i))
names(covar_models_list) <- as.character(bandwidths_i)
for (j in seq_along(bandwidths_i)){
  covar_bw_name <- paste0(covar_name, bandwidths_i[j])
  model_formula <- as.formula(paste("case ~ ", covar_bw_name))
  covar_model <- glm(model_formula, family=binomial, data = ua_data)
  covar_models_list[[j]] <- covar_model
  row_num = which(all_models$covar_type == covar_name &
      all_models$bw == bandwidths[j])
  print(row_num)
  covar_mod_aic <- AIC(covar_model) #$aic
  covar_mod_coef <- as.numeric(coef(covar_model)[1])
  all_models[row_num, "aic"] <- ifelse(is.null(covar_mod_aic), NA, covar_mod_aic)
  all_models[row_num, "coef"] <- ifelse(is.null(covar_mod_coef), NA, covar_mod_coef)
}
#lapply(covar_models_list, summary)
bandwidths_x <- seq(0, max(bandwidths), by=300)
aic_table <- aictab(covar_models_list, second.ord = FALSE) %>% arrange(AIC)
aic_table$covar_type <- covar_name
aic_table$opt_bw <- as.numeric(as.character(aic_table[1,1]))
opt_bw <- as.numeric(as.character(aic_table[1,1]))
all_models[all_models$covar_type == covar_name, "opt_bw"] <- opt_bw
opt_models[opt_models$covar_type == covar_name, "bw"] <- opt_bw
g <- ggplot(aic_table, aes(x = as.numeric(as.character(Modnames)), y = AIC)) +
  #geom_line(color="red") +
  geom_point() + xlab("Bandwidth") +
  ggtitle(covar_name) +
  theme_no_legend
if (nrow(aic_table) > 1){
  g <- g +
    scale_x_continuous(breaks = as.numeric(bandwidths_x)) +
    geom_vline(xintercept = opt_bw, color="blue", linetype='dashed') +
    geom_line(color="red") +
    annotate("text",
      x = opt_bw + (diff(range(as.numeric(aic_table$Modnames)))*.025),
      y = max(aic_table$AIC), label = as.character(opt_bw), color = "blue")
}
g

##----------------------------------------------------------------------------##
################# Chris Sutherland's original script  ##########################
##----------------------------------------------------------------------------##

par(mfrow=c(2,2))

#original layer
plot(terrestrial)

#smoothed layer
bw <- 900 / 15
zmat <- as.matrix(r.terrestrial)
zmat[zmat>0] <- 1
f <- kernel2dsmooth(zmat,kernel.type="gauss", nx=nrow(r.terrestrial),
  ny=ncol(r.terrestrial), sigma = bw)
r.terrestrial.smooth <- r.terrestrial
values(r.terrestrial.smooth) <- f
dev.new()
plot(r.terrestrial.smooth)

#binary surface
r.lp <- r.terrestrial.smooth
r.z <- r.terrestrial.smooth
lin.pred <- 3 + -30 * values(r.terrestrial.smooth)
values(r.lp) <- plogis(lin.pred)
plot(r.lp)
z <- rbinom(length(values(r.z)), 1, values(r.lp))
values(r.z) <- z
plot(r.z)

# cut out some points and limit the analysis to a subset of locations
surface <- data.frame(coordinates(r.terrestrial),values(r.terrestrial))
pick <- 1:nrow(surface)#sample(1:nrow(surface), 1000, replace=FALSE)
system.time(out <- nlm(lik,c(-0.89747,2.323, bw),z=z[pick],
  surface=surface[pick,],hessian=TRUE))

##----------------------------------------------------------------------------##
################################# OLD CODE #####################################
##----------------------------------------------------------------------------##

# # Running optimization only on bandwidth parameter (which doesn't seem to work!)
# # Run model optimization procedures
# tic("nlm", quiet = TRUE, func.tic = tic_msg)
# nlm_results <- nlm(nll_sigma, p=log(start_sigma), case = case, cell = cell_nums,
#   temp_ras = temp_ras, covar_mat = covar_mat, print.level = 2,
#   hessian = TRUE)
# optimx_results <- optimx(par = log(start_sigma), fn = nll_sigma, case = case,
#   method = "BFGS", lower = 0 , upper = log(100),
#   cell = cell_nums, temp_ras = temp_ras, covar_mat = covar_mat,
#   control = list(trace=3))
# toc(quiet = FALSE, func.toc = toc_msg, info = "Finished")
#
# # Compare input, start values, and optimization procedures results
# print(paste0("Model values: intercept = ", intercept, ", beta1 = ", beta1,
#   ", sigma = ", sigma))
# print(paste0("Start values: intercept = ", start_intercept, ", beta1 = ",
#   start_beta1, ", sigma = ", start_sigma))
# print(paste0("nlm fit results: sigma = ",
#   signif(exp(nlm_results$estimate[1]) + 1, 3)))
# print(paste0("optimx fit results: sigma = ",
#   signif(exp(optimx_results[1,1]) + 1, 3)))
#
#
# nll_sigma <- function(parms,
#                       case,
#                       cell,
#                       temp_ras = temp_ras,
#                       covar_mat = covar_mat){
#   # Parameters
#   sigma <- parms[1]   # sigma parameter
#   # Gaussian-weighted smooth of matrix
#   values(temp_ras) <- gauss2dsmooth(covar_mat, lambda=sigma, nx=nrow(covar_ras),
#     ny=ncol(covar_ras))
#   # Compute binomial success probabilities
#   #probs <- plogis(b0 + beta1*(covar_ras[cell]))
#   # Evaluate log of binomial pmf
#   #lik <- dbinom(case, 1, probs, log=TRUE)
#   model_fit <- glm(case ~ temp_ras[cell], family=binomial)
#   nll <- -1*logLik(model_fit)
#   return(as.numeric(nll))
# }

