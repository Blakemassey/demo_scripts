# Load libraries, scripts, and input parameters
suppressPackageStartupMessages(library(AICcmodavg))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(optimx))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(smoothie))
suppressPackageStartupMessages(library(spatialEco))
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(tidyverse))
library(baear)
library(gisr)
library(ibmr)
source("R/Assets/Assets_Bandwidth_Optimization.R")

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

rm(developed_file, elev_file, forest_file, open_water_file)

# Crop rasters
buff <- 0
ext <- extent(357500 + buff, 377660 - buff, 4936600 + buff, 4956700 - buff)
developed <- crop(developed_full, ext)
elev <- crop(elev_full, ext)
elev[elev <= cellStats(elev, median)] <- 0
elev[elev > cellStats(elev, median)] <- 1
forest <- crop(forest_full, ext)
open_water <- crop(open_water_full, ext)
rm(buff, ext, developed_full, elev_full, forest_full, open_water_full)

# Select covariate, ratios, betas, and bandwidths
covar_ras = elev
ratio <- c(.5) #.3, .4, .6, .7
beta1 <- seq(5, 40, by = 5)
bw <- c(seq(0, 300, by = 60), 450, seq(600, 1500, by = 300))

covar_name <- str_replace(names(covar_ras), "_30mc", "")
sigma <- bw/xres(covar_ras)

df_log <- crossing(ratio, beta1 = c(beta1, -1*beta1)) %>%
  mutate(intercept = (beta1 * ratio)*-1)

#  Running optimization on ALL COMBOS of ratio, beta, and bw -------------------
df_all <- df_log %>% expand(nesting(ratio, beta1), sigma) %>%
  mutate(intercept = (beta1 * ratio)*-1) %>%
  dplyr::select(sigma, ratio, beta1, intercept) %>%
  arrange(sigma, ratio, beta1)

df_all_grp <- df_all %>% group_by(sigma) %>% nest()

CreateSmooth <- function(sigma = sigma){
  if (sigma >= 1) {
    covar_ras_smooth <- raster(covar_ras) # creates blank raster
    values(covar_ras_smooth) <- gauss2dsmooth(as.matrix(covar_ras),
      lambda = sigma, nx = nrow(covar_ras), ny = ncol(covar_ras))
  } else {
    covar_ras_smooth <- covar_ras
  }
  return(covar_ras_smooth)
}

smoothed <- df_all_grp %>% dplyr::select(-data) %>% pmap(., CreateSmooth)
smoothed_brick <- brick(smoothed)
names(smoothed_brick) <- paste0(covar_name, df_all_grp$sigma)

CreateUAData <- function(sigma = sigma, intercept=intercept, beta1=beta1){
  raster_name <- paste0(covar_name, sigma)
  covar_ras_smooth <- subset(smoothed_brick, subset = raster_name)
  prob_ras <- raster(covar_ras) # creates blank raster
  prob_vec <- intercept + beta1 * values(covar_ras_smooth)
  values(prob_ras) <- plogis(prob_vec)
  #PlotBWRaster(prob_ras, "Probability Layer", color_pal)
  ua_ras <- raster(covar_ras) # creates blank raster
  values(ua_ras) <- rbinom(length(values(prob_ras)), 1, values(prob_ras))
  ua_full <- data.frame(coordinates(ua_ras), case = values(ua_ras))
  ua_data <- bind_rows(ua_full %>% filter(case == 1) %>% sample_n(200),
    ua_full %>% filter(case == 0) %>% sample_n(200))
  cell_nums <- cellFromXY(ua_ras, ua_data[,c("x","y")])
  ua_data[, "cell_num"] <- cell_nums
  covar_mat <- as.matrix(covar_ras)
  print(paste0("Starting: ", raster_name, " (intercept = ", intercept,
    ", beta1 = ", beta1, ")"))
  ua_data[, "value"] <- covar_ras[cell_nums]
  return(ua_data)
}

ua_data <- df_all %>% dplyr::select(-ratio) %>% pmap(., CreateUAData)
df_all$ua_data <- ua_data

mod_fun <- function(df){
  covar_model <- glm(case ~ value, family=binomial(link="logit"), data = df)
  return(covar_model)
}

df_all <- df_all %>%
  mutate(model = map(ua_data, mod_fun))
warnings()

ExtractIntercept <- function(mod) as.numeric(coef(mod)[1])
ExtractBeta <- function(mod) as.numeric(coef(mod)[2])
ExtractAIC <- function(mod) as.numeric(AIC(mod))

df_all2 <- df_all %>%
  mutate(est_beta1 = map_dbl(model, ExtractBeta)) %>%
  mutate(est_intercept = map_dbl(model, ExtractIntercept)) %>%
  mutate(aic = map_dbl(model, ExtractAIC)) %>%
  mutate(bias_beta = beta1 - est_beta1) %>%
  mutate(bias_intercept = intercept - est_intercept)


optimx_fun <- function(est_intercept, est_beta1, ua_data, row_n){
  start_sigma <- 1
#est_intercept <- 3.9
#est_beta1 <- 3.9
  covar_mat <- as.matrix(covar_ras) # needed for internals in optimx_fun
  print(paste0("Starting: row_n = ", row_n))
  print(paste0("Starting: est_intercept = ", signif(est_intercept, 2),
    ", est_beta1 = ", signif(est_beta1, 2),
    ", start_sigma = ", signif(start_sigma, 2)))
  parms = c(est_intercept, est_beta1, log(start_sigma))
#cases <- df_all2$ua_data[[1]][,"case"]
#cell_nums <- df_all2$ua_data[[1]][,"cell_num"]
  cases <- ua_data[,"case"]
  cell_nums <- ua_data[,"cell_num"]
  optimx_results <- optimx(par = parms, fn = nll_kern, cases = cases,
    cell_nums = cell_nums, temp_ras = temp_ras, covar_mat = covar_mat,
    control = list(trace=3))
  return(optimx_results)
}

nll_kern <- function(parms,
                     cases,
                     cell_nums,
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
  probs <- plogis(b0 + beta1*(temp_ras[cell_nums]))
  # Evaluate log of binomial pmf
  lik <- dbinom(cases, 1, probs, log=TRUE)
  nll <- -1*sum(lik)
  return(nll)
}

covar_mat <- as.matrix(covar_ras) # needed for internals in optimx_fun
temp_ras <- raster(covar_ras) # needed for internals in optimx_fun

optimx_results <- df_all2 %>%
  dplyr::select(est_intercept, est_beta1, ua_data) %>%
  mutate(row_n = 1:n()) %>%
  pmap(optimx_fun)

df_all2$optimx_results <- optimx_results

saveRDS(df_all2, file = paste0("Results/Bandwidth_Optimization/df_all2_",
  lubridate::today(), ".rds"))

# Work on retrieving results !!!



# Plot Intital Estimate Results ------------------------------------------------

df_sum <- df_all2 %>% dplyr::select(-c(ua_data, model))

ggplot(df_sum, aes(x = beta1, y = bias_beta)) +
  geom_boxplot(aes(group = cut_width(beta1, 5), color = as.factor(sigma))) +
  facet_grid( ~ sigma, scale = "free_y", labeller = label_both) +
  ylim(-100, 100)

unique(df_sum$beta1)
table(df_sum$sigma, df_sum$beta1)

# Plotting ALL Logisitic Functions ---------------------------------------------
LogisticProb <- function(intercept, beta1) {
  predictors <- seq(-.25, 1.25, by = .05)
  predictors_logit <- intercept + beta1*(predictors)
  probs <- plogis(predictors_logit)
  df <- data.frame(predictors, predictors_logit, probs)
  ss <- split.data.frame(df, df$predictors)
  return(lapply(ss, "[[", "probs"))
}

df_log_prob <- bind_cols(df_log %>% dplyr::select(ratio),
  df_log %>% dplyr::select(intercept, beta1) %>%
    bind_cols(pmap_df(., LogisticProb)))

df_log_gather <- tidyr::gather(df_log_prob, "predictor", "probability",
  "-0.25":"1.25")

rect_df <- data.frame(xmin = c(-.25, 1), ymin=c(0,0), xmax= c(0,1.25),
    ymax = c(1,1))

ggplot(df_log_gather) + geom_line(aes(as.numeric(predictor), probability)) +
  geom_rect(data = rect_df, alpha = .5, aes(xmin=xmin, ymin=ymin, xmax=xmax,
    ymax=ymax)) +
  geom_segment(aes(x = ratio, y = 0, xend = ratio, yend = 1),
    color = "red") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0)) +
  coord_fixed(ratio = 1/1) + facet_grid(ratio ~ beta1, labeller = label_both) +
  labs(x = "Predictor", y = "Probability") # +
#  geom_text(data=df_log_prob, aes(x=.85, y=.85, label=as.character(intercept)),
#  size = 3, colour="blue", inherit.aes=FALSE, parse=FALSE)

# Plotting Fitted Estimates of Logisitic Functions -----------------------------

# Sigma 5 only
sigma_select <- 5

df_est <- bind_cols(
  df_sum %>%
    filter(sigma == sigma_select) %>%
    dplyr::select(ratio, intercept, beta1),
  df_sum %>%
    filter(sigma == sigma_select) %>%
    mutate(intercept = est_intercept, beta1 = est_beta) %>%
    dplyr::select(c(intercept, beta1)) %>%
    bind_cols(pmap_df(., LogisticProb)))

df_est_gather <- tidyr::gather(df_est, "predictor", "probability",
  "-0.25":"1.25") %>%
  mutate(est_predictor = predictor, est_probability = probability) %>%
  dplyr::select(est_predictor, est_probability)

df_log_est_gather <- bind_cols(df_log_gather, df_est_gather)

ggplot(df_log_est_gather) +
  geom_line(aes(as.numeric(predictor), probability), color = "blue") +
  geom_line(aes(as.numeric(est_predictor), est_probability), color = "darkgreen") +
  geom_rect(data = rect_df, alpha = .5, aes(xmin=xmin, ymin=ymin, xmax=xmax,
    ymax=ymax)) +
  geom_segment(aes(x = ratio, y = 0, xend = ratio, yend = 1),
    color = "red") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0)) +
  coord_fixed(ratio = 1/1) + facet_grid(ratio ~ beta1, labeller = label_both) +
  labs(x = "Predictor", y = "Probability") +
  geom_text(data=df_sum %>% filter(sigma == sigma_select),
    aes(x=.85, y=.85, label=as.character(signif(est_beta, 2))),
    size = 3, colour="darkgreen", inherit.aes=FALSE, parse=FALSE)


