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
ratio <- c(.5) #c(.3, .4, .6, .7)
beta1 <- seq(5, 10, by = 5) #seq(5, 40, by = 5)
bw <- c(seq(0, 300, by = 60), 450, seq(600, 1500, by = 300))

# Select optimization search parameters
bw_range <- c(0, 3000)

# Convert starting values to appropriate values for functions
covar_name <- str_replace(names(covar_ras), "_30mc", "")
sigma <- bw/xres(covar_ras)
sigma_range <- bw_range/xres(covar_ras)

df_beta01 <- crossing(ratio, beta1 = c(beta1, -1*beta1)) %>%
 mutate(beta0 = (beta1 * ratio)*-1)

#  Get ALL COMBOS of ratio, beta, and sigma ------------------------------------
df_all <- df_beta01 %>% expand(nesting(ratio, beta1), sigma) %>%
  mutate(beta0 = (beta1 * ratio)*-1) %>%
  arrange(sigma, ratio, beta1) %>%
  mutate(set = 1:n()) %>%
  dplyr::select(set, sigma, ratio, beta1, beta0)

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

raster_smoothed <- data.frame(sigma = sigma) %>%
  pmap(., CreateSmooth)
brick_smoothed <- brick(raster_smoothed)
names(brick_smoothed) <- paste0(covar_name, sigma)

CreateUAData <- function(sigma = sigma,
                         beta0 = beta0,
                         beta1 = beta1){
  raster_name <- paste0(covar_name, sigma)
  covar_ras_smooth <- subset(brick_smoothed, subset = raster_name)
  prob_ras <- raster(covar_ras) # creates blank raster
  prob_vec <- beta0 + beta1 * values(covar_ras_smooth)
  values(prob_ras) <- plogis(prob_vec)
  ua_ras <- raster(covar_ras) # creates blank raster
  values(ua_ras) <- rbinom(length(values(prob_ras)), 1, values(prob_ras))
  ua_full <- data.frame(coordinates(ua_ras), case = values(ua_ras))
  ua_data <- bind_rows(ua_full %>% filter(case == 1) %>% sample_n(200),
    ua_full %>% filter(case == 0) %>% sample_n(200))
  cell_nums <- cellFromXY(ua_ras, ua_data[,c("x", "y")])
  ua_data[, "cell_num"] <- cell_nums
  covar_mat <- as.matrix(covar_ras)
  print(paste0("Starting: ", raster_name, " (beta0 = ", beta0,
    ", beta1 = ", beta1, ")"))
  ua_data[, "value"] <- covar_ras[cell_nums]
  return(ua_data)
}

df_all_ua <- df_all %>%
  mutate(ua_data = pmap(.l = list(sigma, beta0, beta1),
                        .f = CreateUAData))

FitLogistic <- function(df){
  covar_model <- glm(case ~ value, family=binomial(link="logit"), data = df)
  return(covar_model)
}

df_all_ua_fit <- df_all_ua %>%
  mutate(model_ua = map(ua_data, FitLogistic)) %>%
  mutate(est_beta0 = map_dbl(model_ua, function(mod) coef(mod)[1])) %>%
  mutate(est_beta1 = map_dbl(model_ua, function(mod) coef(mod)[2])) %>%
  mutate(aic = map_dbl(model_ua, function(mod) AIC(mod))) %>%
  mutate(bias_beta = beta1 - est_beta1) %>%
  mutate(bias_beta0 = beta0 - est_beta0)

FitLogisticSigma <- function(est_beta0, est_beta1, ua_data, set){
  start_sigma <- 5
  print(paste0("Starting: set = ", set))
  print(paste0("Starting: est_beta0 = ", signif(est_beta0, 2),
    ", est_beta1 = ", signif(est_beta1, 2),
    ", start_sigma = ", signif(start_sigma, 2)))
  parms = c(est_beta0, est_beta1, start_sigma)
  cases <- ua_data[, "case"]
  cell_nums <- ua_data[, "cell_num"]
  optimx_results <- optimx(par = parms, fn = NLLIntBetaSig,
     lower=c(-1000, -100, sigma_range[1]), upper=c(1000, 100, sigma_range[2]),
    method =c("Nelder-Mead", "L-BFGS-B"), cases = cases,
    cell_nums = cell_nums, temp_ras = temp_ras, covar_mat = covar_mat,
    control = list(trace=3))
  return(optimx_results)
}

NLLIntBetaSig <- function(parms,
                          cases,
                          cell_nums,
                          temp_ras = temp_ras,
                          covar_mat = covar_mat){
  beta0 <- parms[1]   # intercept
  beta1 <- parms[2]   # slope 1
  sigma <- parms[3]   # sigma parameter
  if (sigma == 0) {
    values(temp_ras) <- covar_mat
  } else {
    values(temp_ras) <- gauss2dsmooth(covar_mat, lambda=sigma,
      nx=nrow(temp_ras), ny=ncol(temp_ras))
  }
  probs <- plogis(beta0 + beta1*(temp_ras[cell_nums])) # Binomial success probs
  lik <- dbinom(cases, 1, probs, log=TRUE) # Log of binomial pmf
  nll <- -1*sum(lik)
  return(nll)
}

covar_mat <- as.matrix(covar_ras) # needed for internals in optimx_fun
temp_ras <- raster(covar_ras) # needed for internals in optimx_fun

df_all_optimx_fit <- df_all_ua_fit %>%
  mutate(model_optimx = pmap(.l = list(est_beta0, est_beta1, ua_data, set),
    .f = FitLogisticSigma)) %>%
  mutate(opt_sigma = map_dbl(model_optimx, function(mod) mod[1,3])) %>%
  mutate(opt_beta0 = map_dbl(model_optimx, function(mod) mod[1,1])) %>%
  mutate(opt_beta1 = map_dbl(model_optimx, function(mod) mod[1,2]))

saveRDS(df_all_optimx_fit, file = paste0("Results/Bandwidth_Optimization/",
  "df_all_opmtix_fit", lubridate::today(), ".rds"))

# Fit univariate scales --------------------------------------------------------

bandwidth_fixed <- c(seq(0, 270, by=30), seq(300, 1350, by=150),
  seq(1500, 4500, by=300))  # radius (meters)
#bandwidths_fixed <- seq(bw_range[1], bw_range[2], by = xres(covar_ras)) # radius (m)

sigma_fixed = bandwidths_fixed/xres(covar_ras)

raster_smoothed_fixed <- data.frame(sigma = sigma_fixed) %>%
  pmap(., CreateSmooth)
brick_smoothed_fixed <- brick(raster_smoothed_fixed)
names(brick_smoothed_fixed) <- paste0(covar_name, sigma_fixed)

df_all_optimx_fixed_fit <- df_all_optimx_fit %>% #slice(1:10) %>%
  #dplyr::select(sigma, ua_data) %>%
  mutate(fixed_fit = map(ua_data, FitFixedData)) %>%
  mutate(fixed_min = map(fixed_fit, ExtractFixedMinAICSigma)) %>%
  mutate(fixed_aic_min = map_dbl(fixed_min, function(x) flatten(x)$fixed_aic_min)) %>%
  mutate(fixed_sigma_min = map_dbl(fixed_min, function(x) flatten(x)$fixed_sigma_min))

df_fixed_sum <- df_all_optimx_fixed_fit %>% select(sigma, beta0, beta1,
  fixed_sigma_min)

FitFixedData <- function(ua_data){
  ua_data <- ua_data
  df_fixed <- tibble(covar_sigma = c(paste0(covar_name, sigma_fixed)),
    ua_data_in = list(ua_data))
  df_fixed_results <- df_fixed %>%
     bind_cols(pmap_df(., FitFixedLogistic)) %>%
     mutate(sigma = sigma_fixed) %>%
     dplyr::select(covar_sigma, sigma, beta0, beta1, aic)
  return(df_fixed_results)
}

FitFixedLogistic <- function(covar_sigma, ua_data_in){
  fixed_smooth <- subset(brick_smoothed_fixed, subset = covar_sigma)
  df <- data.frame(case = ua_data_in$case, value = fixed_smooth[ua_data_in$cell_num])
  model_fixed <- glm(case ~ value, family=binomial(link="logit"), data = df)
  fixed_results <- tibble(
    beta0 = coef(model_fixed)[1],
    beta1 = coef(model_fixed)[2],
    aic = AIC(model_fixed))
  return(fixed_results)
}

ExtractFixedMinAICSigma <- function(fixed_fit){
   #fixed_fit <- fixed_fit
   df_top1 <- fixed_fit %>%
     arrange(aic, sigma) %>%
     slice(1)
   fixed_sigma_aic <- list(fixed_aic_min = df_top1$aic[1],
     fixed_sigma_min = df_top1$sigma[1])
  return(fixed_sigma_aic)
}

## -------------------------------------------------------------------------- ##
############################### PLOTTING #######################################
## -------------------------------------------------------------------------- ##

df_all_optimx_fit <- readRDS(paste0("Results/Bandwidth_Optimization/",
  "df_all_opmtix_fit2018-08-17.rds"))

# Plot Intital Estimate Results ------------------------------------------------

df_sum <- df_all_optimx_fit %>% dplyr::select(-c(ua_data, model_ua, model_optimx))

ggplot(df_sum, aes(x = beta1, y = bias_beta)) +
  geom_boxplot(aes(group = cut_width(beta1, 5), color = as.factor(sigma))) +
  facet_grid( ~ sigma, scale = "free_y", labeller = label_both) +
  ylim(-100, 100)

unique(df_sum$beta1)
table(df_sum$sigma, df_sum$beta1)

# Plotting ALL Logisitic Functions ---------------------------------------------

LogisticProb <- function(beta0, beta1) {
  predictors <- seq(-.25, 1.25, by = .05)
  predictors_logit <- beta0 + beta1*(predictors)
  probs <- plogis(predictors_logit)
  df <- data.frame(predictors, predictors_logit, probs)
  ss <- split.data.frame(df, df$predictors)
  return(lapply(ss, "[[", "probs"))
}

df_log_prob <- bind_cols(df_beta01 %>% dplyr::select(ratio),
  df_beta01 %>% dplyr::select(beta0, beta1) %>%
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
#  geom_text(data=df_log_prob, aes(x=.85, y=.85, label=as.character(beta0)),
#  size = 3, colour="blue", inherit.aes=FALSE, parse=FALSE)

# Plotting Fitted Estimates of Logisitic Functions -----------------------------

# Select Sigma for plotting Logistic Functions
sigma_select <- 10 # THIS SHOULDN'T MATTER, RIGHT?

df_est <- bind_cols(
  df_sum %>%
    filter(sigma == sigma_select) %>%
    dplyr::select(ratio, beta0, beta1),
  df_sum %>%
    filter(sigma == sigma_select) %>%
    mutate(beta0 = est_beta0, beta1 = est_beta1) %>%
    dplyr::select(c(beta0, beta1)) %>%
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
    aes(x=.85, y=.85, label=as.character(signif(est_beta1, 2))),
    size = 3, colour="darkgreen", inherit.aes=FALSE, parse=FALSE)


# Plotting Fitted Fixed and Optim Sigma AIC values -----------------------------

df_all_optimx_fixed_fit <- readRDS(paste0("Results/Bandwidth_Optimization/",
  "df_all_opmtix_fixed_fit_2018-08-18.rds"))

df_all_optimx_fixed_fit %>%
  group_by(sigma) %>%
  walk(., PlotSigmaFixedOptimSplit)

df_sigma <-
  df_all_optimx_fixed_fit %>% filter(sigma %in% c(10,20)) %>%
  select(-c(ua_data, model_ua, model_optimx, fixed_min)) %>%
  unnest()
df_sigma[5,]

# All fixed-scale sigma values
ggplot(data = df_sigma) +
  facet_wrap(~ sigma, scales = "free", ncol = 1, labeller = label_both) +
  geom_line(aes(x = sigma1, y = aic1, group = as.factor(beta1), color = as.factor(beta1))) +
  geom_point(aes(x = sigma1, y = aic1, group = as.factor(beta1), color = as.factor(beta1))) +
  geom_vline(data = df_sigma, aes(xintercept = fixed_sigma_min, color = as.factor(beta1))) +
  geom_vline(data = df_sigma, aes(xintercept = opt_sigma, color = as.factor(beta1)), linetype = 'dashed') + #, color = as.factor(beta1), linetype='dashed') +
  annotate("text", data = df_sigma,
      x = opt_sigma + (diff(range(as.numeric(df_sigma$sigma1)))*.025),
      y = max(df_sigma$opt_sigma), label = as.character(opt_sigma), color = "blue")

PlotSigmaFixedOptimSplit(sigma, ratio, beta0, beta1, fixed_data,
  fixed_sigma_min, fixed_aic_min)

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
