# Load packages
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(microbenchmark))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(smoothie))
suppressPackageStartupMessages(library(spatialEco))
suppressPackageStartupMessages(library(spatialfil))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(viridis))
library(gisr)
library(baear)
cols <- viridis(100)
plot_dir <- file.path("C:/Users/blake/Documents/PhD Program",
  "McGarigal Lab Presentations/Lab Presentation - 2018.09/Figures")

# Parameters for Simple Grid and Gaussian Kernel
center_value <- 1
sigma <- 1
nrow <- 21
center <- ceiling(nrow/2)

# Create Simple Center-Value Matrix
s_matrix <- CreateCenterValueMatrix(nrow = nrow, center_value = center_value)
s_matrix
image(s_matrix, col = cols)
gg_s_matrix <- PlotMatrix(s_matrix, 2, "Simple Matrix")
gg_s_matrix
SaveGGPlot("Simple_Matrix", plot_dir)

# Create Gaussian Kernel matrix
g_kern <- gaussian.kernel(sigma = sigma, n = nrow)
image(g_kern, col = cols)
gg_g_kern <- PlotMatrix(g_kern, 2, paste0("Gaussian Kernel (sigma = ", sigma,
  " )"))
gg_g_kern
SaveGGPlot("Gaussian_Kernel_Matrix", plot_dir)

margin = theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
grid.arrange(grobs = lapply(list(gg_s_matrix, gg_g_kern), "+", margin), nrow =1)
SavePlot("Simple_Matrix_And_Gaussian_Kernel", plot_dir)

# Testing on User-Defined Matrix  ----------------------------------------------
# Matrix calculation (only 1 cell (the center) is "smoothed" by kernel)
s_calculated <- s_matrix*g_kern
image(s_calculated, col = cols)
gg_s_calculated <- PlotMatrix(s_calculated, 3,"Simple Matrix * Gaussian Kernel",
  FALSE, FALSE)
gg_s_calculated
SavePlot("Simple_Matrix_By_Gaussian_Kernel", plot_dir)

identical(s_matrix[center, center] * g_kern[center, center],
  s_calculated[center, center]) # Testing matrix calculations match

# Smoothie test (orginal method)
smoothie_test <- function(s_matrix, sigma){
  s_smoothed <- gauss2dsmooth(s_matrix, sigma, nx = nrow(s_matrix),
    ny = ncol(s_matrix))
}
s_smooth <- smoothie_test(s_matrix, 1)
PlotMatrix(s_smooth, 3, "Smoothie Matrix (odd nx,ny)")
max(s_smooth)
SavePlot("Smoothie_Matrix_Odd", plot_dir)

# Smoothie test (even row method)
smoothie_test_row <- function(s_matrix, sigma){
  s_smoothed <- gauss2dsmooth(s_matrix, sigma, nx=RoundTo(nrow(s_matrix), 2),
    ny=ncol(s_matrix))
}
s_smooth_row <- smoothie_test_row(s_matrix, 1)
PlotMatrix(s_smooth_row, 3, "Smoothie Matrix (even nx, odd ny)")
SavePlot("Smoothie_Matrix_Row", plot_dir)

# Smoothie test (even col method)
smoothie_test_col <- function(s_matrix, sigma){
  s_smoothed <- gauss2dsmooth(s_matrix, sigma, nx=nrow(s_matrix),
    ny=RoundTo(ncol(s_matrix), 2))
}
s_smooth_col <- smoothie_test_col(s_matrix, 1)
PlotMatrix(s_smooth_col, 3, "Smoothie Matrix (odd nx, even ny)")
SavePlot("Smoothie_Matrix_Col", plot_dir)

# Smoothie test (even row,col method)
smoothie_test_even <- function(s_matrix, sigma){
  s_smoothed <- gauss2dsmooth(s_matrix, sigma, nx=RoundTo(nrow(s_matrix), 2),
    ny=RoundTo(ncol(s_matrix), 2))
}
s_smooth <- smoothie_test_even(s_matrix, 1)
PlotMatrix(s_smooth, 3, "Smoothie Matrix (even nx,ny)")
SavePlot("Smoothie_Matrix_Even", plot_dir)

# Smoothing 3 point locations
s_matrix3 <- s_matrix
s_matrix3[center + 1, center + 1] <- 0
s_matrix3[3,3] <- 5
s_matrix3[10,9] <- 5
gg_s_matrix3 <- PlotMatrix(s_matrix3, 2, "Simple Matrix")
gg_s_matrix3
SavePlot("Simple_Matrix_3", plot_dir)
s_smooth3 <- smoothie_test2(s_matrix, 1)
gg_s_smooth3 <- PlotMatrix(s_smooth, 3, "Smoothie Matrix (even nx,ny)")
gg_s_smooth3
SavePlot("Smoothie_Matrix_3", plot_dir)
grid.arrange(grobs = lapply(list(gg_s_matrix3, gg_s_smooth3), "+", margin),
  nrow =1)
SavePlot("Simple_Smoothie_Matrix_3", plot_dir)

# Spatialfil test
spatialfil_test <- function(s_matrix, sigma){
  s_spatialfil <- applyFilter(x = s_matrix, kernel = convKernel(sigma = sigma,
    k = 'gaussian'))
}
s_spatialfil <- spatialfil_test(s_matrix, sigma)
image(s_spatialfil, col = cols)
PlotMatrix(s_spatialfil, 3, "Spatialfil Matrix")

identical(s_smooth, s_spatialfil)

micro_results <- microbenchmark(smoothie_test(s_matrix, sigma),
  spatialfil_test(s_matrix, sigma), times = 1000L)
print(micro_results)
autoplot(micro_results)

# Testing on Large Matrix  -----------------------------------------------------
lrg_matrix <- matrix(runif(10000), ncol=100, nrow=100)
lrg_smooth <- smoothie_test(lrg_matrix, sigma)
lrg_spatialfil <- spatialfil_test(lrg_matrix, sigma)
lrg_diff = lrg_smooth - lrg_spatialfil
breaks = seq(range(lrg_matrix)[1], range(lrg_matrix)[2], length.out = 1000)
col = plot3D::gg2.col(length(breaks)-1) #col = grey(1:1000/1000)
image(lrg_matrix, col = col, breaks = breaks, main = "Matrix (original)")
image(lrg_smooth, col = col, breaks = breaks, main = "Matrix 'smoothie'")
image(lrg_spatialfil, col = col, breaks = breaks, main = "Matrix 'spatialfil'")
image(lrg_diff, col = col, main = "Difference (smoothie/spatialfil)")
PlotMatrix(lrg_matrix, 2, title = "Matrix (original)", FALSE)
PlotMatrix(lrg_smooth, 2, title = "Matrix 'smoothie'", FALSE)
PlotMatrix(lrg_spatialfil, 2, title = "Matrix 'spatialfil'", FALSE)
PlotMatrix(lrg_diff, 2, title = "Difference (smoothie/spatialfil)", FALSE)

micro_results <- microbenchmark(smoothie_test(lrg_matrix, sigma),
  spatialfil_test(lrg_matrix, sigma), times = 1000L)
print(micro_results)
autoplot(micro_results)

# Plots for Powerpoint -----
lrg_matrix <- matrix(runif(250000), ncol=500, nrow=500)
lrg_smooth1 <- smoothie_test_even(lrg_matrix, 1)
lrg_smooth2 <- smoothie_test_even(lrg_matrix, 2)
lrg_smooth3 <- smoothie_test_even(lrg_matrix, 3)
lrg_smooth4 <- smoothie_test_even(lrg_matrix, 4)
lrg_smooth5 <- smoothie_test_even(lrg_matrix, 5)

gg_lrg_matrix <- PlotMatrix(lrg_matrix, 2, title = "Matrix (original)", FALSE,
  FALSE)
gg_lrg_matrix
gg_lrg_smooth1 <- PlotMatrix(lrg_smooth1, 2, title = "Smoothed (sigma = 1)",
  FALSE, FALSE)
gg_lrg_smooth1
gg_lrg_smooth2 <- PlotMatrix(lrg_smooth2, 2, title = "Smoothed (sigma = 2)",
  FALSE, FALSE)
gg_lrg_smooth2
gg_lrg_smooth3 <- PlotMatrix(lrg_smooth3, 2, title = "Smoothed (sigma = 3)",
  FALSE, FALSE)
gg_lrg_smooth3

margin_small = theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))
grid.arrange(grobs = lapply(list(gg_lrg_matrix, gg_lrg_smooth1,
  gg_lrg_smooth2, gg_lrg_smooth3), "+", margin_small),
  nrow = 2)
SavePlot("Lrg_Matrix_Smoothed_Sigma1-3", plot_dir)


################## Visualizing Kernels and Slices ##############################

# Parameters for Simple Matrix
center_value <- 5
sigma <- 1
nrow <- ncol <- 11
cols <- colorRampPalette(rev(brewer.pal(9,"Spectral")))(100)
cols <- viridis(100)

# Create Simple Matrix
s_matrix <- CreateCenterValueMatrix(nrow, center_value)
image(s_matrix, col = cols)
PlotMatrix(s_matrix, 2, "Simple Matrix")
PlotMatrixCenterRow2D(s_matrix)

# Convert 's_matrix' to RasterLayer 'manually'
s_raster <- raster(s_matrix, xmn=0, xmx=nrow, ymn=0, ymx=nrow)
plot(s_raster, col = cols)
Plot3DRaster(s_raster, col = cols, main = "Simple Raster")

# Parameters for Gaussian Kernel RasterLayer
sigma <- 2
nrow <- 21
shift_n <- 0

# Gaussian Kernel 1m cells
gkern_raster_1m <- CreateGaussKernRaster(sigma = sigma, nrow = nrow,
  shift_n = shift_n, cell_size = 1)
plot(gkern_raster_1m, col = cols, main = "Gaussian Kernel (1x1 cells)")
PlotMatrix(as.matrix(gkern_raster_1m), 3, "Gaussian Kernel (1m cells)")
Plot3DRaster(gkern_raster_1m, col = cols, x_lab = "X", y_lab = "Y",
  main = "Gaussian Kernel (1m cells)")

# Gaussian Kernel 30m cells
gkern_raster_30m <- CreateGaussKernRaster(sigma = sigma, nrow = nrow,
  shift_n = shift_n, cell_size = 30)
plot(gkern_raster_30m, col = cols, main = "Gaussian Kernel (30m cells)")
PlotMatrix(as.matrix(gkern_raster_30m), 3, "Gaussian Kernel (30m cells)")
Plot3DRaster(gkern_raster_30m, col = cols, x_lab = "X", y_lab = "Y",
  main = "Gaussian Kernel (30m cells)")

# Gaussian Kernel 30m cells - Center Row Values Only
gkern_raster_30m_center <- KeepRasterCenterRow(gkern_raster_30m)
plot(gkern_raster_30m_center, col = cols, main = paste0("Gaussian Kernel Slice",
  " (30m cells)"))
PlotMatrix(t(as.matrix(gkern_raster_30m_center)), 3,
  "Gaussian Kernel Center Cells (30m cells)")
Plot3DRaster(gkern_raster_30m_center, col = cols, x_lab = "X", y_lab = "Y",
  main = "Gaussian Kernel Slice (30m cells)")
PlotRasterCenterRow2D(gkern_raster_30m_center)

# Plot of Gaussian Kernel Slices
PlotRasterKernSlice(sigma = 1, nrow = 21, shift_n = 0, cell_size = 30)
PlotRasterKernSlice(sigma = 2, nrow = 21, shift_n = 0, cell_size = 30)
PlotRasterKernSlice(sigma = 3, nrow = 21, shift_n = 0, cell_size = 30)

# Plot of "Density" (probabilities are standardized for comparing to dnorm)
PlotRasterKernSlice(sigma = 1, nrow = 21, shift_n = 0, cell_size = 30,
  normalize = TRUE, dnorm_line = T)
PlotRasterKernSlice(sigma = 2, nrow = 21, shift_n = 0, cell_size = 30,
  normalize = TRUE, dnorm_line = T)
PlotRasterKernSlice(sigma = 3, nrow = 21, shift_n = 0, cell_size = 30,
  normalize = TRUE, dnorm_line = T)

## Smoothie --------------------------------------------------------------------
nrow = 21
center_value = 1
s_matrix <- CreateCenterValueMatrix(nrow = nrow, center_value = center_value)
# image(s_matrix, col = cols)
gg_s_matrix <- PlotMatrix(s_matrix, 2, "Simple Matrix", label = TRUE)

s_smoothed1 <- gauss2dsmooth(s_matrix, lambda = .25,
  nx = RoundTo(nrow(s_matrix), 2), ny = RoundTo(ncol(s_matrix), 2))
gg_s_smoothed1 <- PlotMatrix(s_smoothed1, 2, title = "Smoothie (sigma = 1)",
  label = TRUE)

s_smoothed2 <- gauss2dsmooth(s_matrix, lambda = 2,
  nx = RoundTo(nrow(s_matrix), 2), ny = RoundTo(ncol(s_matrix), 2))
gg_s_smoothed2 <- PlotMatrix(s_smoothed2, 2, title = "Smoothie (sigma = 2)",
  label = FALSE)

s_smoothed3 <- gauss2dsmooth(s_matrix, lambda = 3,
  nx = RoundTo(nrow(s_matrix), 2), ny = RoundTo(ncol(s_matrix), 2))
gg_s_smoothed3 <- PlotMatrix(s_smoothed3, 2, title = "Smoothie (sigma = 3)",
  label = FALSE)

margin_small = theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"))
grid.arrange(grobs = lapply(list(gg_s_matrix, gg_s_smoothed1, gg_s_smoothed2,
  gg_s_smoothed3), "+", margin_small), nrow = 2, ncol = 2)
SavePlot("Matrix_SmoothieSigma1_3", plot_dir)


# Convert to Rasters
cell_size <- 30
temp_ras <- raster(nrows = nrow(s_matrix), ncols = ncol(s_matrix), xmn=0,
    xmx = nrow(s_matrix)*cell_size, ymn=0, ymx = ncol(s_matrix)*cell_size,
    resolution = cell_size)
ras_s_matrix <- raster(temp_ras) # creates empty raster
values(ras_s_matrix) <- as.matrix(s_matrix)

ras_smoothed1 <- raster(temp_ras)
values(ras_smoothed1) <- gauss2dsmooth(s_matrix, lambda=1,
  nx=RoundTo(nrow(temp_ras),2), ny=RoundTo(ncol(temp_ras), 2))

ras_smoothed2 <- raster(temp_ras)
values(ras_smoothed2) <- gauss2dsmooth(s_matrix, lambda=2,
  nx=RoundTo(nrow(temp_ras),2), ny=RoundTo(ncol(temp_ras), 2))

ras_smoothed3 <- raster(temp_ras)
values(ras_smoothed3) <- gauss2dsmooth(s_matrix, lambda=3,
  nx=RoundTo(nrow(temp_ras),2), ny=RoundTo(ncol(temp_ras), 2))

gg_ras_s_matrix <- PlotRasterAsMatrix(ras_s_matrix, title = "Simple Matrix")
gg_ras_smoothed1 <- PlotRasterAsMatrix(ras_smoothed1,
  title = "Smoothed (sigma = 1)")
gg_ras_smoothed2 <- PlotRasterAsMatrix(ras_smoothed2,
  title = "Smoothed (sigma = 2)")
gg_ras_smoothed3 <- PlotRasterAsMatrix(ras_smoothed3,
  title = "Smoothed (sigma = 3)")

grid.arrange(grobs = lapply(list(gg_ras_s_matrix, gg_ras_smoothed1,
  gg_ras_smoothed2, gg_ras_smoothed3), "+", margin_small), nrow = 2, ncol = 2)
SavePlot("Raster_SmoothieSigma1_3", plot_dir)

# Raster Center Slice only
ras_s_matrix_center <- KeepRasterCenterRow(ras_s_matrix)
ras_smoothed1_center <- KeepRasterCenterRow(ras_smoothed1)
ras_smoothed2_center <- KeepRasterCenterRow(ras_smoothed2)
ras_smoothed3_center <- KeepRasterCenterRow(ras_smoothed3)

gg_ras_s_matrix_center <- PlotRasterAsMatrix(ras_s_matrix_center,
  title = "Simple Matrix")
gg_ras_smoothed1_center <- PlotRasterAsMatrix(ras_smoothed1_center,
  title = "Smoothed (sigma = 1)")
gg_ras_smoothed2_center <- PlotRasterAsMatrix(ras_smoothed2_center,
  title = "Smoothed (sigma = 2)")
gg_ras_smoothed3_center <- PlotRasterAsMatrix(ras_smoothed3_center,
  title = "Smoothed (sigma = 3)")

grid.arrange(grobs = lapply(list(gg_ras_s_matrix_center,
  gg_ras_smoothed1_center, gg_ras_smoothed2_center, gg_ras_smoothed3_center),
  "+", margin_small), nrow = 2, ncol = 2)
SavePlot("Raster_SmoothieCenterSigma1_3", plot_dir)

# Raster Center Slice 3D

#gg_ras_s_matrix_center3D <-
par(mfrow = c(2,2))
Plot3DRaster(ras_s_matrix_center, col=cols, main = "Simple Matrix", rgl=F)
Plot3DRaster(ras_smoothed1_center, col=cols, main ="Smoothed (sigma = 1)",rgl=F)
Plot3DRaster(ras_smoothed2_center, col=cols, main ="Smoothed (sigma = 2)",rgl=F)
Plot3DRaster(ras_smoothed3_center, col=cols, main ="Smoothed (sigma = 3)",rgl=F)
par(mfrow = c(1,1))
SavePlot("Raster_SmoothieCenter3DSigma1_3", plot_dir)

gg1 <- PlotRasterKernSlice(ras_s_matrix_center, title = "Simple Matrix")
gg2 <- PlotRasterKernSlice(ras_smoothed1, title = "Smoothed Raster (sigma = 1)")
gg3 <- PlotRasterKernSlice(ras_smoothed2, title = "Smoothed Raster (sigma = 2)")
gg4 <- PlotRasterKernSlice(ras_smoothed3, title = "Smoothed Raster (sigma = 3)")
margin_small = theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"))
grid.arrange(grobs = lapply(list(gg1, gg2, gg3, gg4), "+", margin_small), nrow = 2,
  ncol = 2)
SavePlot("Raster_SmoothieCenterSigma1_3", plot_dir)

gg1 <- PlotRasterKernSlice(ras_s_matrix, sigma = 1, cell_size = 30,
  title = "Simple Matrix", normalize = FALSE, dnorm_line = TRUE)
gg2 <- PlotRasterKernSlice(ras_smoothed1, sigma = 1,
  title = "Smoothed Raster (sigma = 1)", normalize = TRUE, dnorm_line = TRUE)
gg3 <- PlotRasterKernSlice(ras_smoothed2, sigma = 2,
  title = "Smoothed Raster (sigma = 2)", normalize = TRUE, dnorm_line = TRUE)
gg4 <- PlotRasterKernSlice(ras_smoothed3, sigma = 3,
  title = "Smoothed Raster (sigma = 3)", normalize = TRUE, dnorm_line = TRUE)
margin_small = theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"))
grid.arrange(grobs = lapply(list(gg1, gg2, gg3, gg4), "+", margin_small), nrow = 2,
  ncol = 2)
SavePlot("Raster_SmoothieCenterNormalSigma1_3", plot_dir)


# Matrix center slice only
s_matrix_center <- KeepMatrixCenterRow(s_matrix)
s_smoothed1_center <- KeepMatrixCenterRow(s_smoothed1)
s_smoothed2_center <- KeepMatrixCenterRow(s_smoothed2)
s_smoothed3_center <- KeepMatrixCenterRow(s_smoothed3)

gg_s_matrix_center <- PlotMatrix(mat = s_matrix_center, digits = 2,
  title = "Simple Matrix", T, T, 3)
gg_s_smoothed1_center <- PlotMatrix(s_smoothed1_center, 2,
  title = "Smoothie (sigma = 1)", T, T, 3)
gg_s_smoothed2_center <- PlotMatrix(s_smoothed2_center, 2,
  title = "Smoothie (sigma = 2)", T, T, 3)
gg_s_smoothed3_center <- PlotMatrix(s_smoothed3_center, 2,
  title = "Smoothie (sigma = 3)", T, T, 3)

margin_small = theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"))
grid.arrange(grobs = lapply(list(gg_s_matrix_center, gg_s_smoothed1_center,
  gg_s_smoothed2_center, gg_s_smoothed3_center), "+", margin_small), nrow = 2,
  ncol = 2)
SavePlot("Matrix_SmoothieCenterSigma1_3", plot_dir)

PlotGaussKernSlice

# Convert 's_matrix' to RasterLayer 'manually'
s_raster <- raster(s_matrix_center, xmn=0, xmx=nrow, ymn=0, ymx=nrow)
plot(s_raster, col = cols)
Plot3DRaster(s_raster, col = cols, main = "Simple Raster")
r_smoothed_center <- raster(t(s_smoothed_center), xmn=0, xmx=nrow, ymn=0,
  ymx=nrow)
Plot3DRaster(r_smoothed_center, col = cols, main = "Simple Raster")

# Center slice - 2D
gg_s_matrix_center2D <- PlotMatrixCenterRow2D(mat = s_matrix_center,
  title = "Simple Matrix")
gg_s_smoothed1_center2D <- PlotMatrixCenterRow2D(mat = s_smoothed1_center,
  title = "Smoothie (sigma = 1)")
gg_s_smoothed2_center2D <- PlotMatrixCenterRow2D(mat = s_smoothed2_center,
  title = "Smoothie (sigma = 2)")
gg_s_smoothed3_center2D <- PlotMatrixCenterRow2D(mat = s_smoothed3_center,
  title = "Smoothie (sigma = 3)")

margin_small = theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"))
grid.arrange(grobs = lapply(list(gg_s_matrix_center2D, gg_s_smoothed1_center2D,
  gg_s_smoothed2_center2D, gg_s_smoothed3_center2D), "+", margin_small),
  nrow = 2, ncol = 2)
SavePlot("Matrix_SmoothieCenter2DSigma1_3", plot_dir)


s_smoothed_raster <- raster(s_smoothed1, xmn=0, xmx=ncol(s_smoothed1),
  ymn=0, ymx=nrow(s_smoothed1))
plot(s_smoothed_raster)

# Plot of slice cells "Probability"
s_smoothed_center <- KeepMatrixCenterRow(s_smoothed)
image(s_smoothed_center, col = cols)
PlotMatrix(s_smoothed_center, title = "Smoothed Kernel Slice")
PlotMatrixCenterRow2D(s_smoothed_center, title = "Smoothed Kernel Slice")

# Plot of "Density" (probabilities are standardized for comparing to dnorm)
s_smoothed_center_s1 <- ExtractMatrixCenterRow(s_smoothed_center)


## Testing on Real Landscape Data ----------------------------------------------

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
buffer <- -6000
ext <- extent(357500+buffer, 377660-buffer, 4936600+buffer, 4956700-buffer)
developed <- crop(developed_full, ext)
elev <- crop(elev_full, ext)
forest <- crop(forest_full, ext)
open_water <- crop(open_water_full, ext)

# Plot layers
# plot(elev)
# plot(developed)
# plot(forest)
# plot(open_water)

# Select covariate and bandwith
covar_ras = developed
bandwidth = 1200

# Smooth layer
sigma <- bandwidth/xres(covar_ras)
covar_mat <- as.matrix(covar_ras)
tic("Smoothie")
covar_mat_smoothie <- gauss2dsmooth(covar_mat, lambda = sigma,
  nx = nrow(covar_ras), ny = ncol(covar_ras))
toc()
tic("Spatialfil")
covar_mat_spatialfil <- applyFilter(x = covar_mat, kernel =
    convKernel(sigma = sigma, k = 'gaussian'))
toc()
covar_ras_smoothie  <- covar_ras
covar_ras_spatialfil <- covar_ras
values(covar_ras_smoothie) <- covar_mat_smoothie
values(covar_ras_spatialfil) <- covar_mat_spatialfil
par(mfrow=c(1,3))
plot(covar_ras, main = "Original Raster")
plot(covar_ras_smoothie, main = paste0("Smoothie (sigma = ", sigma, ")"))
plot(covar_ras_spatialfil, main = paste0("Spatialfil (sigma = ", sigma, ")"))

par(mfrow=c(1,1))

# CONCLUSION: Spatialfil only works under certain conditions which seem to be
# dependent on matrix dimensions and binary vs. continous data. Results
# sometimes have "unsmoothed" bands in output (without producing Warnings!). I
# should avoid using applyFilter() without a lot of scrutiny.

# Plots for Powerpoint -----
covar_ras = developed
covar_mat <- as.matrix(covar_ras)

covar_mat_smoothie1 <- gauss2dsmooth(covar_mat, lambda = 1,
  nx = RoundTo(nrow(covar_ras), 2), ny = RoundTo(ncol(covar_ras), 2))
covar_ras_smoothie1  <- covar_ras
values(covar_ras_smoothie1) <- covar_mat_smoothie1

covar_mat_smoothie2 <- gauss2dsmooth(covar_mat, lambda = 2,
  nx = RoundTo(nrow(covar_ras), 2), ny = RoundTo(ncol(covar_ras), 2))
covar_ras_smoothie2  <- covar_ras
values(covar_ras_smoothie2) <- covar_mat_smoothie2

covar_mat_smoothie3 <- gauss2dsmooth(covar_mat, lambda = 3,
  nx = RoundTo(nrow(covar_ras), 2), ny = RoundTo(ncol(covar_ras), 2))
covar_ras_smoothie3  <- covar_ras
values(covar_ras_smoothie3) <- covar_mat_smoothie3

covar_mat_smoothie10 <- gauss2dsmooth(covar_mat, lambda = 10,
  nx = RoundTo(nrow(covar_ras), 2), ny = RoundTo(ncol(covar_ras), 2))
covar_ras_smoothie10  <- covar_ras
values(covar_ras_smoothie10) <- covar_mat_smoothie10

covar_mat_smoothie20 <- gauss2dsmooth(covar_mat, lambda = 20,
  nx = RoundTo(nrow(covar_ras), 2), ny = RoundTo(ncol(covar_ras), 2))
covar_ras_smoothie20  <- covar_ras
values(covar_ras_smoothie20) <- covar_mat_smoothie20

covar_mat_smoothie30 <- gauss2dsmooth(covar_mat, lambda = 30,
  nx = RoundTo(nrow(covar_ras), 2), ny = RoundTo(ncol(covar_ras), 2))
covar_ras_smoothie30  <- covar_ras
values(covar_ras_smoothie30) <- covar_mat_smoothie30

covar_mat_smoothie40 <- gauss2dsmooth(covar_mat, lambda = 40,
  nx = RoundTo(nrow(covar_ras), 2), ny = RoundTo(ncol(covar_ras), 2))
covar_ras_smoothie40  <- covar_ras
values(covar_ras_smoothie40) <- covar_mat_smoothie40

covar_ras_name <- str_replace(names(covar_ras), "_30mc", "")
covar_name <- str_to_title(str_replace(covar_ras_name, "_", " "))
covar_sigma <- paste0(covar_name, " (sigma = ")

gg0 <- PlotRasterAsMatrix(covar_ras, title = covar_name,
  all_coords = F)
gg1 <- PlotRasterAsMatrix(covar_ras_smoothie1, title = paste(covar_sigma, "1)"),
  all_coords = F)
gg2 <- PlotRasterAsMatrix(covar_ras_smoothie2, title = paste(covar_sigma, "2)"),
  all_coords = F)
gg3 <- PlotRasterAsMatrix(covar_ras_smoothie3, title = paste(covar_sigma, "3)"),
  all_coords = F)

margin_small = theme(plot.margin = unit(c(.1, .1, .1, .1), "cm"))
grid.arrange(grobs = lapply(list(gg0, gg1, gg2, gg3), "+", margin_small),
  nrow = 2)
SavePlot(paste0(covar_name, "_Sigma1-3"), plot_dir)

gg10 <- PlotRasterAsMatrix(covar_ras_smoothie10, title = paste(covar_sigma,
  "10)"), all_coords = F)
gg20 <- PlotRasterAsMatrix(covar_ras_smoothie20, title = paste(covar_sigma,
  "20)"), all_coords = F)
gg30 <- PlotRasterAsMatrix(covar_ras_smoothie30, title = paste(covar_sigma,
  "30)"), all_coords = F)
gg40 <- PlotRasterAsMatrix(covar_ras_smoothie40, title = paste(covar_sigma,
  "40)"), all_coords = F)

grid.arrange(grobs = lapply(list(gg10, gg20, gg30, gg40), "+", margin_small),
  nrow = 2)
SavePlot(paste0(covar_name, "_Sigma10-40"), plot_dir)


## -------------------------------------------------------------------------- ##
################################ OLD CODE ######################################
## -------------------------------------------------------------------------- ##


# gg0 <- ggplot(ConvertRasterForGGPlot(covar_ras, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Developed")
# gg1 <- ggplot(ConvertRasterForGGPlot(covar_ras_smoothie1, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Smoothed (sigma = 1)")
# gg2 <- ggplot(ConvertRasterForGGPlot(covar_ras_smoothie2, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Smoothed (sigma = 2)")
# gg3 <- ggplot(ConvertRasterForGGPlot(covar_ras_smoothie3, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Smoothed (sigma = 3)")
#
# gg10 <- ggplot(ConvertRasterForGGPlot(covar_ras_smoothie10, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Smoothed (sigma = 10)")
# gg20 <- ggplot(ConvertRasterForGGPlot(covar_ras_smoothie20, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Smoothed (sigma = 20)")
# gg30 <- ggplot(ConvertRasterForGGPlot(covar_ras_smoothie30, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Smoothed (sigma = 30)")
# gg40 <- ggplot(ConvertRasterForGGPlot(covar_ras_smoothie40, 200000), aes(x, y)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(guide = guide_colorbar(title = "Value",
#     title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
#   scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
#   coord_fixed(ratio = 1) + ggtitle("Smoothed (sigma = 40)")


# theme_cell <- theme(plot.margin = unit(c(1,1,1,1),"cm"),
#     plot.title = element_text(size = 18, face = "bold", hjust = .5, vjust = 0),
#     axis.text = element_text(size = 12, face = "bold"),
#     axis.title = element_text(size = 14, face = "bold"),
#     panel.grid.minor.x = element_line(colour = "grey80"),
#     panel.grid.major.x = element_line(colour =  NA),
#     panel.grid.minor.y = element_line(colour =  NA),
#     panel.grid.major.y = element_line(colour = "grey80", size=1,
#       linetype="dashed"),
#     panel.background = element_rect(fill = "white", colour = "white"))
# theme_matrix <- theme(
#     plot.title = element_text(size = 18, face = "bold", hjust = .5, vjust = 0),
#     axis.text = element_text(size = 12, face = "bold"),
#     axis.title = element_text(size = 14, face = "bold"),
#     panel.grid.minor = element_line(colour = NA),
#     panel.grid.major = element_line(colour =  NA),
#     panel.background = element_rect(fill = "white", colour = "white"))
#
# ggplot(t_kernel_center, aes(x, y)) +
#   geom_col(width=1, color="black") +
#   ggtitle(paste0("Gaussian Kernel Slice (center = ", center_x, ", sigma= ",
#     sigma, ")")) +
#   scale_x_continuous(breaks=seq(1,nrow,1), expand = c(0, 0, 0, 0)) +
#   scale_y_continuous(expand = c(0, 0, .1, 0)) +
#   cell_theme
#
# # Plot of "Density" (probabilities are standardized for comparing to dnorm)
# t_kernel_center2 <- data.frame(y = t_kernel[center_x,]/sum(t_kernel[center_x,]),
#   x=seq_len(ncol))
# ggplot(t_kernel_center2, aes(x, y)) +
#   geom_col(width=1, color="black") +
#   stat_function(fun = dnorm, args = list(mean = center_x, sd = sigma),
#     colour = "blue", size=1.25) + ylab("Density") +  xlab("Cell (X)") +
#   ggtitle(paste0("Gaussian Kernel Slice (center = ", center_x, ", sigma= ",
#     sigma, ")")) +
#   scale_x_continuous(breaks=seq(1,nrow,1), expand = c(0, 0, 0, 0)) +
#   scale_y_continuous(expand = c(0, 0, .1, 0))  +
#   cell_theme
# dnorm(center_x + 1, mean=center_x, sd=sigma)
#
# # Convert 't_kernel' to Raster
# t_kernel_raster <- raster(t_kernel, xmn=0, xmx=ncol(t_kernel), ymn=0,
#   ymx=nrow(t_kernel))
# plot(t_kernel_raster)
# Plot3DRaster(t_kernel_raster, main = paste0("Gaussian Kernel (center = ",
#   center_x, ", sigma= ",sigma, ")"))
#
# g_kernel_raster_shift <- shift(t_kernel_raster, x=shift_n, y=shift_n)
# plot(t_kernel_raster)
# plot(t_kernel_raster_shift)
# t_kernel_raster_shift
#
# t_kernel <- gaussian.kernel(sigma=sigma, n=nrow)
# t_kernel_raster <- raster(t_kernel, xmn=0, xmx=ncol(t_kernel), ymn=0,
#   ymx=nrow(t_kernel))

# ------------------------ Spatilfil ---
# plot(convKernel(sigma = 1, k = 'gaussian'))
# plot(convKernel(sigma = 2, k = 'gaussian'))
# plot(convKernel(sigma = 3, k = 'gaussian'))
# plot(convKernel(sigma = 4, k = 'gaussian'))
# dim(convKernel(sigma = 1, k = 'gaussian')[[1]])[1]
# dim(convKernel(sigma = 2, k = 'gaussian')[[1]])[1]
# dim(convKernel(sigma = 3, k = 'gaussian')[[1]])[1]
# dim(convKernel(sigma = 4, k = 'gaussian')[[1]])[1]
# c((7-1)/2, (15-1)/2, (21-1)/2, (29-1)/2) # +1 to sigma adds 3-4 to cell radius
# sigma = 2
# s_spatialfil <- applyFilter(x = s_matrix, kernel = convKernel(sigma = sigma,
#   k = 'gaussian'))
# PlotMatrixCenterRow2D(s_spatialfil, title = paste0("Spatialfil Slice (sigma = ",
#   sigma, ")"))
# PlotGaussKernSlice(sigma = 2, nrow = 10, shift_n = 0, cell_size = 30)
#
# # Plot of "Probability" for each cell
# ggplot(s_spatialfil, aes(x, y)) +
#   geom_col(width=1, color="black") +
#   ggtitle(paste0("Spatial Kernel Slice (center = ", center_x, ", sigma= ",
#     sigma, ")")) +
#   scale_x_continuous(breaks=seq(1,nrow,1), expand = c(0, 0, 0, 0)) +
#   scale_y_continuous(expand = c(0, 0, .1, 0))  +
#   cell_theme
# # Plot of "Density" (probabilities are standardized for comparing to dnorm)
# t_spatialfil_center2 <- data.frame(
#   y = t_spatialfil[center_x,]/sum(t_spatialfil[center_x,]), x=seq_len(ncol))
# ggplot(t_spatialfil_center2, aes(x, y)) +
#   geom_col(width=1, color="black") +
#   stat_function(fun = dnorm, args = list(mean = center_x, sd = sigma),
#     colour = "blue", size=1.25) + ylab("Density") +  xlab("Cell (X)") +
#   ggtitle(paste0("Spatialfil Kernel Slice (center = ", center_x, ", sigma= ",
#     sigma, ")")) +
#   scale_x_continuous(breaks=seq(1,nrow,1), expand = c(0, 0, 0, 0)) +
#   scale_y_continuous(expand = c(0, 0, .1, 0))  +
#   cell_theme
# t_spatialfil_raster <- raster(t_spatialfil, xmn=0, xmx=ncol(t_spatialfil),
#   ymn=0, ymx=nrow(t_spatialfil))
# plot(t_spatialfil_raster)


