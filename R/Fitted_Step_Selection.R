# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Setup -------------------------------------------------------------------

rm(list = ls())
set.seed(2016)
library(Rcpp)
library(raster)
library(secr)
library(survival)
library(cowplot)
library(ggplot2)

# Source external functions
source("R/Fitted_Step_Selection/Fitted_Step_Selection_Helpers.R")
sourceCpp("R/Fitted_Step_Selection/Fitted_Step_Selection_Helpers.cpp")

n <- 4 * 60 * 24 * 30 * 12
rarify_by <- 4 * 60 * 6 # every 6 h
burnin <- 2e4 # burnin period
n <- n + burnin # total number of time steps to run
nc <- 200 # dimension of the landscape

alpha <- pm2alpha(0.3) # probability to move

## Generate landscapes
tpl <- raster(xmn = 0, xmx = nc, ymn = 0, ymx = nc, res = 1)
hrc <- raster(dist2hrc(nc), template = tpl)

xx <- genHabitat(nc, A = 0.5, p = 0.5)
xx <- raster(xx$hab, template = tpl)
res <-   stack(list(hrc = hrc, hab = xx))

## Selection coefficients
omegas <- list(
  c(hrc = -0.05, hab = 0),
  c(hrc = -0.05, hab = 2)
)

# Simulate movement, 2 scenarios ------------------------------------------

p1 <- simulate_walkf(alpha, omegas = omegas[[1]], n = n, resources = res,
  rarify_by = rarify_by, burnin = burnin, xy0 = sample(0:199, 2, TRUE),
  with_attr = TRUE, random_error = 0.1)

p2 <- simulate_walkf(alpha, omegas = omegas[[2]], n = n, resources = res,
  rarify_by = rarify_by, burnin = burnin, xy0 = sample(0:199, 2, TRUE),
  with_attr = TRUE, random_error = 0.1)

# Estimation: p1
cc <- ssf_case_control(p1, n_control = 10, res = res, method = "mle")
cc$cc$hab <- factor(cc$cc$hab)
mod1 <- survival::clogit(case ~ hrc + hab + strata(step_id), data = cc$cc)

## ssud ssf
p1_ud1 <- exp(coef(mod1)[1] * res[[1]] + coef(mod1)[2] * res[[2]])
p1_ud1 <- p1_ud1 / sum(p1_ud1[])

## simulate ud
hab_kern <- mk_hab_kern(res, coef(mod1)[1:2])
mov_kern <- mk_move_kern(ceiling(qgamma(0.99, scale = cc$gamma_dist$scale,
  shape = cc$gamma_dist$shape)), scale = cc$gamma_dist$scale,
  shape = cc$gamma_dist$shape)

p1_ud2 <- cells2UD(simulate_ud(n = 1e6, mov_kern, hab_kern, res,
  start = p1$xy[1, ])$xy, res, add_mp = FALSE)

# Estimation: p2
cc <- ssf_case_control(p2, n_control = 10, res = res, method = "mle")
cc$cc$hab <- factor(cc$cc$hab)
mod1 <- survival::clogit(case ~ hrc + hab + strata(step_id), data = cc$cc)

## ssud ssf
p2_ud1 <- exp(coef(mod1)[1] * res[[1]] + coef(mod1)[2] * res[[2]])
p2_ud1 <- p2_ud1 / sum(p2_ud1[])

## simulate ud
hab_kern <- mk_hab_kern(res, coef(mod1)[1:2])
mov_kern <- mk_move_kern(ceiling(qgamma(0.99, scale = cc$gamma_dist$scale,
  shape = cc$gamma_dist$shape)), scale = cc$gamma_dist$scale,
  shape = cc$gamma_dist$shape)

p2_ud2 <- cells2UD(simulate_ud(n = 1e6, mov_kern, hab_kern, res,
  start = p1$xy[1, ])$xy, res, add_mp = FALSE)
plot(p2_ud1)

# Generate plots ----------------------------------------------------------
r1 <- data.frame(rasterToPoints(res[[1]]))

library(ggplot2)
pl1 <- ggplot(r1, aes(x = x, y = y, fill = hrc)) + geom_raster() +
  coord_fixed()  +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
  axis.text = element_text(size = 8))

r1 <- data.frame(rasterToPoints(res[[2]]))
head(r1)

pl2 <- ggplot(r1, aes(x = x, y = y, fill = factor(hab))) + geom_raster() +
  coord_fixed()  +
  scale_fill_manual(values = c("white", "darkgreen")) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))

p1 <- data.frame(p1$xy)

pl3 <- ggplot(p1, aes(x = x, y = y)) +
  geom_path(size = 0.05) +
  geom_point(alpha = 0.05) +
  coord_fixed()  +
  scale_x_continuous(expand=c(0,0), limits = c(0, 200)) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 200)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))

p2 <- data.frame(p2$xy)

pl4 <- ggplot(p2, aes(x = x, y = y)) +
  geom_path(size = 0.05) +
  geom_point(alpha = 0.05) +
  coord_fixed()  +
  coord_fixed()  +
  scale_x_continuous(expand=c(0,0), limits = c(0, 200)) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 200)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))

## UDS
r1 <- data.frame(rasterToPoints(p1_ud1))

pl5 <- ggplot(r1, aes(x = x, y = y, fill = layer)) + geom_raster() +
  coord_fixed()  +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))

r1 <- data.frame(rasterToPoints(p1_ud2))

pl6 <- ggplot(r1, aes(x = x, y = y, fill = layer)) + geom_raster() +
  coord_fixed()  +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))

r1 <- data.frame(rasterToPoints(p2_ud1))

pl7 <- ggplot(r1, aes(x = x, y = y, fill = layer)) + geom_raster() +
  coord_fixed()  +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))

r1 <- data.frame(rasterToPoints(p2_ud2))
head(r1)

pl8 <- ggplot(r1, aes(x = x, y = y, fill = layer)) + geom_raster() +
  coord_fixed()  +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = "x", y = "y") +
  theme_bw() + theme(plot.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))


plot_grid(pl1, pl2)
plot_grid(pl3, pl4, pl5, pl7, pl6, pl8, labels = LETTERS[1:6], ncol = 2)
