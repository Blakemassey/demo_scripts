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

# Simulate movement -------------------------------------------------------



## all together
## FAST
simulate_walkf <- function(alpha, omegas, resources, n,
                           rarify_by = 100,
                           burnin = round(n / 10), xy0 = c(0, 0),
                           with_attr = FALSE, more = NULL, random_error = 0) {
  r <- as.array(resources)
  nc <- dim(r)[2]

  r <- r[nc:1, , ] # bring in the right order
  r <- sapply(1:dim(r)[3], function(i) as.vector(t(r[, , i]))) # flatten 2d arrays to 1d
  dists <- c(0, 1, 1, 1, 1)

  # tranisition porbabilities
  tm1 <- tpm_func(alpha, omegas, r, dists, nc)

  # simulate walk
  w <- walk_func1(tm1, n, xy0, nc)

  # rm burnin
  if (burnin > 0)
    w <- w[-(1:burnin)]

  # rarify
  w <- w[seq(1, length(w), by = rarify_by)]


  xy <- cbind(x = w %% nc, y = w %/% nc)

  if (random_error != 0) {
    xy[, 1] <- xy[, 1] + runif(nrow(xy), -random_error, random_error)
    xy[, 2] <- xy[, 2] + runif(nrow(xy), -random_error, random_error)

  }

  xy <- if (with_attr) {
    list(xy = xy,
         alpha = alpha,
         omegas = omegas,
         resources = resources,
         n = n,
         xy0 = xy0,
         rarify_by = rarify_by,
         burnin = burnin,
         more = more,
         with_attr = TRUE)
  } else {
    list(xy = xy,
         with_attr = FALSE)
  }

  class(xy) <-  c("walk", "list")
  xy
}

simulate_udf <- function(alpha, omegas, resources, n,
                         rarify_by = 100,
                         burnin = round(n / 10), xy0 = c(0, 0),
                         with_attr = FALSE, more = NULL, random_error = 0) {
  r <- as.array(resources)
  nc <- dim(r)[2]

  r <- r[nc:1, , ] # bring in the right order
  r <- sapply(1:dim(r)[3], function(i) as.vector(t(r[, , i]))) # flatten 2d arrays to 1d
  dists <- c(0, 1, 1, 1, 1)

  # tranisition porbabilities
  tm1 <- tpm_func(alpha, omegas, r, dists, nc)

  # simulate walk
  w <- ud_func1(tm1, n, xy0, nc)

  # rm burnin => to cpp
  # rarify => to cpp
  return(w)


  xy <- if (with_attr) {
    list(xy = xy,
         alpha = alpha,
         omegas = omegas,
         resources = resources,
         n = n,
         xy0 = xy0,
         rarify_by = rarify_by,
         burnin = burnin,
         more = more,
         with_attr = TRUE)
  } else {
    list(xy = xy,
         with_attr = FALSE)
  }

  class(xy) <-  c("walk", "list")
  xy
}

## FAST
tpm_only <- function(alpha, omegas, resources) {

  r <- as.array(resources)
  nc <- dim(r)[2]

  r <- r[nc:1, , ] # bring in the right order
  r <- sapply(1:2, function(i) as.vector(t(r[, , i]))) # flatten 2d arrays to 1d
  dists <- c(0, 1, 1, 1, 1)

  # tranisition porbabilities
  tpm_func(alpha, omegas, r, dists, nc)
}

simulate_walk_only <- function(tpm, n, nc,
                               rarify_by = 100,
                               burnin = round(n / 10), xy0 = c(0, 0)) {
  # simulate walk
  w <- walk_func1(tpm, n, xy0, nc)

  # rm burnin
  if (burnin > 0)
    w <- w[-(1:burnin)]

  # rarify
  w <- w[seq(1, length(w), by = rarify_by)]

  xy <- list(xy = cbind(x = w %% nc, y = w %/% nc))
  class(xy) <-  c("walk", "list")
  xy
}

## FAST with changing omegas (each time step has an unique omega)
simulate_walkf_time <- function(alpha, omegas, resources, n,
                           rarify_by = 100,
                           burnin = round(n / 10), xy0 = c(0, 0)) {

  if (n > nrow(omegas)) {
    stop("Not for every timestep an omega")
  }
  r <- as.array(resources)
  nc <- dim(r)[2]

  r <- r[nc:1, , ] # bring in the right order
  r <- sapply(1:2, function(i) as.vector(t(r[, , i]))) # flatten 2d arrays to 1d


  dists <- c(0, 1, 1, 1, 1)

  # simulate walk
  w <- walk_func_time(alpha, r, omegas, n, xy0, nc, dists)
 #walk_func_time(double alpha, NumericMatrix resources, NumericMatrix omegas, int n, NumericVector xy0, int nc, IntegerVector d) {

  # rm burnin
  if (burnin > 0)
    w <- w[-(1:burnin)]

  # rarify
  w <- w[seq(1, length(w), by = rarify_by)]

  xy <- list(xy = cbind(x = w %% nc, y = w %/% nc))
  class(xy) <-  c("walk", "list")
  xy
}


# Steady state UD ---------------------------------------------------------

simulate_ud_ss <- function(alpha, omegas, resources, n,
                           rarify_by = 100,
                           burnin = round(n / 10), nstart = 100) {
  r <- as.array(resources)
  nc <- dim(r)[2]

  r <- r[nc:1, , ] # bring in the right order
  r <- sapply(1:2, function(i) as.vector(t(r[, , i]))) # flatten 2d arrays to 1d
  dists <- c(0, 1, 1, 1, 1)

  # tranisition porbabilities
  tm1 <- tpm_func(alpha, omegas, r, dists, nc)

  # simulate walk
  starts <- expand.grid(1:nc - 1, 1:nc - 1)[sample.int(nc^2, nstart), ]

  w <- do.call(rbind, lapply(1:nrow(starts), function(i) {
    w <- walk_func1(tm1, n, unlist(starts[i, ]), nc)

    # rm burnin
    if (burnin > 0)
      w <- w[-(1:burnin)]
    # rarify
    w <- w[seq(1, length(w), by = rarify_by)]
    cbind(x = w %% nc, y = w %/% nc)
  }))

  xy <- list(xy = w)
  class(xy) <-  c("walk", "list")
  xy
}

diskern <- function(x, y, d, alpha, omegas, resources, nc) {
  # x and y are the coordinates
  # d are the dists to the HR center
  yx <- cbind(
    (y + c(0, 1, 0, -1, 0) - 1) %% nc + 1,
    (x + c(0, 0, 1, 0, -1) - 1) %% nc + 1
  ) # coordinates

  num <- exp(-alpha * d + rowSums(sweep(resources[map21d(xy, nc), ], 2, omegas, "*")))
  num / sum(num)
}

pickOne <- function(loc, r, nc) {
  if (FALSE) {
    loc <- xy[i-1, ]
    r <- d1
    c <- 300
  }
  w <- sample.int(5, 1, prob = r[loc[1], loc[2], ])
  cbind(
    (loc[1] + c(0, 0, 1, 0, -1) - 1) %% nc + 1,   # wrap world
    (loc[2] + c(0, 1, 0, -1, 0) - 1) %% nc + 1)[w, ]   # wrap world
}

rhrBA <- function(x, y) {
  r1 <- x[]
  r2 <- y[]
  r1 <- r1 / sum(r1)
  r2 <- r2 / sum(r2)
  ## bhattacharyya's afinity
  sum(sqrt(r1 * r2))
}



mu2alpha <- function(mu) {

  log(mu / (4 - 4 * mu))
}

pm2alpha <- function(p) {
  log((-4 * (p - 1))/p)
}


plot.walk <- function(x, ...) {
  plot(x$xy, asp = 1, xlab = "", ylab = "", las = 1,  col = adjustcolor("black", 0.2),
       pch = 20, ...)
}

nextPos <- function(pts, cc, i, fast = FALSE) {
  di <- sqrt((pts[, 1] - pts[i, 1])^2 + (pts[, 2] - pts[i, 2])^2) /100 # slightly faster
  ff <- exp(cc[1] * pts[, 3] + cc[2] * di + cc[3] * log(di))  # taking the log out is not really faster
  ff[is.infinite(ff)] <- 0  # should we allow a probability for staying
  ff <- ff / sum(ff)
  #ff <- ff / (2 * pi * di)
  if (fast) {
    sample_int_expj(n = length(di), size = 1, prob = ff)
  } else {
    sample.int(n = length(di), size = 1, prob = ff)
  }
}

genHabitat <- function(nc, p = 0.1, A = 0.1) {
  r <- make.mask(nx = nc, ny = nc, spacing = 1)
  h <- randomHabitat(r, p = p, A = A)
  r <- raster(xmn=0, xmx=nc, ymn=0, ymx=nc, ncols=nc, nrows=nc)
  r <- rasterize(data.frame(h), r, field=1, background=0)
  r <- as.matrix(r)
  r <- list(hab = r, p = p, A = A, nc = nc)
  class(r) <- c("hab", "matrix")
  r
}

image.hab <- function(x, ...) {
  image(x = 1:x$nc, y = 1:x$nc, z = x$hab, las = 1,
        xlab = "", ylab = "", col = c("white", "darkgreen"), asp = 1,
        main = paste0("p = ", x$p, ", A = ", x$A))
}


dist2hrc <- function(nc) {
  m <- matrix(NA, nrow = nc, ncol = nc)
  x0 <- y0 <- ncol(m) / 2
  d <- ((col(m) - x0)^2 + (row(m) - y0)^2) / 100  # devide by 100 to avoid numerical problems
  d
}

map21d <- function(ij, nc) {
  ij[, 2] + (ij[, 1] - 1) * nc
}

# SSF ---------------------------------------------------------------------

ssf_case_control <- function(x, n_controll = 10, resources, method = c("mme", "mle"), replace_zero = 0.1) {

  obs_lengths <- sp::LineLength(x$xy, sum = FALSE)

  if (any(obs_lengths == 0)) {
    warning(sprintf("Step lengths of lengths 0; 0 are replaced with %s", replace_zero))
    obs_lengths[obs_lengths == 0] <- replace_zero
  }

  if (method == "mle") {
    # http://stackoverflow.com/questions/17616702/difficulty-fitting-gamma-distribution-with-r
    m <- fitdistrplus::fitdist(obs_lengths, "gamma", lower = c(0, 0))  #
    shape <- unname(coef(m)["shape"])
    scale <- unname(1/coef(m)["rate"])
  } else if (method == "mme") {
    mean <- mean(obs_lengths)
    var <- var(obs_lengths)
    shape <- mean^2/var
    scale <- var/mean
  }

  ## Generate random points
  ns <- nrow(x$xy)  # number of steps
  ne <- ns - 1  # effective number of steps

  sl <- rgamma(ne * n_controll, shape = shape, scale = scale)  # step lengths for new steps
  ta <- runif(ne * n_controll, -2 * pi, 2 * pi)  # turning angles for new stps

  ## Controll points
  case_for_controll <- rep(1:ne, each = n_controll)
  xy_cc <- x$xy[case_for_controll, ]
  xy_cc[, 1] <- xy_cc[, 1] + sl * cos(ta)
  xy_cc[, 2] <- xy_cc[, 2] + sl * sin(ta)

  cc_df <- do.call(rbind, list(
    data.frame(
      step_id = 1:ne,
      case = TRUE,
      xstart = x$xy[1:ne, 1],
      ystart = x$xy[1:ne, 2],
      xend = x$xy[2:ns, 1],
      yend = x$xy[2:ns, 2],
      sl = obs_lengths
    ),
    data.frame(
      step_id = rep(1:ne, each = n_controll),
      case = FALSE,
      xstart = x$xy[case_for_controll, 1],
      ystart = x$xy[case_for_controll, 2],
      xend = xy_cc[, 1],
      yend = xy_cc[, 2],
      sl = sl
    )
  ))

  ## Extract covariates
  cc_df$sl[cc_df$sl == 0] <- 1/(2 * pi)
  cc_df$log_sl <- log(cc_df$sl)
  cc_df <- cbind(cc_df, raster::extract(resources, cc_df[, c("xend", "yend")]))
  list(cc = cc_df, gamma_dist = list(shape = shape, scale = scale))
}

correct_scale_shape <- function(tentative_gamma, ests, plot = FALSE) {
  scale_new <- 1 / ((1 / tentative_gamma$gamma_dist$scale) - ests[1])
  shape_new <- tentative_gamma$gamma_dist$shape + ests[2]

  if (plot) {
    curve(dgamma(x,
                 shape = tentative_gamma$gamma_dist$shape,
                 scale = tentative_gamma$gamma_dist$scale),
          from = 0, to = 40, n = 1000,  ylim = c(0, 0.2))

    curve(dgamma(x, shape = shape_new, scale = scale_new), add = TRUE, col = "red", n = 1000)
    legend("topright", legend = c("tentative estimates", "adjusted estimates"), lty = 1, col = c("black", "red"))
  }

  c(shape = unname(shape_new), scale = unname(scale_new))
}


# Simulate UD -------------------------------------------------------------

cells2UD <- function(x, res, add_mp = TRUE) {
  xx <- rasterize(x, res, fun = "count", background = 0)
  if (add_mp) xx <- xx + .Machine$double.eps
  xx / sum(xx[], na.rm = TRUE)
}


mk_move_kern <- function(r = 10, shape, scale) {
  mov_kern <- expand.grid(x = -r:r, y = -r:r)
  mov_kern$d <- sqrt(mov_kern[, 1]^2 + mov_kern[, 2]^2) # slightly faster
  mov_kern$d[mov_kern$d == 0] <- 1/(2 * pi)
  mov_kern <- mov_kern[mov_kern$d < r, ]
  mov_kern$mov_kern <- dgamma(mov_kern$d, shape = shape, scale = scale) /
    (2 * pi * mov_kern$d)

  as.matrix(mov_kern)
}

## Generalized
mk_hab_kern <- function(res, omegas_est) {
  r <- as.array(res)
  nc <- dim(r)[2]
  pts <- data.frame(sapply(1:dim(r)[3], function(i) as.vector(t(r[, , i])))) # flatten 2d arrays to 1d
  pts$hab_kern_log <- apply(sweep(pts, 2, omegas_est, `*`), 1, sum)
  pts$hab_kern <- exp(pts$hab_kern_log)
  p2 <- data.frame(rasterToPoints(res))
  pts$x <- floor(p2$x)
  pts$y <- floor(p2$y)
  pts$cell <- pts$x + pts$y * ncol(res)
  pts <- pts[, c("cell", "hab_kern")]
  as.matrix(pts[order(pts$cell), ])
}

simulate_ud <- function(n_steps = 1e5L, mov_kern, hab_kern, start = c(50, 50), res) {
  nc <- ncol(res)
  start <- start[1] + start[2] * nc
  s <- simulate_ssaP1(n_steps, start, nc, mov_kern, hab_kern)
  xy <- list(xy = cbind(x = s %% nc, y = s %/% nc))
  class(xy) <-  c("walk", "list")
  xy
}


simulate_ud_trans <- function(dat, mov_kern, hab_kern, res, every = 10) {

  total_n <- nrow(dat$xy)
  starts <- seq(1, total_n, by = every)

  xx <- lapply(starts, function(i) {
    simulate_ud(n_steps = total_n - i, mov_kern, hab_kern, start = dat$xy[i, ], res)
  })

  cells2UD(do.call(rbind, lapply(xx, "[[", "xy")), res)

}



simulate_and_estimate <- function(alpha, omegas, n, res,
                                  rarify_by, burnin, xy0, n_controll, sim_every = 10 ) {


  x <- simulate_walkf(alpha, omegas, resources = res, n,
                      rarify_by = rarify_by, burnin = burnin, xy0 = xy0)

  cc <- ssf_case_control(x, n_controll, res)
  cc$cc$hab <- factor(cc$cc$hab)
  mod1 <- survival::clogit(case ~ hrc + hab + sl + log_sl + strata(step_id), data = cc$cc)

  # naive
  ud_1 <- exp(coef(mod1)[1] * res[[1]] + coef(mod1)[2] * res[[2]])
  ud_1 <- ud_1 / sum(ud_1[])

  # Simulation
  scale_shape <- correct_scale_shape(cc, coef(mod1)[3:4], FALSE)
  hab_kern <- mk_hab_kern(res, coef(mod1)[1:2])
  mov_kern <- mk_move_kern(qgamma(0.99, scale = scale_shape[1], shape = scale_shape[2]),
                           scale = scale_shape[1], shape = scale_shape[2])


  ud_2 <- simulate_ud_trans(x, mov_kern, hab_kern, res, every = sim_every)
  return(list(ud_1 = ud_1, ud_2 = ud_2, mod = mod1, path = x))
}


kld <- function(x, y) {
  sum(x[] * log(x[]/y[]))
}

rhrCUDRaster <- function(x, ...) {
  r1 <- x
  v <- raster::getValues(r1)
  v <- v / sum(v, na.rm=TRUE)  # standarize
  udFromDat <- raster::setValues(r1, v)
  v <- cumsum(v[order(-v)])[order(order(-v))]
  r2 <- raster::setValues(r1, v)
  return(r2)
}

rhrAreaFast.RasterLayer <- function(x, level = 95, plot = FALSE, ...) {
  x <- rhrCUDRaster(x)
  if (plot) {
    plot(x <= level/100)
  }
  sum(x[] <= level/100) * prod(res(x))
}
