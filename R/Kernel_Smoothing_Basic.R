library(DescTools)
library(optimx)
library(smoothie)
library(gisr)
## Smoothie --------------------------------------------------------------------
nrow = 21
center_value = 1
s_matrix <- CreateCenterValueMatrix(nrow = nrow, center_value = center_value)
# image(s_matrix, col = cols)
gg_s_matrix <- PlotMatrix(s_matrix, 2, "Simple Matrix", label = TRUE)

x = -10:5
y = exp(x)
plot(x, y, type = "l")
log(y)

s_smoothed <- gauss2dsmooth(s_matrix, lambda = sigma,
  nx = RoundTo(nrow(s_matrix), 2), ny = RoundTo(ncol(s_matrix), 2))
sum(s_smoothed)
s_smoothed1 <- s_smoothed/sum(s_smoothed)
sum(s_smoothed1)
gg_s_smoothed1 <- PlotMatrix(s_smoothed1, 2, title = "Smoothie (sigma = 1)",
  label = TRUE)
gg_s_smoothed1

set.seed(1001)
n = 200
beta0 = -1
beta1 = 2
sigma = 1
x <- runif(n, min = -10, max = 10)
y <- beta0 + (x * beta1) + rnorm(n, mean = 0, sd = sigma)
df2 <- data.frame(x,y)
parms2 =  c(-2, 3, 2)

ll_regress <- function(parms, df) {
  df <- df
  beta0 <- parms[1]
  beta1 <- parms[2]
  sigma <- exp(parms[3])
  pred_value <- beta0 + (beta1 * df$x)
  -sum(dnorm(df$y, mean = pred_value, sd = sigma, log = T))
}

optimx_results <- optimx(par = parms2, fn = ll_regress, df = df2,
  lower=c(-50, -50, .0000001), upper=c(50, 50, 100),
  method =c("Nelder-Mead", "L-BFGS-B"),
  control = list(trace=0))
optimx_results

exp(optimx_results$p3)



