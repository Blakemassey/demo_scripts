library(ggplot2)
library(ggthemes)
theme_update(plot.title = element_text(hjust = 0.5))

# Visualize the relationship between probability, odds, and log odds --------- #
probs <- seq(.001, .999, by = .001)
odds <- probs/(1-probs)
ln_odds <- log(odds)
df2 <- data.frame(probs, odds, ln_odds)

ggplot(df2) + geom_line(aes(probs,odds))
ggplot(df2) + geom_line(aes(odds,ln_odds))
ggplot(df2) + geom_line(aes(probs,ln_odds))

# ----------------- Visualize: Predictors and Probabilities ------------------ #

predictors <- seq(-.25, 1.25, by = .01)
intercept <- -5
beta1 <- 10
predictors_logit <- intercept + beta1*(predictors)
df <- data.frame(predictors, probs = plogis(predictors_logit))
(y_mid_int <- (-(1*intercept/beta1)))
rect_df <- data.frame(xmin = c(-.25, 1), ymin=c(0,0), xmax= c(0,1.25),
  ymax = c(1,1))
ggplot(df) + geom_line(aes(predictors, probs), color = "blue") +
  geom_segment(aes(x = y_mid_int, y = 0, xend = y_mid_int, yend = 1),
    color = "red") +
  geom_rect(data = rect_df, alpha = .5, aes(xmin=xmin, ymin=ymin, xmax=xmax,
    ymax=ymax)) +
  annotate("text", x = y_mid_int + .03, y = .03, label = signif(y_mid_int, 2),
    color = "red") +
  ylim(0,1) + labs(x = "Predictor", y = "Probability") + theme_no_legend +
  ggtitle(paste0("Logistic Regression (", "intercept = ", intercept,
    ", beta1 = ", beta1, ")")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0))

# -------- Example 1: Predictor = Continous, Response = Binomial  ------------ #

library('MASS')
data("menarche")
health_data <- menarche %>% transmute(age = Age, total = Total,
  menarche = Menarche, non_menarche = total-menarche, prop = menarche/total)
mod <- glm(cbind(menarche, non_menarche) ~ age, family=binomial, data=health_data)


#predict gives the predicted value in terms of logits
#predict gives the predicted value in terms of logits
plot.dat <- data.frame(prob = menarche$Menarche/menarche$Total,
                       age = menarche$Age,
                       fit = predict(m, menarche))

health_data %>%
  mutate(fit <- )

#convert those logit values to probabilities
plot.dat$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))

library(ggplot2)
ggplot(plot.dat, aes(x=age, y=prob)) +
  geom_point() +
  geom_line(aes(x=age, y=fit_prob))



pred_data <- data.frame(fit = predict(mod, health_data))
#convert those logit values to probabilities
health_data[, "fit"] <- exp(pred_data$fit)/(1+exp(pred_data$fit))

library(ggplot2)
p <- ggplot(health_data, aes(x=age, y=prop)) + geom_point()
p
ggplot(pred_data, aes(x=age, y=prob)) +
  geom_point() +
  geom_line(aes(x=age, y=fit_prob))

exp(coef(mod))
exp(intercept + coef*THOUGHT_Value)/(1+(exp(intercept+coef*THOUGHT_Value))

# ---- EXAMPLE 2: Predictor = Continous (x2), Response = Binary -------------- #
# Create fake data
heat_data <- data.frame(
  fitness = c(1, 6, 11, 16, 21, 2, 7, 12, 17, 22, 3, 8, 13, 18, 23, 4, 9, 14,
    19, 5, 10, 15, 20),
  temperature = c(66, 72, 70, 75, 75, 70, 73, 78, 70, 76, 69, 70, 67, 81, 58,
    68, 57, 53, 76, 67, 63, 67, 79),
  heat_stress = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0,
    1, 1))

# Run logistic regression model
mod1 <- glm(heat_stress ~ temperature, data = heat_data, family = binomial)

# Create a temporary data frame of hypothetical values
pred_range <- data.frame(temperature = seq(53, 81, 0.5))

# Predict the fitted values given the model and hypothetical data
pred_values <- as.data.frame(predict(mod1, newdata = pred_range,
  type="link", se=TRUE))

# Combine the hypothetical data and predicted values
pred_data <- cbind(pred_range, pred_values)

# Calculate confidence intervals
std <- qnorm(0.95 / 2 + 0.5)
pred_data$ymin <- model$family$linkinv(pred_data$fit - std * pred_data$se)
pred_data$ymax <- model$family$linkinv(pred_data$fit + std * pred_data$se)
pred_data$fit <- model$family$linkinv(pred_data$fit)  # Rescale to 0-1

# Plot everything
p <- ggplot(heat_data, aes(x=temperature, y=heat_stress))
p + geom_point() +
  geom_ribbon(data=pred_data, aes(x = temperature, ymin = ymin, ymax = ymax),
    alpha=0.5, inherit.aes = FALSE) +
  geom_line(data=pred_data, aes(y=fit)) +
  labs(x="Temperature", y="Heat Stress")

# Run logistic regression model with two covariates
# Run logistic regression model
mod2 <- glm(heat_stress ~ temperature + fitness, data = heat_data,
  family = binomial)

# Create a temporary data frame of hypothetical values
pred_range <- data.frame(temperature = rep(seq(53, 81, 0.5), 2),
                        fitness = c(rep(3, 57), rep(18, 57)))

# Predict the fitted values given the model and hypothetical data
pred_values <- as.data.frame(predict(mod2, newdata = pred_range,
  type="link", se=TRUE))

# Combine the hypothetical data and predicted values
pred_data <- cbind(pred_range, pred_values)

# Calculate confidence intervals
std <- qnorm(0.95 / 2 + 0.5)
pred_data$ymin <- model$family$linkinv(pred_data$fit - std * pred_data$se)
pred_data$ymax <- model$family$linkinv(pred_data$fit + std * pred_data$se)
pred_data$fit <- model$family$linkinv(pred_data$fit)  # Rescale to 0-1

# Plot everything
p <- ggplot(heat_data, aes(x=temperature, y=heat_stress))
p + geom_point() +
  geom_ribbon(data=pred_data, aes(x=temperature, ymin=ymin, ymax=ymax,
    fill=as.factor(fitness)), alpha=0.4, inherit.aes = FALSE) +
  scale_fill_brewer(type = "qual", palette = 2, name = "Fitness Level") +
  geom_line(data=pred_data, aes(y=fit, colour=as.factor(fitness))) +
  scale_colour_brewer(type = "qual", palette = 2, name = "Fitness Level") +
  labs(x="Temperature", y="Heat Stress")
