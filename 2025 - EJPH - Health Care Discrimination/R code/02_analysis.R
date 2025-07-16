library(easystats)
library(glmmTMB)
library(ggeffects)
library(ggplot2)

load("data/diskr.RData")

# MAIHDA - Null model -----------------------------------

m1 <- glmmTMB(
  diskr_amb_sum2 ~ age3 + (1 | sex:income:migrant_background),
  family = nbinom1(),
  weights = weightbe,
  data = d
)


# check model fit -----------------------------------

check_zeroinflation(m1)
check_overdispersion(m1)
check_model(m1)


# Predictions -----------------------------------

# pr1 <- predict_response(
#   m1,
#   c("sex", "income", "migrant_background"),
#   type = "random",
#   interval = "confidence"
# )
# plot(pr1)


# partially adjusted models -----------------------------------

m2a <- glmmTMB(
  diskr_amb_sum2 ~ age3 + income + (1 | sex:income:migrant_background),
  family = nbinom1(),
  weights = weightbe,
  data = d
)

m2b <- glmmTMB(
  diskr_amb_sum2 ~ age3 + migrant_background + (1 | sex:income:migrant_background),
  family = nbinom1(),
  weights = weightbe,
  data = d
)

m2c <- glmmTMB(
  diskr_amb_sum2 ~ age3 + sex + (1 | sex:income:migrant_background),
  family = nbinom1(),
  weights = weightbe,
  data = d
)


# MAIHDA - full model -----------------------------------

m3 <- glmmTMB(
  diskr_amb_sum2 ~ age3 + income + migrant_background + sex + (1 | sex:income:migrant_background),
  family = nbinom1(),
  weights = weightbe,
  data = d
)


# ICCs -------------------------------------

icc(m1)
icc(m2a)
icc(m2b)
icc(m2c)
icc(m3, tolerance = 1e-10)


# r-squared -------------------------------------

r2(m1)
r2(m2a)
r2(m2b)
r2(m2c)
r2(m3, tolerance = 1e-10)


# PVC - Proportional Change in the between-stratum Variance ---------------

v1 <- get_variance(m1)
v2a <- get_variance(m2a)
v2b <- get_variance(m2b)
v2c <- get_variance(m2c)
v3 <- get_variance(m3, tolerance = 1e-10)

# PCV Modell 1 zu Modell 2a
(v1$var.random - v2a$var.random) / v1$var.random

# PCV Modell 1 zu Modell 2b
(v1$var.random - v2b$var.random) / v1$var.random

# PCV Modell 1 zu Modell 2c
(v1$var.random - v2c$var.random) / v1$var.random

# PCV Modell 1 zu Modell full
(v1$var.random - v3$var.random) / v1$var.random


# Coefficient table ------------------------------

compare_parameters(m1, m2a, m2b, m2c, m3, effects = "all", exponentiate = TRUE) |>
  print_html(select = "{estimate} ({ci})")


# predictions plot ------------------------------

# fit model, with "intersec" variable. It's the same as model "m1" above,
# but only using one variable as random effect, which makes the handling
# for the plot slightly easier
m_plot <- glmmTMB(
  diskr_amb_sum2 ~ age3 + (1 | intersec),
  family = nbinom1(),
  weights = weightbe,
  data = d
)

# average predicted events of discrimination
pr_plot <- predict_response(
  m_plot,
  "intersec",
  type = "random",
  interval = "confidence",
  condition = c(age3 = "41-60")
)

# get raw data
x <- get_data(m_plot)
# create a (weighted) frequency table
frq <- data_tabulate(x$intersec, weights = x$weightbe)
# we add the relative frequencies (valid percentages)
# to the axis-labels of the plot
for (i in 1:nlevels(pr_plot$x)) {
  pos <- which(levels(pr_plot$x)[i] == frq$Value)
  levels(pr_plot$x)[i] <- sprintf("%s (%.1f%%)", levels(pr_plot$x)[i], frq$`Valid %`[pos])
}

# plot-code
p <- pr_plot |>
  data_arrange("predicted") |>
  ggplot(aes(
    x = forcats::fct_reorder(x, predicted, .desc = TRUE),
    y = predicted,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_pointrange(fatten = 2) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_ggeffects() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(color = "black")
  )

p
ggsave("varianz_strata.png", plot = p, width = 11, height = 7.5, scale = 0.65, dpi = 600)


# Pairwise Comparisons ------------------------------

comp <- predict_response(
  m_plot,
  "intersec",
  type = "random",
  interval = "confidence",
  condition = c(age3 = "41-60")
)
test_predictions(comp) |>
  data_filter(p.value < 0.05) |>
  data_select(exclude = c("Contrast", "conf.low", "conf.high")) |>
  print_html()
