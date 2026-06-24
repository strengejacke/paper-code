library(easystats)
library(ggplot2)
library(rstanarm)

# load dataset
d <- data_read("Example2/Study 2 - Dataset.rds")

# re-scale quantitative predictors so we can set a global prior on all of the
# nuisance predictors
d <- standardize(d, select = c("stay", "age", "cci_c", "barthel_code"))

# Prior-Assumptions for the intercept ------------
# - Fall Incidence ref. category in hospitals ~ 5%
prob_fall <- qlogis(0.05)
prob_fall_scale <- 0.5

# student-t with 5 df prior for the intercept, which is recommended for
# logistic regression:
# Ghosh J, Li Y, Mitra R (2018) On the Use of Cauchy Prior Distributions for
# Bayesian Logistic Regression. Bayesian Anal 13:. https://doi.org/10.1214/17-BA1051
prior_intercept <- student_t(
  df = 5,
  location = prob_fall,
  scale = prob_fall_scale
)

set.seed(1207)

mf1 <- formula(
  fall_incidence ~ stay +
    age +
    mmse +
    cci_c + # cci instead of multimorb
    barthel_code +
    fall_risk +
    cam_score +
    sex +
    group +
    chemicalres
)


# Models (prior) -------------------------

# Here we run all regression models for the prior predictive checks with different
# prior assumptions on the effect of dementia (mmse). We keep the priors on all other
# predictors the same (weakly informative priors). Most important is the
# prior_PD = TRUE argument, which ensures that we only sample from the prior

## weakly informative (inappropriate) priors on dementia -----------------

m_weakly_ppc <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  refresh = 0,
  cores = 4,
  seed = 1207,
  # only sample from the prior predictive distribution
  prior_PD = TRUE,
  open_progress = FALSE
)

## believer priors on dementia -----------------

prob_dem_mid <- log(2)
prob_dem_hi <- log(3.5)

m_believer_ppc <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1)
  ),
  prior_intercept = prior_intercept,
  refresh = 0,
  cores = 4,
  seed = 1207,
  # only sample from the prior predictive distribution
  prior_PD = TRUE,
  open_progress = FALSE
)

## agnostic priors on dementia -----------------

prob_dem_mid <- 0
prob_dem_hi <- 0

m_agnostic_ppc <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1)
  ),
  prior_intercept = prior_intercept,
  refresh = 0,
  cores = 4,
  seed = 1207,
  # only sample from the prior predictive distribution
  prior_PD = TRUE,
  open_progress = FALSE
)

## sceptical priors on dementia -----------------

prob_dem_mid <- -log(2)
prob_dem_hi <- -log(3.5)

m_skeptical_ppc <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1)
  ),
  prior_intercept = prior_intercept,
  refresh = 0,
  cores = 4,
  seed = 1207,
  # only sample from the prior predictive distribution
  prior_PD = TRUE,
  open_progress = FALSE
)

save(
  m_believer_ppc,
  m_agnostic_ppc,
  m_skeptical_ppc,
  m_weakly_ppc,
  file = "ppc-models.RData"
)

# prior predictive checks -----------------

## prior predictive checks - easily creating the plot -----------------

# prior predictive checks work by simply estimating the relation between
# the predictor of interest (here: mmse) and the outcome (here: fall incidence)
# based on the prior predictive distribution. There are many functions to do
# this, e.g. using `rstanarm::posterior_epred()`. `estimate_relation()` from
# the modelbased package is a convenient wrapper around these functions, which
# works for dozens of modeling packages.
#
# The easiest way to create the plot is to use the `performance::check_priors()`
# function.

performance::check_priors(m_believer_ppc, "mmse")
performance::check_priors(m_agnostic_ppc, "mmse")
performance::check_priors(m_skeptical_ppc, "mmse")
performance::check_priors(m_weakly_ppc, "mmse")


## prior predictive checks - manually creating the plot -----------------

# Here we calculate the prior predictive checks and create the plots by hand,
# using `estimate_relation()`.
#
# For Bayesian models, it is important to keep the draws (sampels) from the
# prior predictive distribution for visualization. This is done by setting
# keep_iterations = TRUE.

# size and alpha of plot elements
dot_size <- 0.95
dot_alpha <- 0.15

## plot function ---------------------

plot_ppc <- function(pdata, ptitle) {
  # base plot
  ggplot(
    pdata,
    aes(x = mmse, group = iter_group, y = iter_value, color = mmse, fill = mmse)
  ) +
    # dots - geom_jitter2 from the see package avoids overplotting and has
    # some nicer dots
    geom_jitter2(alpha = dot_alpha, size = dot_size) +
    geom_boxplot(
      aes(group = NULL),
      alpha = 0.35,
      fill = "white",
      outliers = FALSE,
      # color = "white",
      width = 0.4
    ) +
    # no legend needed, axis labels angled for better readability
    theme_modern(
      legend.position = "none",
      axis.text.angle = 45,
      base_size = 10,
      axis.text.size = 10,
      plot.title.size = 12
    ) +
    # y axis in percent format
    scale_y_continuous(labels = scales::percent_format()) +
    # custom colors for the different levels of mmse. we use the color-blind
    # friendly okabeito palette from the see package
    scale_color_manual(
      values = c(
        mild = unname(see::okabeito_colors("green")),
        moderate = unname(see::okabeito_colors("blue")),
        severe = unname(see::okabeito_colors("red"))
      )
    ) +
    scale_fill_manual(
      values = c(
        mild = unname(see::okabeito_colors("green")),
        moderate = unname(see::okabeito_colors("blue")),
        severe = unname(see::okabeito_colors("red"))
      )
    ) +
    labs(x = NULL, y = NULL, title = ptitle)
}


## believer ---------------------------

out_believer <- modelbased::estimate_relation(
  m_believer_ppc,
  by = "mmse",
  keep_iterations = TRUE
)

# reshape iterations for plotting. These are by default in wide format, but
# for ggplot, we need them in long format.
d_believer <- reshape_iterations(out_believer)

# we convert mmse to a factor with other labels that are used for the plot axes
d_believer$mmse <- factor(
  as.numeric(d_believer$mmse),
  labels = c("mild", "moderate", "severe")
)


## agnostic ---------------------------

out_agnostic <- modelbased::estimate_relation(
  m_agnostic_ppc,
  by = "mmse",
  keep_iterations = TRUE
)

d_agnostic <- reshape_iterations(out_agnostic)
d_agnostic$mmse <- factor(
  as.numeric(d_agnostic$mmse),
  labels = c("mild", "moderate", "severe")
)


## sceptical ---------------------------------------

out_skeptical <- modelbased::estimate_relation(
  m_skeptical_ppc,
  by = "mmse",
  keep_iterations = TRUE
)

d_skeptical <- reshape_iterations(out_skeptical)
d_skeptical$mmse <- factor(
  as.numeric(d_skeptical$mmse),
  labels = c("mild", "moderate", "severe")
)


## inappropriate (weakly informative) ---------------------------

out_weakly <- modelbased::estimate_relation(
  m_weakly_ppc,
  by = "mmse",
  keep_iterations = TRUE
)

d_weakly <- reshape_iterations(out_weakly)
d_weakly$mmse <- factor(
  as.numeric(d_weakly$mmse),
  labels = c("mild", "moderate", "severe")
)


# create plots ----------------------------

p1 <- plot_ppc(d_believer, "Believer")
p3 <- plot_ppc(d_agnostic, "Agnostic")
p5 <- plot_ppc(d_skeptical, "Skeptical")
p7 <- plot_ppc(d_weakly, "Inappropriate")

# this is an alternative histogram plot of the same data

# p2 <- ggplot(
#   reshape_iterations(out_believer),
#   aes(group = iter_group, x = iter_value)
# ) +
#   geom_histogram() +
#   theme_modern() +
#   scale_x_continuous(labels = scales::percent_format()) +
#   scale_y_continuous(limits = c(0, 3000)) +
#   labs(x = NULL, y = NULL, title = NULL)

# arrange and save plots -----------------

p_dots <- plots(
  p1,
  p3,
  p5,
  p7,
  n_rows = 1,
  tags = NULL
)

ggsave(
  "#Submission Frontiers Psych/#2 Revision/Figure 5.tiff",
  plot = p_dots,
  device = "tiff",
  scale = 1.5,
  compress = "lzw",
  width = 17,
  height = 9,
  unit = "cm",
  dpi = 600
)
