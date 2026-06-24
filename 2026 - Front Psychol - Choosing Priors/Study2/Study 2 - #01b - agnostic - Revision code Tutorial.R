library(easystats)
library(ggplot2)
library(rstanarm)

# library(ggridges)

d <- data_read("Example2/Study 2 - Dataset.rds")

# re-scale quantitative predictors so we can set a global prior on all of the
# nuisance predictors
d <- standardize(d, select = c("stay", "age", "cci_c", "barthel_code"))

# Prior-Assumptions ----------------------------------------------

## for the intercept ------------
# - Fall Incidence in hospitals for reference category (no/mild dementia) ~ 5%
prob_fall <- qlogis(0.05)
prob_fall_scale <- 0.5

prior_intercept <- student_t(
  df = 5,
  location = prob_fall,
  scale = prob_fall_scale
)

# scale = .5 (on linear scale) allows a variation of about an assumed range of
# fall incidents from ~ 2% to 13%:
plogis(prob_fall + prob_fall_scale * qt(c(0.05, 0.95), df = 5))


## for the odds-ratios ---------
# skeptical prior - we choose a conservative prior
prob_dem_mid <- 0
prob_dem_hi <- 0

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

m1 <- glm(
  mf1,
  data = d,
  family = binomial("logit")
)


# weakly informative priors ----------------------------- This is inappropriate
# for logistic regression, as it implies very high prior probabilities (0,1) for
# the outcome

# m_weakly <- stan_glm(
#   mf1,
#   data = d,
#   family = binomial("logit"),
#   refresh = 0,
#   cores = 4,
#   seed = 1207
# )

# Models ------------------------

## wide priors on dementia -----------------------------

exp(prob_dem_hi + 2.5 * qt(c(0.05, 0.95), df = 5))

m2a <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 2.5, 2.5, 1, 1, 1, 1, 1, 1, 1)
  ),
  prior_intercept = prior_intercept,

  refresh = 0,
  open_progress = FALSE,
  cores = 4,
  seed = 1207
)

## informative (narrow) priors on dementia -----------------------------

exp(prob_dem_hi + 0.5 * qt(c(0.05, 0.95), df = 5))

m2b <- stan_glm(
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
  open_progress = FALSE,
  cores = 4,
  seed = 1207
)

## very informative (ultranarrow) priors on dementia -----------------------

exp(prob_dem_hi + 0.2 * qt(c(0.05, 0.95), df = 5))

m2c <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 0.2, 0.2, 1, 1, 1, 1, 1, 1, 1)
  ),
  prior_intercept = prior_intercept,

  refresh = 0,
  open_progress = FALSE,
  cores = 4,
  seed = 1207
)

# Save --------------------------------

save(
  m1,
  m2a,
  m2b,
  m2c,
  file = "Study 2b agnostic - Models.RData"
)

sjPlot::tab_model(
  m1,
  m2a,
  m2b,
  m2c,
  show.p = FALSE,
  show.intercept = FALSE,
  terms = "mmse3"
)
