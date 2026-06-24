# --- Script: Study 2 - Case-Control Study (Believer Priors) ---
# This script demonstrates the practical application of prior elicitation
# in a logistic regression model. It fits a frequentist baseline model and
# three Bayesian models using "believer" (optimistic) priors with varying
# levels of uncertainty (wide, narrow, ultranarrow scales).

# Load required libraries
library(easystats) # For data scaling and formatting
library(ggplot2) # Core plotting engine
library(rstanarm) # For fitting Bayesian regression models using Stan

# Load the dataset for Study 2
d <- data_read("Example2/Study 2 - Dataset.rds")

# --- Section: Data Preparation ---

# Re-scale quantitative predictors (z-standardization).
# Why? Standardizing allows us to set a uniform, global default prior (scale = 1)
# on all "nuisance" predictors because they are now all on the same metric.
d <- standardize(d, select = c("stay", "age", "cci_c", "barthel_code"))


# --- Section: Prior Assumptions & Elicitation ---

## 1. Prior for the Intercept (Baseline Fall Risk)
# We assume the fall incidence in hospitals for our reference category
# (patients with mild or no dementia) is approximately 5%.
# Since logistic regression operates on the log-odds scale, we must convert 5%.
prob_fall <- qlogis(0.05) # qlogis() converts probability to log-odds (approx -2.94)

# We assign a moderate degree of uncertainty to this baseline guess.
prob_fall_scale <- 0.5

# Define the prior for the intercept as a Student's t-distribution (df = 5)
prior_intercept <- student_t(
  df = 5,
  location = prob_fall,
  scale = prob_fall_scale
)

# Sanity Check: What does a scale of 0.5 mean in terms of actual probability?
# Using plogis() to convert the 5th and 95th quantiles back to probabilities,
# we see this prior assumes the true baseline fall risk is likely between ~2% and ~11%.
plogis(prob_fall + prob_fall_scale * qt(c(0.05, 0.95), df = 5))


## 2. Prior for the Predictors of Interest (Odds Ratios)
# Literature suggests moderate dementia doubles the odds of falling (OR = 2),
# and severe dementia increases the odds 3.5-fold (OR = 3.5).
# Again, we must convert these Odds Ratios to the log-odds (linear) scale.
prob_dem_mid <- log(2) # Log-odds location for moderate dementia (approx 0.69)
prob_dem_hi <- log(3.5) # Log-odds location for severe dementia (approx 1.25)


# --- Section: Model Setup ---

set.seed(1207) # Ensure reproducibility

# Define the model formula
# Note: 'mmse' is a categorical variable with 3 levels. The model will create
# 2 dummy variables (moderate and severe), with 'mild' as the hidden reference.
mf1 <- formula(
  fall_incidence ~ stay +
    age +
    mmse +
    cci_c +
    barthel_code +
    fall_risk +
    cam_score +
    sex +
    group +
    chemicalres
)

# Fit Baseline Model: Classical Frequentist Logistic Regression (Maximum Likelihood)
# This will suffer from separation due to sparse data in the severe dementia group.
m1 <- glm(
  mf1,
  data = d,
  family = binomial("logit")
)


# --- Section: Bayesian Models (Believer Priors) ---

# Note on prior vectors: The `location` and `scale` arguments require a vector
# of values corresponding to every term in the model formula.
# Positions 3 and 4 correspond to the 'moderate' and 'severe' dementia dummy
# variables.
# All other covariates are assigned a location of 0 and a scale of 1 (weakly
# informative).

## Model 2a: Wide priors on dementia (Scale = 2.5) -------------------------

# Sanity Check: Calculate the 90% credible interval of the prior on the OR scale.
# A wide scale of 2.5 implies an extremely wide, almost uninformative range of
# expected ORs.
exp(prob_dem_hi + 2.5 * qt(c(0.05, 0.95), df = 5))

m2a <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 2.5, 2.5, 1, 1, 1, 1, 1, 1, 1) # Dementia terms get scale 2.5
  ),
  prior_intercept = prior_intercept,
  refresh = 0,
  open_progress = FALSE,
  cores = 4,
  seed = 1207
)

## Model 2b: Informative (Narrow) priors on dementia (Scale = 0.5) ---------

# Sanity Check: A scale of 0.5 yields a much more reasonable 90% prior interval
# for the OR, ranging from approx 1.28 to 9.59.
exp(prob_dem_hi + 0.5 * qt(c(0.05, 0.95), df = 5))

m2b <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1) # Dementia terms get scale 0.5
  ),
  prior_intercept = prior_intercept,
  refresh = 0,
  open_progress = FALSE,
  cores = 4,
  seed = 1207
)

## Model 2c: Very Informative (Ultranarrow) priors on dementia (Scale = 0.2)

# Sanity Check: A scale of 0.2 implies extreme confidence, restricting the
# expected OR tightly between approx 2.34 and 5.24.
exp(prob_dem_hi + 0.2 * qt(c(0.05, 0.95), df = 5))

m2c <- stan_glm(
  mf1,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 0.2, 0.2, 1, 1, 1, 1, 1, 1, 1) # Dementia terms get scale 0.2
  ),
  prior_intercept = prior_intercept,
  refresh = 0,
  open_progress = FALSE,
  cores = 4,
  seed = 1207
)

# --- Section: Save and Summarize ---

# Save the fitted models to an RData file so they can be loaded by the plotting
# scripts
save(
  m1,
  m2a,
  m2b,
  m2c,
  file = "Study 2a believer - Models.RData"
)

# Generate a clean, comparative HTML table of the models using sjPlot.
# We isolate the output to only show the "severe dementia" term (mmse3) for clarity.
sjPlot::tab_model(
  m1,
  m2a,
  m2b,
  m2c,
  show.p = FALSE,
  show.intercept = FALSE,
  terms = "mmse3"
)
