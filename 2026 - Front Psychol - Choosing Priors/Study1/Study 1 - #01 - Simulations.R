# --- Script: Study 1 - Simulation Study ---
# This script runs the large-scale simulation demonstrating how prior location,
# prior scale, and sample size interact to influence the posterior distribution.
# Reference for prior scales: https://arxiv.org/abs/1708.07487

# Load required libraries
library(insight) # For extracting information from model objects
library(datawizard) # For data manipulation (standardization)
library(bayestestR) # For Bayesian indices (Probability of Direction, HDI, etc.)
library(parameters) # For extracting summary parameters from frequentist models
library(rstanarm) # For fitting Bayesian regression models using Stan


# --- Section: Simulate Data ---
# Simulates a dataset with a known, true effect size (correlation of r = 0.3)

generate_data <- function(sample_size = 50, effect = 0, true_effect = 0.3) {
  # Generate chunks of data (groups of 5 observations) to introduce
  # realistic sampling variation around the true effect.
  out <- do.call(
    rbind,
    lapply(1:(sample_size / 5), function(i) {
      # Add random noise to the true effect for this specific chunk
      corr <- rnorm(1, mean = true_effect, sd = true_effect * 0.25)
      # Simulate a bivariate correlation with the noisy effect
      simulate_correlation(n = 5, r = corr, mean = effect, sd = 3)
    })
  )

  # If the desired sample size isn't perfectly divisible by 5,
  # fill the remaining rows to reach the exact sample size needed.
  if (nrow(out) < sample_size) {
    out <- rbind(
      out,
      simulate_correlation(
        n = sample_size - nrow(out),
        r = true_effect,
        mean = effect,
        sd = 3
      )
    )
  }

  # Standardize the data (z-scores) so the regression coefficient
  # operates on a standardized metric (-1 to 1).
  d <- standardize(out)
  colnames(d) <- c("y", "x")
  d
}


# --- Section: Fit Models ---
# Fits both a frequentist and a Bayesian linear model to the same dataset

compute_models <- function(dat, location, scale) {
  # 1. Fit classical frequentist linear model (Maximum Likelihood)
  m_freq <- lm(y ~ x, data = dat)

  # 2. Fit Bayesian linear model using Hamiltonian Monte Carlo (rstanarm)
  m_stan <- stan_glm(
    y ~ x,
    data = dat,
    family = gaussian(),
    prior = normal(
      location = location, # The central "guess" of the prior
      scale = scale, # The uncertainty/spread of the prior
      autoscale = FALSE # CRITICAL: Forces rstanarm to use the exact scale provided
      # rather than automatically adjusting it based on data variance.
    ),
    refresh = 0, # Suppress Stan output printing to keep console clean
    chains = 4, # Number of MCMC chains
    iter = 400, # Iterations per chain (lowered here for simulation speed)
    cores = 4, # Run chains in parallel
    open_progress = FALSE
  )

  list(bayes = m_stan, freq = m_freq)
}


# --- Section: Wrapper for Generation and Extraction ---
# Generates data, fits models, and extracts the key statistics into a single row

generate_and_process <- function(
  sample_size,
  effect,
  true_effect,
  location,
  scale,
  simulation
) {
  # Generate the simulated data
  dat <- generate_data(sample_size, effect, true_effect)

  # Fit both models
  models <- compute_models(dat, location, scale)

  # Extract Bayesian posterior summaries
  bayesian_coef <- point_estimate(models$bayes, "all") # Gets Mean, Median, MAP
  bayesian_ci <- bayestestR::ci(models$bayes) # Gets Highest Density Interval (HDI)

  # Calculate Probability of Direction (Pd)
  # Proportion of the posterior distribution that is greater than 0
  pdir_0 <- mean(as.data.frame(models$bayes, pars = "x")[[1]] > 0, na.rm = TRUE)
  # Proportion of the posterior distribution that is greater than the true effect (0.3)
  pdir_0_3 <- mean(
    as.data.frame(models$bayes, pars = "x")[[1]] > 0.3,
    na.rm = TRUE
  )

  # Return all extracted metrics as a single-row dataframe
  data.frame(
    N = sample_size,
    Location = location,
    Scale = scale,
    Effect = effect,
    Simulation = simulation,

    # Bayesian Estimates
    Mean = bayesian_coef$Mean[2],
    Median = bayesian_coef$Median[2],
    SD = sd(as.data.frame(models$bayes, pars = "x")[[1]]),
    MAD = mad(as.data.frame(models$bayes, pars = "x")[[1]]),
    HDI_low = bayesian_ci$CI_low[2],
    HDI_high = bayesian_ci$CI_high[2],
    PD_0 = pdir_0,
    PD_0_3 = pdir_0_3,

    # Frequentist Estimates
    Coefficient = coef(models$freq)["x"],
    SE = standard_error(models$freq)$SE[2],
    CI_low = parameters::ci(models$freq)$CI_low[2],
    CI_high = parameters::ci(models$freq)$CI_high[2],
    stringsAsFactors = FALSE
  )
}


# --- Section: Define Simulation Parameters ---

# Define prior scales based on established recommendations
# "For the rscale argument, several named values are recognized:
# "medium.narrow", "medium", "wide", and "ultrawide". These correspond
# to r scale values of 1/sqrt(27), 1/3, 1/sqrt(3) and 1, respectively.".
# https://www.rdocumentation.org/packages/BayesFactor/versions/0.9.12-4.2/topics/correlationBF)

# Generates the vector: [1.000, 0.577, 0.333, 0.192]
# corresponding to Wide, Medium, Narrow, and Ultranarrow
prior_scales <- 1 / sqrt(3^(0:3))

# Setup grid values for the simulation runs
n_sims <- 100 # Number of iterations per unique combination
sample_sizes <- seq(20, 200, by = 10) # N = 20, 30, 40 ... up to 200
locations <- c(-0.6, -0.3, 0, 0.3, 0.6) # The 5 prior central locations being tested
effect <- 0 # Base mean for the generated variables
true_effect <- 0.3 # The underlying true correlation we are trying to recover
simulations <- seq_len(n_sims)


# --- Section: Execution Setup ---
# Prepare the output dataframe and progress tracking

result <- data.frame()
pb <- txtProgressBar(min = 0, max = length(simulations), style = 3)
total_progress <- length(sample_sizes) *
  length(locations) *
  length(prior_scales)
current_progress <- 0

# Set seed for reproducibility (ensures the same "random" data is generated every time)
set.seed(1207)

tstart <- Sys.time()


# --- Section: Main Simulation Loop ---
# Nested loops iterating over every combination of sample size, location, and scale

for (sample_size in sample_sizes) {
  for (location in locations) {
    for (scale in prior_scales) {
      current_progress <- current_progress + 1
      for (simulation in simulations) {
        # Run the wrapper function for this specific iteration
        out <- generate_and_process(
          sample_size = sample_size,
          effect = effect,
          true_effect = true_effect,
          location = location,
          scale = scale,
          simulation = simulation
        )

        # Append the new row to the main results dataframe
        result <- rbind(result, out)
        setTxtProgressBar(pb, simulation)
      }

      # Print console updates to monitor progress
      cat("\nFinished N=", sample_size, ", Location=", location, "\n")
      cat(
        "Total progress:",
        round(100 * current_progress / total_progress),
        "%",
        "\n"
      )
    }
  }

  # Save an intermediate state after each full sample size block finishes.
  # This prevents total data loss if the computer crashes during the multi-hour run.
  save(
    result,
    file = sprintf(
      "simulations_tmp_%s.RData",
      gsub(
        ".",
        "_",
        make.names(format(Sys.time(), "%a %e %b %Y %H:%M")),
        fixed = TRUE
      )
    )
  )
}

# Print total run time once the loop completes
cat("\n\nElapsed time for simulations: ")
cat(format(Sys.time() - tstart))
cat("\n\n")


# --- Section: Final Data Formatting and Save ---

# Attach simulation metadata as attributes to the dataframe for easy reference later
attr(result, "elapsed_time") <- format(Sys.time() - tstart)
attr(result, "scale") <- prior_scales
attr(result, "location") <- locations
attr(result, "effect") <- effect
attr(result, "true_effect") <- true_effect

close(pb)

# Create a combined grouping variable (useful for ggplot facets/grouping later)
result$Group <- sprintf(
  "N=%i, Location=%g, Scale=%g",
  result$N,
  result$Location,
  result$Scale
)

# Save the final, complete dataset
save(
  result,
  file = sprintf(
    "simulations_%s.RData",
    gsub(
      ".",
      "_",
      make.names(format(Sys.time(), "%a %e %b %Y %H:%M")),
      fixed = TRUE
    )
  )
)
