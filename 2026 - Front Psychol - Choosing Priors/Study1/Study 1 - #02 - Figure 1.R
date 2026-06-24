# Load required libraries
library(ggplot2) # Core plotting engine
library(patchwork) # For combining multiple ggplots into a cohesive layout

# --- Section: Custom Bayesian Plotting Function ---

# This function generates a row of three plots: Prior, Likelihood, and Posterior
plot_bayes <- function(
  dPrior, # Function defining the prior density
  dLikelihood, # Function defining the likelihood density
  xlim = c(-10, 10), # x-axis limits
  ylim = 0.4, # y-axis upper limit
  header = FALSE, # Whether to add column titles (for the top row)
  footer = FALSE, # Whether to add x-axis labels (for the bottom row)
  proper = TRUE # If TRUE, the prior is a proper distribution (filled area).
  # If FALSE, it's an improper flat prior (just a line).
) {
  # Nested function to calculate the posterior density mathematically
  # Posterior is proportional to Prior * Likelihood
  dPost <- function(x, fPrior, fLikelihood, norm = TRUE) {
    # Multiply prior and likelihood
    d <- fPrior(x) * fLikelihood(x)

    # Normalize the area under the curve to 1 (valid probability distribution)
    if (norm) {
      d <- d /
        integrate(
          dPost,
          -Inf,
          Inf,
          fPrior = fPrior,
          fLikelihood = fLikelihood,
          norm = FALSE
        )[["value"]] # Extract the integral value
    }
    d
  }

  # Plot 1: The Prior Distribution
  p1 <- ggplot() +
    stat_function(
      fun = dPrior,
      xlim = xlim,
      size = 0.9,
      fill = "blue",
      color = if (!proper) "blue" else NA, # Ensure line is visible for flat prior
      geom = if (proper) "area" else "line", # Use line instead of area for improper flat prior
      alpha = 0.4
    ) +
    labs(
      y = NULL,
      x = if (footer) expression(theta) else NULL,
      title = if (header) "Prior (blue)"
    )

  # Plot 2: The Likelihood Function
  p2 <- ggplot() +
    stat_function(
      fun = dLikelihood,
      xlim = xlim,
      size = 0.9,
      color = "orange2"
    ) +
    labs(
      y = NULL,
      x = if (footer) expression(theta) else NULL,
      title = if (header) "Likelihood (yellow)"
    )

  # Plot 3: The Posterior Distribution (overlaid with Prior and Likelihood)
  p3 <- ggplot() +
    # Draw the Prior
    stat_function(
      fun = dPrior,
      xlim = xlim,
      size = 0.9,
      fill = "blue",
      color = if (!proper) "blue" else NA,
      geom = if (proper) "area" else "line",
      alpha = 0.4
    ) +
    # Draw the computed Posterior
    stat_function(
      fun = dPost,
      xlim = xlim,
      size = 0.9,
      fill = "darkgreen",
      geom = "area",
      alpha = 0.4,
      args = list(fPrior = dPrior, fLikelihood = dLikelihood)
    ) +
    # Draw the Likelihood outline
    stat_function(fun = dLikelihood, xlim = xlim, size = 1, color = "orange2") +
    labs(
      y = NULL,
      x = if (footer) expression(theta) else NULL,
      color = NULL,
      title = if (header) "Posterior (green)"
    )

  # Combine the three plots side-by-side using patchwork
  p1 +
    p2 +
    p3 &
    coord_cartesian(ylim = c(0, ylim)) &
    theme_minimal() &
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(), # Hide y-axis text since density values are relative
      axis.line.y = element_blank(),
      legend.position = "none"
    )
}


# --- Section: Part 1 - Impact of Prior Distribution ---

# Define the sequence of scales (standard deviations) for the priors
prior_scales <- 10 / sqrt(3^(0:3))

# Define a fixed likelihood (representing a fixed dataset)
dLikelihood <- function(x) dnorm(x, 1.5, sqrt(5))

# Scenario 1.1: Flat (improper) prior - assigning a constant visual height
dPrior1 <- function(x) rep(0.07, length(x))
p1 <- plot_bayes(dPrior1, dLikelihood, header = TRUE, proper = FALSE)

# Scenario 1.2: Wide prior
dPrior2 <- function(x) dnorm(x, 0 - 1.5, prior_scales[2])
p2 <- plot_bayes(dPrior2, dLikelihood)

# Scenario 1.3: Narrow prior
dPrior3 <- function(x) dnorm(x, 2.5 + 1.5, prior_scales[3])
p3 <- plot_bayes(dPrior3, dLikelihood)

# Scenario 1.4: Very narrow prior
dPrior4 <- function(x) dnorm(x, 2.5 + 1.5, prior_scales[4])
p4 <- plot_bayes(dPrior4, dLikelihood, footer = TRUE)

# Combine the four prior scenarios vertically using see::plots
out1 <- see::plots(
  p1,
  p2,
  p3,
  p4,
  n_columns = 1,
  # Add labels to the left side indicating the prior type
  tags = c(
    "Flat\n(improper)",
    "",
    "",
    "Wide",
    "",
    "",
    "Narrow",
    "",
    "",
    "Very narrow"
  ),
  title = "Impact of Prior Distribution",
  theme = theme(title = element_text(size = 12.5))
)


# --- Section: Part 2 - Impact of Sample Size ---

# Here, the prior is kept fixed (Very narrow: dPrior4),
# but the likelihood narrows to simulate an increasing sample size (N).

# Scenario 2.1: Baseline sample size (N=30)
p1 <- plot_bayes(dPrior4, dLikelihood, header = TRUE, ylim = .6)

# Scenario 2.2: Larger sample size (N=60) - standard deviation of likelihood decreases
p2 <- plot_bayes(dPrior4, function(x) dnorm(x, 1.5, sqrt(5 / 2)), ylim = .6)

# Scenario 2.3: Even larger sample size (N=150)
p3 <- plot_bayes(dPrior4, function(x) dnorm(x, 1.5, 1), ylim = .6)

# Scenario 2.4: Largest sample size (N=300)
p4 <- plot_bayes(
  dPrior4,
  function(x) dnorm(x, 1.5, sqrt(5 / 10)),
  ylim = .6,
  footer = TRUE
)

# Combine the four sample size scenarios vertically
out2 <- see::plots(
  p1,
  p2,
  p3,
  p4,
  tags = c("N = 30", "", "", "N = 60", "", "", "N = 150", "", "", "N = 300"),
  n_columns = 1,
  title = "Impact of Sample Size",
  theme = theme(title = element_text(size = 12.5))
)


# --- Section: Final Composition and Export ---

# Stack Part 1 (out1) on top of Part 2 (out2) using patchwork's '/' operator
final <- wrap_elements(out1) / wrap_elements(out2)

# Save the complete figure as a high-resolution TIFF
ggsave(
  "figures_final/Figure 1.tiff",
  plot = final,
  device = "tiff",
  scale = 1.9,
  compress = "lzw", # LZW compression is standard for lossless TIFFs
  width = 12,
  height = 16,
  unit = "cm",
  dpi = 600
)

# Alternative individual saves (currently commented out)
# ggsave(
#   "#Submission Frontiers Psych/#2 Revision/Figure 1a.tiff",
#   plot = out1,
#   device = "tiff",
#   scale = 1.9,
#   compress = "lzw",
#   width = 12,
#   height = 8,
#   unit = "cm",
#   dpi = 600
# )

# ggsave(
#   "#Submission Frontiers Psych/#2 Revision/Figure 1b.tiff",
#   plot = out2,
#   device = "tiff",
#   scale = 1.9,
#   compress = "lzw",
#   width = 12,
#   height = 8,
#   unit = "cm",
#   dpi = 600
# )
