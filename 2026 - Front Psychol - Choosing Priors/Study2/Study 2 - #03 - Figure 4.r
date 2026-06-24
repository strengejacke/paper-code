# Load required libraries
library(easystats) # Provides convenient themes and color palettes
library(ggplot2) # Core plotting engine
library(ggdist) # For visualizing distributions and uncertainty (slabs/intervals)
library(distributional) # For creating and manipulating probability distributions
library(patchwork) # For combining multiple ggplots into a single figure
library(posterior) # For working with Bayesian posterior draws

# Function to extract the prior distribution for the target predictor
extract_mmse2_prior <- function(model) {
  # Extract prior summary from the fitted rstanarm model
  prior <- rstanarm::prior_summary(model)$prior

  # Create a Student's t-distribution object using the extracted location and scale.
  # The index [4] corresponds to the specific parameter for severe dementia in the model.
  dist_student_t(df = 5, mu = prior$location[4], sigma = prior$scale[4])
}

# Main function to build the prior/posterior plot for a given set of models
build_plot <- function() {
  # Construct a data frame containing both posterior draws and prior distributions
  # for the three different scales (wide, narrow, ultranarrow)
  dists_believer <- data.frame(
    width = factor(
      c("wide", "narrow", "ultranarrow"),
      levels = c("ultranarrow", "narrow", "wide") # Set order for the y-axis
    ),

    # Extract the posterior draws for the severe dementia parameter ('mmse3').
    # m2a, m2b, and m2c refer to the models loaded in the environment later on.
    posterior = c(
      as_draws_rvars(m2a)$mmse3,
      as_draws_rvars(m2b)$mmse3,
      as_draws_rvars(m2c)$mmse3
    ),

    # Extract the corresponding priors used for those specific models
    prior = c(
      extract_mmse2_prior(m2a),
      extract_mmse2_prior(m2b),
      extract_mmse2_prior(m2c)
    )
  )

  # Build the ggplot
  ggplot(dists_believer, aes(y = width)) +
    # Add a reference line at 0 (indicating no effect)
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +

    # Plot the prior distributions as semi-transparent slabs (density curves)
    stat_slab(
      aes(xdist = prior, fill = "Prior"),
      alpha = 0.3,
      point_interval = median_hdci # Use median and Highest Density Continuous Interval
    ) +

    # Plot the posterior distributions as slabs with point intervals (median + CI)
    stat_slabinterval(
      aes(xdist = posterior, fill = "Posterior"),
      alpha = 0.3,
      point_interval = median_hdci
    ) +

    # Apply a clean, modern theme from the easystats package
    theme_modern(base_size = 10, axis.text.size = 10) +

    # Manually assign colorblind-friendly colors for the Prior and Posterior
    scale_fill_manual(
      values = c(
        Prior = unname(see::okabeito_colors("red")),
        Posterior = unname(see::okabeito_colors("blue"))
      )
    ) +

    # Add text labels showing the exact median log-OR below the posterior distribution
    geom_text(
      aes(x = 1, label = sprintf("(log-OR %.2f)", median(posterior))),
      nudge_y = -0.15,
      size = 2.8
    ) +

    # Set specific limits and breaks for the x-axis to ensure consistency across subplots
    scale_x_continuous(limits = c(-5, 5), breaks = seq(-4, 4, 2)) +
    scale_thickness_shared() +

    # Clean up labels and legend positioning
    labs(y = NULL, fill = NULL, x = NULL) +
    theme_sub_legend(position = "bottom")
}


# --- Section: Generate Subplots ---

# 1. Load the models for the "Believer" prior scenario and build its plot
load("#Submission Frontiers Psych/#2 Revision/Study 2a believer - Models.RData")
p_believer <- build_plot() + ggtitle("Believer priors")

# 2. Load the models for the "Agnostic" prior scenario and build its plot
load("#Submission Frontiers Psych/#2 Revision/Study 2b agnostic - Models.RData")
p_agnostic <- build_plot() +
  ggtitle("Agnostic priors") +
  scale_y_discrete(labels = NULL) # Hide y-axis labels to avoid clutter when placed side-by-side

# 3. Load the models for the "Skeptical" prior scenario and build its plot
load(
  "#Submission Frontiers Psych/#2 Revision/Study 2c skeptical - Models.RData"
)
p_skeptical <- build_plot() +
  ggtitle("Skeptical priors") +
  labs(x = "log-Odds Ratios") # Add the main x-axis label to the bottom-left plot


# --- Section: Combine and Save ---

# Combine the three subplots into a single figure using the patchwork package
p <- p_believer +
  p_agnostic +
  p_skeptical +
  plot_layout(ncol = 2, guides = "collect") + # Arrange in 2 columns and merge the legends
  plot_annotation(theme = theme_sub_legend(position = "bottom")) # Place the merged legend at the bottom

# Export the final combined plot as a high-resolution TIFF for publication
ggsave(
  "figures_final/Figure 4.tiff",
  plot = p,
  device = "tiff",
  scale = 2.2,
  compress = "lzw", # LZW compression is standard for lossless TIFFs
  width = 9.5,
  height = 9.5,
  unit = "cm",
  dpi = 600 # 600 DPI meets most journal requirements for print quality
)
