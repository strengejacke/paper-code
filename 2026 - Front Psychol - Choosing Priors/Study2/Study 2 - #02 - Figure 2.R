# Load required libraries
library(see) # Provides plotting themes and color palettes (part of easystats)
library(datawizard) # For data manipulation and transformation (part of easystats)
library(ggplot2) # Core plotting engine

# --- Section: Data Generation ---

# Create a sequence of x values from -10 to 10 with a very fine step size
x <- seq(-10, 10, by = 0.005)

# Calculate the probability density (y) for a Student's t-distribution with 5
# degrees of freedom
y <- dt(x, 5)

# Combine into a data frame and create a grouping variable 'SD' (representing
# scale units)
dat <- data.frame(x = x, y = y, SD = "4 Scalers", stringsAsFactors = FALSE)

# Categorize the x values into scale units to help structure the plot regions
dat$SD[dat$x >= -3 & dat$x <= 3] <- "3 Scalers"
dat$SD[dat$x >= -2 & dat$x <= 2] <- "2 Scalers"
dat$SD[dat$x >= -1 & dat$x <= 1] <- "1 Scalers"


# --- Section: Helper Functions ---

# Define a custom function to shade specific regions under the density curve
# It filters the data between zstart and zend and draws a filled polygon (geom_area)
shade_curve <- function(data, zstart, zend, alpha = .5) {
  geom_area(
    data = subset(data, x >= 0 + zstart & x < 0 + zend),
    aes(y = y),
    fill = see::see_colors("indigo"), # Use the indigo color from the 'see' package
    color = NA, # Remove borders around the shaded areas
    alpha = alpha # Set transparency
  )
}


# --- Section: Data Preparation for Labels ---

# Calculate approximate cumulative probabilities to label the dashed lines
dat_filter <- dat |>
  # Because the step size is 0.005, sum(y) is approx 200.
  # Dividing cumsum(y) by 2 gives a rough percentage (0 to 100) for the
  # cumulative probability.
  data_modify(y_cdf = cumsum(y) / 2) |>
  # Keep only the rows where x is exactly at our integer scale boundaries
  data_filter(x %in% c(-4, -3, -2, -1, 1, 2, 3, 4))


# --- Section: Building the Plot ---

p <- ggplot(dat, aes(x = x, y = y)) +
  # Draw the main density curve outline
  geom_line() +

  # Shade the central region (-1 to 1) with the highest opacity (darkest)
  shade_curve(data = dat, zstart = -1, zend = 1, alpha = 0.7) +

  # Shade the outer regions with progressively lower opacity (lighter)
  shade_curve(data = dat, zstart = -2, zend = -1, alpha = 0.5) +
  shade_curve(data = dat, zstart = 1, zend = 2, alpha = 0.5) +
  shade_curve(data = dat, zstart = -3, zend = -2, alpha = 0.3) +
  shade_curve(data = dat, zstart = 2, zend = 3, alpha = 0.3) +
  shade_curve(data = dat, zstart = -4, zend = -3, alpha = 0.1) +
  shade_curve(data = dat, zstart = 3, zend = 4, alpha = 0.1) +

  # Format the x-axis limits and explicitly set breaks at every integer
  scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +

  # Apply a clean, modern theme from the 'see' package
  theme_modern(show.ticks = TRUE, base_size = 12) +

  # Set axis labels (using an expression to render the theta symbol properly)
  labs(
    x = expression("Scale of " * theta),
    y = NULL
    # title = "Probability Mass for Student's t-Distribution with 5 DF"
  ) +

  # Hide the y-axis text/labels since the exact density values aren't the focus
  scale_y_continuous(labels = NULL) +

  # Add the percentage labels above each dashed line
  geom_text(
    data = dat_filter,
    aes(x = x, y = y + 0.07, label = paste0(round(y_cdf, 0), "%")),
    check_overlap = TRUE
  ) +

  # Draw the dashed vertical lines at each scale integer
  geom_segment(
    data = dat_filter,
    aes(x = x, xend = x, y = 0, yend = y),
    linetype = "dashed"
  ) +

  # Add the central explanatory text inside the darkest shaded region
  annotate(
    "text",
    x = 0,
    y = 0.1,
    label = "~ 63% of the\nprobability mass",
    colour = "white"
  )


# --- Section: Export ---

# Export the final plot as a high-resolution TIFF for publication
ggsave(
  "#Submission Frontiers Psych/#2 Revision/Figure 2.tiff",
  plot = p,
  device = "tiff",
  scale = 2.25,
  compress = "lzw", # LZW compression is standard for lossless TIFFs
  width = 8.5,
  height = 5,
  unit = "cm",
  dpi = 600 # 600 DPI ensures high print quality
)
