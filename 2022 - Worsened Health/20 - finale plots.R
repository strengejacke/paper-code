library(easystats)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(marginaleffects)
library(glmmTMB)
library(patchwork)

load("regression_analyses_interactions.RData")

# Plot-Funktion --------------------------------


abbildung <- function(predict_data,
                      comparison_data,
                      title = NULL,
                      prozent_label = TRUE,
                      axis_labels = NULL,
                      legend = FALSE,
                      log_ylim = -2) {


  predict_data <- merge(predict_data, comparison_data, all = TRUE)
  predict_data$p <- insight::format_p(predict_data$p)

  # journal requires 0 before period, and italic p
  predict_data$p[predict_data$p == "p < .001"] <- "p < 0.001"
  predict_data$p <- gsub("p", "italic(p)", predict_data$p, fixed = TRUE)
  predict_data$p <- gsub("=", "==", predict_data$p, fixed = TRUE)

  round_percent <- function(ylim) {
    sprintf("%g%%", round(100 * ylim))
  }

  if (log_ylim == -2) {
    upper_lim <- .3
  } else {
    upper_lim <- .5
  }

  y.breaks <- c(-5:log_ylim)
  y.breaks <- 2^y.breaks[!is.na(y.breaks)]
  y.limits <- c(.03125, upper_lim)

  plot_data <- predict_data

  cp <- predict_data |>
    dplyr::group_by(x) |>
    dplyr::slice(which.max(conf.high)) |>
    data_merge(comparison_data) |>
    dplyr::mutate(y = conf.high * 1.05)

  p <- ggplot(plot_data, aes(x = x, y = predicted, ymin = conf.low,
                             ymax = conf.high, colour = group)) +
    geom_errorbar(position = position_dodge(.3), width = 0, size = .8) +
    geom_point(position = position_dodge(.3), size = 3.3) +
    # geom_text(data = cp, mapping = aes(x = x, y = y, label = cp$p),
    #           color = "black",
    #           position = position_dodge(.12), parse = TRUE) +
    see::scale_color_flat() +
    see::scale_fill_flat() +
    see::theme_modern(plot.title.size = 14, plot.title.space = 2) +
    theme(panel.grid.major.y = element_line(colour = "#d0d0d0"),
          panel.grid.minor.y = element_line(colour = "#e8e8e8"))

  if (legend) {
    p <- p +
      theme(legend.position = "bottom") +
      labs(y = NULL, x = NULL, title = title, colour = "Wave")
  } else {
    p <- p +
      guides(fill = "none", color = "none") +
      labs(y = NULL, x = NULL, title = title)
  }

  if (prozent_label) {
    p <- p + scale_y_log10(labels = round_percent(y.breaks), breaks = y.breaks, limits = y.limits)
  } else {
    p <- p + scale_y_log10(labels = NULL, limits = y.limits)
  }

  p <- p + scale_x_discrete(labels = levels(plot_data$x))

  p
}






# Abbildung 1: Age --------------------------

pv1 <- ggpredict(mi1, c("age_dicho", "wave"))
pc1 <- mi1 |>
  comparisons(
    type = "response",
    variables = "wave") |>
  summary(by = "age_dicho") |>
  as.data.frame() |>
  data_select(c("age_dicho", "p.value")) |>
  standardize_names() |>
  data_rename(pattern = "age_dicho", replacement = "x")

pc1 <- pc1[order(pc1$x, levels(pc1$x)), ]




# Abbildung 2: Gender --------------------------

pv2 <- ggpredict(mi2, c("gender", "wave"))
pc2 <- mi2 |>
  comparisons(
    type = "response",
    variables = "wave") |>
  summary(by = "gender") |>
  as.data.frame() |>
  data_select(c("gender", "p.value")) |>
  standardize_names() |>
  data_rename(pattern = "gender", replacement = "x")

pc2 <- pc2[order(pc2$x, levels(pc2$x)), ]




# Abbildung 3: Education --------------------------

pv3 <- ggpredict(mi3, c("education2", "wave"))
pc3 <- mi3 |>
  comparisons(
    type = "response",
    variables = "wave") |>
  summary(by = "education2") |>
  as.data.frame() |>
  data_select(c("education2", "p.value")) |>
  standardize_names() |>
  data_rename(pattern = "education2", replacement = "x")

pc3 <- pc3[order(pc3$x, levels(pc3$x)), ]






# Abbildung 4: Partner in HH --------------------------

pv4 <- ggpredict(mi4, c("partnerinhh", "wave"))
pc4 <- mi4 |>
  comparisons(
    type = "response",
    variables = "wave") |>
  summary(by = "partnerinhh") |>
  as.data.frame() |>
  data_select(c("partnerinhh", "p.value")) |>
  standardize_names() |>
  data_rename(pattern = "partnerinhh", replacement = "x")

pc4 <- pc4[order(pc4$x, levels(pc4$x)), ]





# Abbildung 5: Covid affected  --------------------------

pv5 <- ggpredict(mi5, c("covid_affected", "wave"))

pc5 <- mi5 |>
  comparisons(
    type = "response",
    variables = "wave") |>
  summary(by = "covid_affected") |>
  as.data.frame() |>
  data_select(c("covid_affected", "p.value")) |>
  standardize_names() |>
  data_rename(pattern = "covid_affected", replacement = "x")

# fix long labes
levels(pv5$x) <- c("never", "positive, no symptoms", "symptoms", "hospitalized")
levels(pc5$x) <- c("never", "positive, no symptoms", "symptoms", "hospitalized")

pc5 <- pc5[order(pc5$x, levels(pc5$x)), ]





p1 <- abbildung(pv1, pc1, title = "Age")
p2 <- abbildung(pv2, pc2, title = "Gender")
p3 <- abbildung(pv3, pc3, title = "Education")
p4 <- abbildung(pv4, pc4, title = "Partner in household")
p5 <- abbildung(pv5, pc5, title = "COVID affected", legend = TRUE, log_ylim = -1)



layout <- c(
  area(1, 1),
  area(1, 2),
  area(1, 3),
  area(2, 1),
  area(2, 2, 2, 3)
)

plot(layout)

p1 + p2 + p3 + p4 + p5 + plot_layout(design = layout)

ggsave("figure.tiff", compress = "lzw", units = "cm", width = 17, height = 14,
       scale = 1.3, dpi = 600)


save(pv1, pv2, pv3, pv4, pv5, pc1, pc2, pc3, pc4, pc5, file = "predictions_for_plots.RData")
