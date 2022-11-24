library(easystats)
library(ggplot2)
library(patchwork)
library(dplyr)
library(forcats)

load("Daten/share.RData")
load("Daten/share8.RData")
load("Daten/share9.RData")


# data preparation ---------------------

## filter waves and missing weights -------------

share <- data_filter(share, both_waves == 2)

share8$crossin_weights[is.na(share8$crossin_weights)] <- share9$crossin_weights[is.na(share8$crossin_weights)]
share9$crossin_weights[is.na(share9$crossin_weights)] <- share8$crossin_weights[is.na(share9$crossin_weights)]

out <- rbind(
  share8[c("mergeid", "crossin_weights", "wave")],
  share9[c("mergeid", "crossin_weights", "wave")]
)
share$crossin_weights <- NULL
share <- data_merge(share, out, by = c("mergeid", "wave"), join = "left")
share <- data_filter(share, !is.na(crossin_weights))


## create categorical variables -------------------

share <- datawizard::to_factor(
  share,
  select = c("health_past3months", "wave", "gender", "covid_affected",
  "partnerinhh", "covid_regime_si3", "covid_regime_ch3")
)
share$stringency_index <- share$global_covid_regime_si3

d <- share |>
  select(mean_str_index, land, wave) |>
  distinct()

d2 <- share |>
  select(stringency_index, land) |>
  distinct()

d$wave <- as.factor(d$wave)
d <- data_join(d, d2, by = "land")

increase <- rep(TRUE, 28)
increase[4] <- FALSE

d$increase <- c(increase, increase)

x1 <- data_filter(d, stringency_index == "low")
x2 <- data_filter(d, stringency_index == "middle")
x3 <- data_filter(d, stringency_index == "high")

p1 <- ggplot(x1, aes(x = mean_str_index, y = fct_rev(land), colour = wave)) +
  geom_line(
    aes(group = land),
    colour = "#999999",
    arrow = arrow(ends = ifelse(x1$increase, "last", "first"), length = unit(8, "points"))
  ) +
  geom_point() +
  labs(x = NULL, colour = NULL, y = NULL, title = "Low SI (lower tercile)") +
  scale_color_material(guide = "none") +
  scale_x_continuous(limits = c(30, 80)) +
  theme_modern(plot.title.size = 14, plot.title.space = 2)
  # theme(
  #   panel.grid.major.y = element_line(colour = "#d0d0d0"),
  #   panel.grid.minor.y = element_line(colour = "#e8e8e8")
  # )


p2 <- ggplot(x2, aes(x = mean_str_index, y = fct_rev(land), colour = wave)) +
  geom_line(
    aes(group = land),
    colour = "#999999",
    arrow = arrow(ends = ifelse(x2$increase, "last", "first"), length = unit(8, "points"))
  ) +
  geom_point() +
  labs(x = NULL, colour = NULL, y = NULL, title = "Middle SI (middle tercile)") +
  scale_color_material(guide = "none") +
  scale_x_continuous(limits = c(30, 80)) +
  theme_modern(plot.title.size = 14, plot.title.space = 2)
  # theme(
  #   panel.grid.major.y = element_line(colour = "#d0d0d0"),
  #   panel.grid.minor.y = element_line(colour = "#e8e8e8")
  # )


p3 <- ggplot(x3, aes(x = mean_str_index, y = fct_rev(land), colour = wave)) +
    geom_line(
    aes(group = land),
    colour = "#999999",
    arrow = arrow(ends = ifelse(x3$increase, "last", "first"), length = unit(8, "points"))
  ) +
  geom_point() +
  labs(x = NULL, colour = "Wave", y = NULL, title = "High SI (upper tercile)") +
  scale_color_material() +
  scale_x_continuous(limits = c(30, 80)) +
  theme_modern(plot.title.size = 14, plot.title.space = 2) +
  theme(legend.position = "bottom")


p1 + p2 + p3

ggsave("change_si.tiff", compress = "lzw", units = "cm", width = 12, height = 7,
       scale = 1.7, dpi = 600)

mean(share$mean_str_index[share$wave == "8"], na.rm = TRUE)
mean(share$mean_str_index[share$wave == "9"], na.rm = TRUE)
