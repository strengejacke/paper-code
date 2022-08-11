library(easystats)
library(ggplot2)
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

d$wave <- as.factor(d$wave)

increase <- rep(TRUE, 28)
increase[4] <- FALSE

ggplot(d, aes(x = mean_str_index, y = fct_rev(land), colour = wave)) +
  geom_line(
    aes(group = land),
    colour = "#999999",
    arrow = arrow(ends = ifelse(increase, "last", "first"), length = unit(8, "points"))
  ) +
  geom_point() +
  labs(x = "Average Stringency Index", colour = "Wave", y = NULL) +
  scale_color_material() +
  theme_lucid() +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(limits = c(30, 80))

ggsave("change_si.tiff", compress = "lzw", units = "cm", width = 7, height = 8,
       scale = 2, dpi = 600)

mean(share$mean_str_index[share$wave == "8"], na.rm = TRUE)
mean(share$mean_str_index[share$wave == "9"], na.rm = TRUE)
