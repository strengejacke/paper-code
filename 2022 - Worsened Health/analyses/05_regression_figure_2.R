library(easystats)
library(glmmTMB)
library(ggeffects)
library(emmeans)
library(ggplot2)
library(survey)

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
  select = c("wave", "gender", "covid_affected", "partnerinhh", "covid_regime_si3", "covid_regime_ch3")
)
share$stringency_index <- share$global_covid_regime_si3


## create frequency weights ----------------

share <- rescale_weights(share, group = "countries", probability_weights = "crossin_weights")


## recode variables ----------------

share$age_dicho <- factor(categorize(share$age), labels = c("50-69", "70+"))
share <- share |>
  dplyr::group_by(wave) |>
  categorize(select = "covid_percent", split = "quantile", n_groups = 3, append = TRUE)

share$covid_percent3 <- factor(share$covid_percent_r, labels = c("lower tertile", "middle tertile", "upper tertile"))



# mixed models, interactions ---------------------

mi1 <- glmmTMB(
  health_past3months ~ wave * age_dicho + gender + education2 + partnerinhh +
    covid_affected + stringency_index + covid_percent3 +
    (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

mi2 <- glmmTMB(
  health_past3months ~ wave * gender + age_dicho + education2 + partnerinhh +
    covid_affected + stringency_index + covid_percent3 +
    (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

mi3 <- glmmTMB(
  health_past3months ~ wave * education2 + age_dicho + gender + partnerinhh +
    covid_affected + stringency_index + covid_percent3 +
    (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

mi4 <- glmmTMB(
  health_past3months ~ wave * partnerinhh + age_dicho + gender + education2 +
    covid_affected + stringency_index + covid_percent3 +
    (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

mi5 <- glmmTMB(
  health_past3months ~ wave * covid_affected + age_dicho + gender + education2 +
    partnerinhh + stringency_index + covid_percent3 +
    (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

save(share, mi1, mi2, mi3, mi4, mi5, file = "regression_analyses_interactions.RData")
# save(share, mi1, mi2, mi3, mi4, mi5, mi6, mi7, file = "regression_analyses_interactions.RData")
