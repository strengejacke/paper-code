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



# mixed models, w/o interaction ---------------------

model1 <- glmmTMB(
  health_past3months ~ wave + age_dicho + gender + education2 + partnerinhh +
    covid_affected + (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)


model2 <- glmmTMB(
  health_past3months ~ wave + stringency_index + covid_percent3 +
    (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)


model3 <- glmmTMB(
  health_past3months ~ wave + age_dicho + gender + education2 + partnerinhh +
    covid_affected + stringency_index + covid_percent3 +
    (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

save(share, model1, model2, model3, file = "regression_analyses2.RData")





# mixed models, w/o interaction, unweighted ---------------------

model1 <- glmmTMB(
  health_past3months ~ wave + age_dicho + gender + education2 + partnerinhh +
    covid_affected + (1 + wave | mergeid) + (1 | countries),
  family = binomial(),
  data = share
)


model2 <- glmmTMB(
  health_past3months ~ wave + stringency_index + covid_percent3 +
    (1 + wave | mergeid),
  family = binomial(),
  data = share
)


model3 <- glmmTMB(
  health_past3months ~ wave + age_dicho + gender + education2 + partnerinhh +
    covid_affected + stringency_index + covid_percent3 +
    (1 + wave | mergeid),
  family = binomial(),
  data = share
)

save(share, model1, model2, model3, file = "regression_analyses-unweighted.RData")





# survey models, w/o interaction ---------------------

des <- svydesign(
  ids = ~mergeid,
  strata = ~countries,
  weights = ~crossin_weights,
  data = share
)

model1s <- svyglm(
  health_past3months ~ wave + age_dicho + gender + education2 + partnerinhh +
    covid_affected,
  design = des,
  family = quasibinomial()
)

model2s <- svyglm(
  health_past3months ~ stringency_index + covid_percent3,
  design = des,
  family = quasibinomial()
)

model3s <- svyglm(
  health_past3months ~ wave + age_dicho + gender + education2 + partnerinhh +
    covid_affected + stringency_index + covid_percent3,
  design = des,
  family = quasibinomial()
)
