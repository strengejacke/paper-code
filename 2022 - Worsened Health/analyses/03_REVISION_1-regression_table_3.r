library(easystats)
library(glmmTMB)

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

share$living_alone <- ifelse(share$hhsize > 1, 0, 1)
share$living_alone <- factor(share$living_alone, labels = c("no", "yes"))

## create categorical variables -------------------

share <- to_factor(
  share,
  select = c(
    "wave", "gender", "covid_affected", "partnerinhh",
    "covid_regime_si3", "covid_regime_ch3", "living_alone"
  )
)
share$stringency_index <- share$global_covid_regime_si3


## create frequency weights ----------------

share <- rescale_weights(share, group = "countries", probability_weights = "crossin_weights")


## recode variables ----------------

share$age_dicho <- factor(categorize(share$age), labels = c("50-69", "70+"))
share <- share |>
  data_group("wave") |>
  categorize(select = "covid_percent", split = "quantile", n_groups = 3, append = TRUE)

share$covid_percent3 <- factor(share$covid_percent_r, labels = c("lower tertile", "middle tertile", "upper tertile"))



# mixed models, w/o interaction ---------------------

model1_new <- glmmTMB(
  health_past3months ~ wave + age_dicho + gender + education2 + living_alone +
    covid_affected + (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

model1_old <- glmmTMB(
  health_past3months ~ wave + age_dicho + gender + education2 + partnerinhh +
    covid_affected + (1 + wave | countries),
  family = binomial(),
  data = share,
  weights = pweights_a
)

compare_parameters(model1_new, model1_old, exponentiate = TRUE)
