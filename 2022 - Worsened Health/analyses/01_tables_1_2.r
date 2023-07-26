library(easystats)

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

share <- to_factor(
  share,
  select = c("health_past3months", "wave", "gender", "covid_affected",
  "partnerinhh", "covid_regime_si3", "covid_regime_ch3")
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



# Tabelle: Sample description by country, more depressed ------------------------


## low Stringency countries -----------------

share_low_si <- data_filter(share, !is.na(health_past3months) & stringency_index == "low")
share_low_si$countries <- droplevels(share_low_si$countries)

### Wave 8

share_low_si |>
  data_filter(wave == "8") |>
  report_sample(
    group_by = "countries",
    select = c("covid_affected", "health_past3months"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_rename(
    pattern = c("covid_affected [never], %", "covid_affected [tested positive, no symptoms], %",
                "covid_affected [symptoms], %", "covid_affected [hospitalized], %",
                "health_past3months [worsened], %"),
    replacement = c("Never", "Tested positiv (no symptomps)", "Symptoms",
                  "Hospitalized", "Worsened health, %")
  ) |>
  export_table(format = "html")

### Wave 9

share_low_si |>
  data_filter(wave == "9") |>
  report_sample(
    group_by = "countries",
    select = c("covid_affected", "health_past3months"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_rename(
    pattern = c("covid_affected [never], %", "covid_affected [tested positive, no symptoms], %",
                "covid_affected [symptoms], %", "covid_affected [hospitalized], %",
                "health_past3months [worsened], %"),
    replacement = c("Never", "Tested positiv (no symptomps)", "Symptoms",
                    "Hospitalized", "Worsened health, %")
  ) |>
  export_table(format = "html")

### Social factors only for initial wave (time-invariant characteristics)

share_low_si |>
  data_filter(wave == "8") |>
  report_sample(
    group_by = "countries",
    select = c("age_dicho", "education2", "gender", "partnerinhh"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_select(exclude = c("education2 [low], %", "education2 [mid], %")) |>
  data_rename(
    pattern = c("age_dicho [70+], %", "education2 [high], %", "gender [Female], %", "partnerinhh [No], %"),
    replacement = c("Aged 70+, %", "High education, %", "Female gender, %", "Partner in household, %")
  ) |>
  export_table(format = "html")



## middle Stringency countries -----------------

share_middle_si <- data_filter(share, !is.na(health_past3months) & stringency_index == "middle")
share_middle_si$countries <- droplevels(share_middle_si$countries)

### Wave 8

share_middle_si |>
  data_filter(wave == "8") |>
  report_sample(
    group_by = "countries",
    select = c("covid_affected", "health_past3months"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_rename(
    pattern = c("covid_affected [never], %", "covid_affected [tested positive, no symptoms], %",
                "covid_affected [symptoms], %", "covid_affected [hospitalized], %",
                "health_past3months [worsened], %"),
    replacement = c("Never", "Tested positiv (no symptomps)", "Symptoms",
                    "Hospitalized", "Worsened health, %")
  ) |>
  export_table(format = "html")

### Wave 9

share_middle_si |>
  data_filter(wave == "9") |>
  report_sample(
    group_by = "countries",
    select = c("covid_affected", "health_past3months"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_rename(
    pattern = c("covid_affected [never], %", "covid_affected [tested positive, no symptoms], %",
                "covid_affected [symptoms], %", "covid_affected [hospitalized], %",
                "health_past3months [worsened], %"),
    replacement = c("Never", "Tested positiv (no symptomps)", "Symptoms",
                    "Hospitalized", "Worsened health, %")
  ) |>
  export_table(format = "html")

### Social factors only for initial wave (time-invariant characteristics)

share_middle_si |>
  data_filter(wave == "8") |>
  report_sample(
    group_by = "countries",
    select = c("age_dicho", "education2", "gender", "partnerinhh"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_select(exclude = c("education2 [low], %", "education2 [mid], %")) |>
  data_rename(
    pattern = c("age_dicho [70+], %", "education2 [high], %", "gender [Female], %", "partnerinhh [No], %"),
    replacement = c("Aged 70+, %", "High education, %", "Female gender, %", "Partner in household, %")
  ) |>
  export_table(format = "html")



## high Stringency countries -----------------

share_high_si <- data_filter(share, !is.na(health_past3months) & stringency_index == "high")
share_high_si$countries <- droplevels(share_high_si$countries)

### Wave 8

share_high_si |>
  data_filter(wave == "8") |>
  report_sample(
    group_by = "countries",
    select = c("covid_affected", "health_past3months"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_rename(
    pattern = c("covid_affected [never], %", "covid_affected [tested positive, no symptoms], %",
                "covid_affected [symptoms], %", "covid_affected [hospitalized], %",
                "health_past3months [worsened], %"),
    replacement = c("Never", "Tested positiv (no symptomps)", "Symptoms",
                    "Hospitalized", "Worsened health, %")
  ) |>
  export_table(format = "html")

### Wave 9

share_high_si |>
  data_filter(wave == "9") |>
  report_sample(
    group_by = "countries",
    select = c("covid_affected", "health_past3months"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_rename(
    pattern = c("covid_affected [never], %", "covid_affected [tested positive, no symptoms], %",
                "covid_affected [symptoms], %", "covid_affected [hospitalized], %",
                "health_past3months [worsened], %"),
    replacement = c("Never", "Tested positiv (no symptomps)", "Symptoms",
                    "Hospitalized", "Worsened health, %")
  ) |>
  export_table(format = "html")

### Social factors only for initial wave (time-invariant characteristics)

share_high_si |>
  data_filter(wave == "8") |>
  report_sample(
    group_by = "countries",
    select = c("age_dicho", "education2", "gender", "partnerinhh"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_select(exclude = c("education2 [low], %", "education2 [mid], %")) |>
  data_rename(
    pattern = c("age_dicho [70+], %", "education2 [high], %", "gender [Female], %", "partnerinhh [No], %"),
    replacement = c("Aged 70+, %", "High education, %", "Female gender, %", "Partner in household, %")
  ) |>
  export_table(format = "html")



## Overall mean of Social factors

share_all <- data_filter(share, !is.na(health_past3months))

### Wave 8 only, for age, education etc.

share_all |>
  data_filter(wave == "8") |>
  report_sample(
    select = c("age_dicho", "education2", "gender", "partnerinhh"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_select(exclude = c("education2 [low], %", "education2 [mid], %")) |>
  data_rename(
    pattern = c("age_dicho [70+], %", "education2 [high], %", "gender [Female], %", "partnerinhh [No], %"),
    replacement = c("Aged 70+, %", "High education, %", "Female gender, %", "Partner in household, %")
  ) |>
  export_table(format = "html")

mean(share_all$age[share_all$wave == "8"], na.rm = TRUE)

share_all |>
  report_sample(
    group_by = "wave",
    select = c("covid_affected", "health_past3months"),
    digits = 1
  ) |>
  data_rotate(rownames = "Country", colnames = TRUE) |>
  data_rename(
    pattern = c("covid_affected [never], %", "covid_affected [tested positive, no symptoms], %",
                "covid_affected [symptoms], %", "covid_affected [hospitalized], %",
                "health_past3months [worsened], %"),
    replacement = c("Never", "Tested positiv (no symptomps)", "Symptoms",
                    "Hospitalized", "Worsened health, %")
  ) |>
  export_table(format = "html")
