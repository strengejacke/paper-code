library(easystats)
load("Daten/share_8_9.RData")

share <- data_to_factor(share, select = c("health_past3months", "wave", "gender", "covid_affected", "partnerinhh", "covid_regime_si3", "covid_regime_ch3"))
share$stringency_index <- share$global_covid_regime_si3

share <- share |>
  data_group("wave") |>
  categorize(select = "covid_percent", split = "quantile", n_groups = 3, append = TRUE)

share$covid_percent3 <- factor(share$covid_percent_r, labels = c("lower tertile", "middle tertile", "upper tertile"))

share |>
  data_filter(wave == 8) |>
  report_sample(
    select = c("age", "gender", "education2", "partnerinhh"),
    digits = 1
  ) |>
  # data_rotate(rownames = "row", colnames = TRUE) |>
  export_table(format = "html")


share |>
  report_sample(
    group_by = "wave",
    select = c("health_past3months", "covid_affected", "stringency_index", "covid_percent3"),
    digits = 1
  ) |>
  # data_rotate(rownames = "row", colnames = TRUE) |>
  export_table(format = "html")
