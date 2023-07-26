library(easystats)
library(ggeffects)

load("./regression_analyses_interactions.RData")
load("./predictions_for_plots.RData")

ratio <- function(x, lo = 1, hi = 2) {
  x[hi] / x[lo]
}


# change in relative risk for age
pv1 |>
  lapply(format_value, as_percent = TRUE) |>
  as.data.frame() |>
  data_arrange("group") |>
  data_select(-std.error) |>
  format_table() |>
  export_table()

pv1 |>
  as.data.frame() |>
  data_group("group") |>
  data_modify(relrisk = ratio(predicted)) |>
  data_select(c("group", "relrisk")) |>
  data_unique()
rr_p1 <- hypothesis_test(pv1, test = "(b3-b1) = (b4-b2)")
rr_p1


# change in relative risk for female sex
pv2 |>
  lapply(format_value, as_percent = TRUE) |>
  as.data.frame() |>
  data_arrange("group") |>
  data_select(-std.error) |>
  format_table() |>
  export_table()

pv2 |>
  as.data.frame() |>
  data_group("group") |>
  data_modify(relrisk = ratio(predicted)) |>
  data_select(c("group", "relrisk")) |>
  data_unique()
rr_p2 <- hypothesis_test(pv2, test = "(b3-b1) = (b4-b2)")
rr_p2


# change in relative risk for education
pv3 |>
  lapply(format_value, as_percent = TRUE) |>
  as.data.frame() |>
  data_arrange("group") |>
  data_select(-std.error) |>
  format_table() |>
  export_table()

pv3 |>
  as.data.frame() |>
  data_group("group") |>
  data_modify(relrisk = ratio(predicted, lo = 3, hi = 1)) |>
  data_select(c("group", "relrisk")) |>
  data_unique()
rr_p3 <- hypothesis_test(pv3, test = "(b5-b1) = (b6-b2)")
rr_p3


# change in relative risk for living with partner
pv4 |>
  lapply(format_value, as_percent = TRUE) |>
  as.data.frame() |>
  data_arrange("group") |>
  data_select(-std.error) |>
  format_table() |>
  export_table()

pv4 |>
  as.data.frame() |>
  data_group("group") |>
  data_modify(relrisk = ratio(predicted)) |>
  data_select(c("group", "relrisk")) |>
  data_unique()
rr_p4 <- hypothesis_test(pv4, test = "(b3-b1) = (b4-b2)")
rr_p4


# change in relative risk for covid affected
pv5 |>
  lapply(format_value, as_percent = TRUE) |>
  as.data.frame() |>
  data_arrange("group") |>
  data_select(-std.error) |>
  format_table() |>
  export_table()

pv5 |>
  as.data.frame() |>
  data_group("group") |>
  data_modify(relrisk = ratio(predicted, lo = 1, hi = 4)) |>
  data_select(c("group", "relrisk")) |>
  data_unique()
rr_p5 <- hypothesis_test(pv5, test = "(b7-b1) = (b8-b2)")
rr_p5
