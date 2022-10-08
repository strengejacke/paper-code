library(easystats)
library(dplyr)

load("david_recodes.RData")
david$schwer_dement_d <- as.factor(as.numeric(david$schwer_dement2))
levels(david$schwer_dement_d) <- c("leichte/mittlere Demenz", "schwere Demenz")

# known group validity QUALIDEM mild to severe dementia ----------------

# dichotomized
d <- data_filter(david, schwer_dement_d == "leichte/mittlere Demenz")
d$known_group_pas <- categorize(d$pas_score)
d$known_group_barth <- categorize(d$barthtot_a)
d$known_group_cci <- categorize(d$cci)
d$known_group_age <- categorize(d$alter)

t.test(QD_total100 ~ known_group_age, d) |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ sex, d) |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ known_group_barth, d, alternative = "less") |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ known_group_pas, d, alternative = "greater") |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ known_group_cci, d, alternative = "greater") |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")


# known group validity QUALIDEM very severe dementia ----------------

d <- data_filter(david, schwer_dement_d == "schwere Demenz")
d$known_group_pas <- categorize(d$pas_score)
d$known_group_barth <- categorize(d$barthtot_a)
d$known_group_cci <- categorize(d$cci)
d$known_group_age <- categorize(d$alter)

t.test(QD_total100 ~ known_group_age, d) |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ sex, d) |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ known_group_barth, d, alternative = "less") |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ known_group_pas, d, alternative = "greater") |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")
t.test(QD_total100 ~ known_group_cci, d, alternative = "greater") |>
  model_parameters(effectsize_type = "d") |>
  print(layout = "vertical")

















# quantiles
d <- data_filter(david, schwer_dement_d == "leichte/mittlere Demenz")
d$known_group_pas <- categorize(d$pas_score, split = "quantile", n_groups = 3)
d$known_group_barth <- categorize(d$barthtot_a, split = "quantile", n_groups = 3)
d$known_group_cci <- categorize(d$cci, split = "quantile", n_groups = 3)
d$known_group_age <- categorize(d$alter, split = "quantile", n_groups = 3)

x <- data_filter(d, known_group_pas != 2)
t.test(QD_total100 ~ known_group_pas, x, alternative = "greater")

x <- data_filter(d, known_group_barth != 2)
t.test(QD_total100 ~ known_group_barth, x, alternative = "less")

x <- data_filter(d, known_group_cci != 2)
t.test(QD_total100 ~ known_group_cci, x, alternative = "greater")

x <- data_filter(d, known_group_age != 2)
t.test(QD_total100 ~ known_group_age, x)

t.test(QD_total100 ~ sex, x)



# flooring/ceiling QUALIDEM very severe dementia ----------------

d <- data_filter(david, schwer_dement_d == "schwere Demenz")
d$known_group_pas <- categorize(d$pas_score, split = "quantile", n_groups = 3)
d$known_group_barth <- categorize(d$barthtot_a, split = "quantile", n_groups = 3)
d$known_group_cci <- categorize(d$cci, split = "quantile", n_groups = 3)
d$known_group_age <- categorize(d$alter, split = "quantile", n_groups = 3)

x <- data_filter(d, known_group_pas != 2)
t.test(QD_total100 ~ known_group_pas, x, alternative = "greater")

x <- data_filter(d, known_group_barth != 2)
t.test(QD_total100 ~ known_group_barth, x, alternative = "less")

x <- data_filter(d, known_group_cci != 2)
t.test(QD_total100 ~ known_group_cci, x, alternative = "greater")

x <- data_filter(d, known_group_age != 2)
t.test(QD_total100 ~ known_group_age, x)

t.test(QD_total100 ~ sex, x)
