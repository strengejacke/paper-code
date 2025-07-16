# load packages -----------------------
library(easystats)


# load data -----------------------
d <- data_read("data/f23.0414.02_20240715.sav", convert_factors = FALSE)


# recodings -----------------------

# reverse items, to "never" corresponds to 0 and "often" to 4
d <- recode_values(
  d,
  select = c("f10_1", "f10_2", "f10_3", "f10_4", "f10_5"),
  recode = list(`0` = 5, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
  append = "r"
)

# Sum score for all items with *at least* 3 valid answers
d$diskr_amb_sum2 <- d |>
  data_select(c("f10_1r", "f10_2r", "f10_3r", "f10_4r", "f10_5r")) |>
  row_sums(min_valid = 3)

# for cases with 3 valid answers, the sum-score can only have a range
# from 0-12, so we rescale the score back to 0-12. therefore, find all
# cases with exact two missing values
missings_by_row <- d |>
  data_select(c("f10_1r", "f10_2r", "f10_3r", "f10_4r", "f10_5r")) |>
  row_count(count = NA)

# for two missing values in 5 items, we have three valid answers, i.e. a
# score ranging from 0 to 12.
d$diskr_amb_sum2[missings_by_row == 2] <- round(rescale(
  d$diskr_amb_sum2[missings_by_row == 2],
  to = c(0, 20),
  range = c(0, 12)
))

# same for cases with 4 valid answers (only one missing value)
d$diskr_amb_sum2[missings_by_row == 1] <- round(rescale(
  d$diskr_amb_sum2[missings_by_row == 1],
  to = c(0, 20),
  range = c(0, 16)
))

# for logistic regression, we use a dichotomized variable
d$diskr_amb_dicho <- as.factor(recode_into(
  diskr_amb_sum2 == 0 ~ 0,
  diskr_amb_sum2 > 0 ~ 1,
  default = NA,
  data = d
))

# make sure that we have categorical variables for our intersectional strata
d <- d |>
  to_factor(select = c("Aequi_terz", "ges", "MH_gen")) |>
  data_rename(c(income = "Aequi_terz", sex = "ges", migrant_background = "MH_gen"))

# clean factor levels a bit
levels(d$migrant_background) <- c("non-migrant", "1st gen.", "2nd gen.")
levels(d$income) <- c("low income", "intermediate income", "high income")
levels(d$sex) <- c("male", "female")

# set new reference levels
d$income <- relevel(d$income, ref = "high income")

# recode age into three groups
d$age3 <- recode_into(
  altq <= 40 ~ 1,
  altq > 40 & altq <= 60 ~ 2,
  altq > 60 ~ 3,
  default = NA,
  data = d
)
d$age3 <- factor(d$age3, labels = c("18-40", "41-60", ">60"))

# for plotting, we use a single variable as random effect (higher level),
# representing the interactions of the intersectional strata. We do this
# because it's a bit easier to create the axis labels
d$intersec <- d |>
  data_select(c("sex", "income", "migrant_background")) |>
  data_unite("intersec", separator = ", ") |>
  data_extract("intersec")

# Some of the three intersectionl strata have missing values, which are
# included as "NA" in the united variable element. Set these to missing.
d$intersec[grepl(", NA,", d$intersec, fixed = TRUE)] <- NA

save(d, file = "data/diskr.RData")
