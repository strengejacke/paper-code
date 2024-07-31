library(easystats)
load("nako.RData")

nako <- nako |>
  data_rename(
    pattern = c(
      "a_sn_sni", "d_sn7", "d_sn8", "a_ses_househ", "a_ses_famst",
      "basis_age", "basis_sex", "a_ses_incpos",
      "a_ses_isced97_cat", "mig_status3", "a_emo_phq9_sum",
      "a_emo_phq9_cut10", "a_ses_bedarfgw",
      "a_ses_beruf", "a_ses_ewstat", "aussiedler_2"
    ),
    replacement = c(
      "sni", "inf_supp", "emo_supp", "hh_size", "fam_status",
      "age", "gender", "income_pos", "education", "migstatus",
      "phq9_score", "phq9_dicho", "ses_weight", "jobtype", "employment",
      "resettler"
    )
  )

nako$education <- labels_to_levels(nako$education)
nako$sni_r <- reverse_scale(nako$sni)
levels(nako$phq9_dicho) <- c("no depression", "depressive symptoms")

data_tabulate(
  nako,
  select = c("education", "gender", "resettler"),
  by = "phq9_dicho",
  proportions = "row",
  include_na = FALSE
)

data_tabulate(
  nako,
  select = c("education", "gender", "resettler", "age3"),
  by = "phq9_dicho",
  proportions = "row",
  include_na = TRUE
)

# missing values in PHQ9
data_tabulate(nako$phq9_dicho, remove_na = FALSE)
data_tabulate(nako$education, remove_na = FALSE)

# nako-sample nur wo phq9 vollständig ist
nako <- data_filter(nako, !is.na(phq9_dicho))

# Häusigkeiten im Gesamtsample
data_tabulate(nako$phq9_dicho)
data_tabulate(nako$education)
data_tabulate(nako$gender)
data_tabulate(nako$resettler)
data_tabulate(nako$age3)

out <- rbind(
  report_sample(
    nako,
    select = "phq9_dicho",
    by = "education",
    ci = 0.95
  ) |>
    data_rename("Variable", "Education", "") |>
    data_transpose(),
  report_sample(
    nako,
    select = "phq9_dicho",
    by = "gender",
    ci = 0.95
  ) |>
    data_rename("Variable", "Gender") |>
    data_transpose(),
  report_sample(
    nako,
    select = "phq9_dicho",
    by = "resettler",
    ci = 0.95
  ) |>
    data_rename("Variable", "Resettler") |>
    data_transpose(),
  report_sample(
    nako,
    select = "phq9_dicho",
    by = "sni_dicho",
    ci = 0.95
  ) |>
    data_rename("Variable", "SNI") |>
    data_transpose(),
  report_sample(
    nako,
    select = "phq9_dicho",
    by = "age3",
    ci = 0.95
  ) |>
    data_rename("Variable", "Age") |>
    data_transpose()
) |>
  rownames_as_column(var = "Characteristic")

out[["1"]] <- gsub("[", "(", out[["1"]], fixed = TRUE)
out[["1"]] <- gsub("]", ")", out[["1"]], fixed = TRUE)
out[["1"]] <- gsub("phq9_dicho (depressive symptoms), %", "", out[["1"]], fixed = TRUE)

print_html(data_rename(
  out,
  c("Characteristic", "1"),
  c(" ", "PHQ-9 >= 10 (depressive Symptoms), %")
))

chisq.test(nako$education, nako$phq9_dicho)
chisq.test(nako$gender, nako$phq9_dicho)
chisq.test(nako$resettler, nako$phq9_dicho)

sjstats::crosstable_statistics(nako, phq9_dicho, education)
sjstats::crosstable_statistics(nako, phq9_dicho, gender)
sjstats::crosstable_statistics(nako, phq9_dicho, resettler)
sjstats::crosstable_statistics(nako, phq9_dicho, sni_dicho)
sjstats::crosstable_statistics(nako, phq9_dicho, age3)







# total
describe_distribution(nako$phq9_score)

# gender
data_tabulate(nako$gender)
means_by_group(
  nako,
  "phq9_score",
  "gender"
)
sjstats::mannwhitney(nako, phq9_score, gender)

# migration
data_tabulate(nako$resettler)
means_by_group(
  nako,
  "phq9_score",
  "resettler"
)
sjstats::mannwhitney(nako, phq9_score, resettler)

# education
data_tabulate(nako$education)
means_by_group(
  nako,
  "phq9_score",
  "education"
)
sjstats::mannwhitney(nako, phq9_score, education)

# age grpups
data_tabulate(nako$age3)
means_by_group(
  nako,
  "phq9_score",
  "age3"
)
sjstats::mannwhitney(nako, phq9_score, age3)

# sni
data_tabulate(nako$sni_dicho)
means_by_group(
  nako,
  "phq9_score",
  "sni_dicho"
)
