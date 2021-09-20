library(dplyr)
library(strengejacke)
library(easystats)
library(glmmTMB)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(survey)

load("share.RData")

share <- share %>%
  to_label(eurodcat, age_dicho, living, gender, make_ends_meet, education,
           migrant, migrant_western, country, chron_cond, gali, iadl_d,
           sphus_d, limited_adl_d, eurodcat)

d <- share %>%
  dplyr::filter(!is.na(crossin_weights)) %>%
  select(eurodcat, wave, migrant, age_between, gender, living, education, hhsize,
         age_within, mergeid, crossin_weights, country, sphus, chron_n, chron_cond,
         gali, iadl, limited_adl, limited_adl_d, iadl_d, sphus_d, age_dicho, eurodcat)

d$Age <- d$age_between

design <- svydesign(
  ids = ~ mergeid,
  weights = ~ crossin_weights,
  data = d,
  strata = ~ country,
  nest = TRUE
)




# functions ----------------------


trends <- function(model) {
  ## estimates trends

  emm <- emtrends(model,
                  c("migrant", "Age", "gender"),
                  var = "wave",
                  at = list(Age = c(-1, 1)),
                  adjust = "none")


  ## significant trend per group

  model_parameters(emm) %>%
    rec(Age, rec = c("-1=low;1=high"), suffix = "") |>
    filter(p < 0.05) |>
    select(-Coefficient, -SE, -CI_low, -CI_high, -z) |>
    print(caption = "Are the trends significant?")

  cat("\n\n")

  ## Are migrants different from non-migrants?

  contrast(emm, method = "pairwise", by = c("Age", "gender"), adjust = "none") |>
    model_parameters() |>
    rec(Age, rec = c("-1=low;1=high"), suffix = "") |>
    # filter(p < 0.05) |>
    select(-Coefficient, -SE, -CI_low, -CI_high, -z) |>
    print(caption = "Are migrants different from non-migrants?")
}





# Modelle rechnen -----------------------

f1 <- as.formula("gali ~ wave * migrant * Age * gender + education + hhsize + age_within")
m1 <- svyglm(f1, family = quasibinomial(), design = design)

f5 <- as.formula("sphus_d ~ wave * migrant * Age * gender + education + hhsize + age_within")
m5 <- svyglm(f5, family = quasibinomial(), design = design)

f6 <- as.formula("eurodcat ~ wave * migrant * Age * gender + education + hhsize + age_within")
m6 <- svyglm(f6, family = quasibinomial(), design = design)





#  Global Activity Limitation Index (GALI) ----------------------

trends(m1)

#  Depression ----------------------

trends(m6)

#  SPH ----------------------

trends(m5)
