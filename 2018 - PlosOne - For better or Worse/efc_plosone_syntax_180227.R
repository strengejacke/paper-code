# R Code for
#
# Lüdecke D, Bien B, McKee K, Krevers B, Mestheneos E, Di Rosa M, et al.
# For better or worse: Factors predicting outcomes of family care of older
# people over a one-year period. A six-country European study. PLoS ONE.
# 2018;13: e0195294. doi:10.1371/journal.pone.0195294


library(sjlabelled)
library(sjstats)
library(sjmisc)
library(sjPlot)
library(tidyverse)
library(lme4)

# load data ----

load("S1_Dataset.RData")


# labels for predictors -----

yax <- c(
  "Intercept",
  "Gender (female)",
  "Carer's age (standardized)",
  "Education (mid)",
  "Education (hi)",
  "Religious (quite)",
  "Religious (very)",
  "Dependency (high)",
  "Memory problems, but no dementia",
  "Dementia diagnosis",
  "Behavioural problems",
  "Hours of care per Week (standardized)",
  "Unmet needs (moderate)",
  "Unmet needs (high)",
  "Negative Impact (standardized)",
  "Positive Value (cstandardized)",
  "Relationship: children",
  "Relationship: children in-law",
  "Relationship: others"
)


efc_final$g2ctry <- to_label(efc_final$g2ctry)


# Model 1: Changed Status - care-recipient cared by different carer ----

fit1 <- glmer(
  cs_diffcarer ~
    c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
    c12hour_z + more_s_r + neg_c_7_z + pos_v_4_z + e15_r + (1 | g2ctry),
  data = efc_final,
  weights = adj_inv_weight,
  family = binomial("logit")
)


# Model 2: Changed Status - care-recipient in residential care ----

fit2 <- glmer(
  cs_inresidential ~
    c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
    c12hour_z + more_s_r + neg_c_7_z + pos_v_4_z + e15_r + (1 | g2ctry),
  data = efc_final,
  weights = adj_inv_weight,
  family = binomial("logit")
)


# Table odds ratios models 1 and 2 ----

tab_model(
  fit1, fit2,
  collapse.ci = T,
  pred.labels = yax,
  dv.labels = c(get_label(efc_final$cs_diffcarer), get_label(efc_final$cs_inresidential))
)


# NULL-models ----

fit1.null <- glmer(
  cs_diffcarer ~ 1 + (1 | g2ctry),
  data = fit1@frame,
  family = binomial("logit")
)

fit2.null <- glmer(
  cs_inresidential ~ 1 + (1 | g2ctry),
  data = fit2@frame,
  family = binomial("logit")
)


# Figure 1:	Random intercepts (group levels) of first model ----

plot_model(
  fit1,
  type = "re",
  show.values = T,
  sort.est = T,
  grid = F,
  value.offset = .3,
  grid.breaks = c(.25, .5, 1, 2, 4),
  vline.color = "grey90"
) +
  theme_sjplot2() +
  labs(title = NULL)

ggsave("Fig1.tiff", width = 7, height = 4, units = "in", dpi = 300)


# Figure 2:	Random intercepts (group levels) of second model ----

plot_model(
  fit2,
  type = "re",
  show.values = T,
  sort.est = T,
  grid = F,
  value.offset = .3,
  grid.breaks = c(.01, .02, .05, .1, .2, .5, 1, 2, 5, 10, 20),
  vline.color = "grey90"
) +
  theme_sjplot2() +
  labs(title = NULL)

ggsave("Fig2.tiff", width = 7, height = 4, units = "in", dpi = 300)


# Interaction between hours of care and relationship - model 3 ----

fit3 <- glmer(
  cs_diffcarer ~
    c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
    more_s_r + neg_c_7_z + pos_v_4_z + e15_r * c12hour_z + (1 | g2ctry),
  data = efc_final,
  weights = adj_inv_weight,
  family = binomial("logit")
)


# Table of model with interaction ----

tab_model(fit3, collapse.ci = T)


# Figure 3: Interaction between hours of care and relationship ----

efc_final$e15_r <- set_label(efc_final$e15_r, label = "Relationship")

## Use non-standardized c12hour here, so x-axis in plot match range of variable

fit3_plot <- glmer(
  cs_diffcarer ~
    c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
    more_s_r + neg_c_7_z + pos_v_4_z + e15_r * c12hour + (1 | g2ctry),
  data = efc_final,
  weights = adj_inv_weight,
  family = binomial("logit")
)

# check convergence
converge_ok(fit3_plot)

plot_model(
  fit3_plot,
  type = "eff",
  terms = c("c12hour", "e15_r"),
  ci.lvl = NA,
  axis.title = c("Hours of Care", "Predicted Probabilities for Changed Status"),
  title = ""
) +
  theme_sjplot2() +
  xlim(c(0, 120))

ggsave("Fig3.tiff", width = 7.5, height = 5, units = "in", dpi = 300)
