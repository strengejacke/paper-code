library(tidyverse)
library(sjmisc)
library(sjPlot)
library(parameters)
library(lme4)


# Daten laden -------------------------

load("DEAS 2008-2017, Dropoff, Variablenauswahl.RData")
d <- sjmisc::to_label(deas_gesamt_complete, erw, migrat_r, isced, gender, ehramt_weit, partner)



# SF-36 Basismodell (ohne Interaktion) -------------------------

model1 <- lmer(
  sf36 ~ welle + isced + aee_oecd_between_z2 + lone6_between_z2 + migrat_r +
    gender + alter_aktuell_z2 + srh_between_z2 + srh_within_z2 +
    aee_oecd_within_z2 + lone6_within_z2 + partner + ehramt_weit +
    (1 + welle || fallnum) + (1 | bland) + (1 | bbsr_kreistyp),
  data = d,
  weights = drop_weight
)

# print(model_parameters(model1), select = c("Parameter", "Coefficient", "CI_low", "CI_high", "p"))



# Optimismus Basismodell (ohne Interaktion) -------------------------

model2 <- lmer(
  optimismus100 ~ welle + isced + aee_oecd_between_z2 + lone6_between_z2 + migrat_r +
    gender + alter_aktuell_z2 + srh_between_z2 + srh_within_z2 +
    aee_oecd_within_z2 + lone6_within_z2 + partner + ehramt_weit +
    (1 + welle || fallnum) + (1 | bland) + (1 | bbsr_kreistyp),
  data = d,
  weights = drop_weight
)

# print(model_parameters(model2), select = c("Parameter", "Coefficient", "CI_low", "CI_high", "p"))



# SF-36 mit Interaktion -------------------------

model3 <- lmer(
  sf36 ~ welle * isced + welle * aee_oecd_between_z2 + welle * lone6_between_z2 + migrat_r +
    gender + alter_aktuell_z2 + srh_between_z2 + srh_within_z2 +
    aee_oecd_within_z2 + lone6_within_z2 + partner + ehramt_weit +
    (1 + welle || fallnum) + (1 | bland) + (1 | bbsr_kreistyp),
  data = d,
  weights = drop_weight
)

# print(model_parameters(model3), select = c("Parameter", "Coefficient", "CI_low", "CI_high", "p"))



# Optimismus mit Interaktion -------------------------

model4 <- lmer(
  optimismus100 ~ welle * isced + welle * aee_oecd_between_z2 + welle * lone6_between_z2 + migrat_r +
    gender + alter_aktuell_z2 + srh_between_z2 + srh_within_z2 +
    aee_oecd_within_z2 + lone6_within_z2 + partner + ehramt_weit +
    (1 + welle || fallnum) + (1 | bland) + (1 | bbsr_kreistyp),
  data = d,
  weights = drop_weight
)

# print(model_parameters(model4), select = c("Parameter", "Coefficient", "CI_low", "CI_high", "p"))



# Tabellen

tab_model(model1, model2, show.re.var = FALSE, show.r2 = FALSE, show.icc = FALSE)
tab_model(model3, model4, show.re.var = FALSE, show.r2 = FALSE, show.icc = FALSE)
