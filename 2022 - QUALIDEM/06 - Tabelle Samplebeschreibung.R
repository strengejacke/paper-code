library(sjmisc)
library(sjstats)
library(dplyr)
library(sjPlot)
library(purrr)
library(datawizard)

load("david_recodes.RData")
david$schwer_dement_d <- as.factor(as.numeric(david$schwer_dement2))
levels(david$schwer_dement_d) <- c("leichte/mittlere Demenz", "schwere Demenz")

# by groups -------

david |>
  to_factor(dementia_mmse) %>%
  mutate(
    sex_num = recode_to(sex),
    phys_res = recode_to(Fixierung_d),
    drug_use = recode_to(bm_allg_sedierung)
  ) %>%
  group_by(schwer_dement_d) %>%
  summarise(
    Perc_Female = 100 * mean(sex_num),
    Mean_Age = mean(alter),
    SD_Age = sd(alter),
    Mean_Barthel = mean(barthtot_a),
    SD_Barthel = sd(barthtot_a),
    Mean_PAS = mean(pas_score),
    SD_PAS = sd(pas_score),
    Mean_CCI = mean(cci),
    SD_CCI = sd(cci),
    Mean_Stay = mean(Dauer),
    SD_Stay = sd(Dauer),
    Mean_QoL = mean(QD_alle_100),
    SD_QoL = sd(QD_alle_100),
    N = n()
  ) %>%
  map_if(is.numeric, ~ round(.x, 1)) %>%
  rotate_df() %>%
  tab_df()


# total -------

david %>%
  mutate(
    sex_num = recode_to(sex),
    phys_res = recode_to(Fixierung_d),
    drug_use = recode_to(bm_allg_sedierung)
  ) %>%
  summarise(
    Perc_Female = 100 * mean(sex_num),
    Mean_Age = mean(alter),
    SD_Age = sd(alter),
    Mean_Barthel = mean(barthtot_a),
    SD_Barthel = sd(barthtot_a),
    Mean_PAS = mean(pas_score),
    SD_PAS = sd(pas_score),
    Mean_CCI = mean(cci),
    SD_CCI = sd(cci),
    Mean_Stay = mean(Dauer),
    SD_Stay = sd(Dauer),
    Mean_QoL = mean(QD_alle_100),
    SD_QoL = sd(QD_alle_100),
    N = n()
  ) %>%
  map_if(is.numeric, ~ round(.x, 1)) %>%
  rotate_df() %>%
  tab_df()



# P-Werte ----

chisq.test(david$sex, david$schwer_dement_d)
t.test(alter ~ schwer_dement_d, david)
mwu(david, barthtot_a, schwer_dement_d)
mwu(david, pas_score, schwer_dement_d)
mwu(david, cci, schwer_dement_d)
t.test(QD_alle_100 ~ schwer_dement_d, david)
table(david$schwer_dement_d)
