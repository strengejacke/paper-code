# Outcomes dichotomisieren 

nako$phq9_dicho <- nako$a_emo_phq9_cut10
nako$phq9_dicho <- assign_labels(
  nako$phq9_dicho,
  variable = "PHQ9, dichotomisiert",
  values = c("no depression", "depression")
)

nako$sni_dicho <- recode_values(
  as.factor(as.numeric(nako$a_sn_sni)),
  recode = list(`isolated` = 1, `not isolated` = 2:4)
)
nako$sni_dicho <- relevel(nako$sni_dicho, "not isolated")
nako$sni_dicho <- assign_labels(nako$sni_dicho, variable = "Social Isolation")

# "Umbenennen"
nako$SNI <- nako$a_sn_sni
nako$phq9_score <- nako$a_emo_phq9_sum + 1
nako$Alter <- nako$basis_age
nako$Aufenthaltsdauer <- standardize(nako$jahre_in_de)
nako$Bildung <- nako$education
nako$Geschlecht <- nako$basis_sex

# Lebensphase in 4 Gruppen
nako$Lebensphase <- nako$alter_immigr_gr
nako$Lebensphase[nako$Lebensphase == 5] <- 4
nako$Lebensphase <- to_factor(nako$Lebensphase)
