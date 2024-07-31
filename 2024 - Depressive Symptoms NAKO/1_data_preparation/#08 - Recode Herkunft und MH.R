nako$aussiedler_region <- dplyr::case_when(
  nako$aussiedler_2 == "Ausländer" & nako$geb_region_proband == "Osteuropa" ~ "non_german_easteurope",
  nako$aussiedler_2 == "Ausländer" & nako$geb_region_proband == "Südeuropa" ~ "non_german_southeurope",
  nako$aussiedler_2 == "Ausländer" & nako$geb_region_proband == "Westasien" ~ "non_german_westasia",
  nako$aussiedler_2 == "Eingebürgert" & nako$geb_region_proband == "Südeuropa" ~ "naturalisation_southeurope",
  nako$aussiedler_2 == "Eingebürgert" & nako$geb_region_proband == "Westasien" ~ "naturalisation_westasia",
  nako$aussiedler_2 == "Eingebürgert" & nako$geb_region_proband == "Osteuropa" ~ "naturalisation_easteurope",
  nako$aussiedler_2 == "Nachkommen (2. Gen)" & nako$geb_region_vater == "Osteuropa" ~ "descendents_easteurope",
  nako$aussiedler_2 == "Nachkommen (2. Gen)" & nako$geb_region_vater == "Südeuropa" ~ "descendents_southeurope",
  nako$aussiedler_2 == "Nachkommen (2. Gen)" & nako$geb_region_vater == "Westasien" ~ "descendents_westasia",
  nako$aussiedler_2 == "Aussiedler" ~ "resettler",
  nako$aussiedler_2 == "Ohne MH" ~ "non-migrant population",
  TRUE ~ NA_character_
)

nako$aussiedler_region2 <- dplyr::case_when(
  nako$aussiedler_2 == "Ausländer" & nako$geb_region_proband == "Osteuropa" ~ "non_german_easteurope",
  nako$aussiedler_2 == "Ausländer" & nako$geb_region_proband == "Südeuropa" ~ "non_german_southeurope",
  nako$aussiedler_2 == "Ausländer" & nako$geb_region_proband == "Westasien" ~ "non_german_westasia",
  nako$aussiedler_2 == "Eingebürgert" & nako$geb_region_proband == "Südeuropa" ~ "naturalisation_southeurope",
  nako$aussiedler_2 == "Eingebürgert" & nako$geb_region_proband == "Westasien" ~ "naturalisation_westasia",
  nako$aussiedler_2 == "Eingebürgert" & nako$geb_region_proband == "Osteuropa" ~ "naturalisation_easteurope",
  nako$aussiedler_2 == "Aussiedler" & nako$geb_region_proband == "Zentralasien" ~ "resettler_centralasia",
  nako$aussiedler_2 == "Aussiedler" & nako$geb_region_proband == "Osteuropa" ~ "resettler_easteurope",
  nako$aussiedler_2 == "Ohne MH" ~ "non-migrant population",
  TRUE ~ NA_character_
)

nako$aussiedler_region <- as.factor(nako$aussiedler_region)
nako$aussiedler_region <- relevel(nako$aussiedler_region, "non-migrant population")

nako$aussiedler_region2 <- as.factor(nako$aussiedler_region2)
nako$aussiedler_region2 <- relevel(nako$aussiedler_region2, "non-migrant population")

