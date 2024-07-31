library(easystats)

d <- nako

d[] <- lapply(d, function(i) {
  if (is.factor(i)) {
    levels(i) <- gsub("'", "", levels(i) , fixed = TRUE)
  } else if (is.character(i)) {
    i <- gsub("'", "", i , fixed = TRUE)
  }
  i
})

# data_tabulate(d, c("mig_staat"))

d$geburtsland_DE_proband <- d$d_se_n1
levels(d$geburtsland_DE_proband) <-  c("DE", "woanders")

d$geburtsland_DE_vater <- d$d_se_bf3
levels(d$geburtsland_DE_vater) <-  c("DE", "woanders", "unbekannt")

d$geburtsland_DE_mutter <- d$d_se_bf6
levels(d$geburtsland_DE_mutter) <-  c("DE", "woanders", "unbekannt")

d$de_pass <- d$d_se_n5_1
d$geburtsland_proband <- d$d_se_n1a
d$geburtsland_vater <- d$d_se_bf3a
d$geburtsland_mutter <- d$d_se_bf6a

d <- to_factor(
  d,
  c("de_pass", "geburtsland_proband", "geburtsland_vater", "geburtsland_mutter"),
  labels_to_levels = TRUE
)

sowjetunion <- c("Russland", "Weißrussland", "Kasachstan", "Ukraine", "Usbekistan",
                 "Georgien", "Aserbaidschan", "Litauen", "Lettland", "Kirgisistan",
                 "Tadschikistan", "Armenien", "Turkmenistan", "Estland")

d$aussiedler <- recode_into(
  # ohne MH
  de_pass == "Ja" & geburtsland_DE_proband == "DE" & geburtsland_DE_vater != "woanders" & geburtsland_DE_mutter != "woanders" ~ 1,
  # ohne MH, Vertriebene
  de_pass == "Ja" & geburtsland_DE_proband == "DE" & (geburtsland_vater %in% c("Polen", "Russland", "Tschechien") | geburtsland_mutter %in% c("Polen", "Russland", "Tschechien")) ~ 2,
  # mit MH, Nachkommen
  de_pass == "Ja" & geburtsland_DE_proband == "DE" & ((geburtsland_DE_vater == "woanders" & !geburtsland_vater %in% c("Polen", "Russland", "Tschechien")) | (geburtsland_DE_mutter == "woanders" & !geburtsland_mutter %in% c("Polen", "Russland", "Tschechien"))) ~ 3,
  # ohne MH, im Ausland geboren
  de_pass == "Ja" & geburtsland_DE_proband == "woanders" & !geburtsland_proband %in% c("Russland", "Weißrussland", "Kasachstan", "Ukraine") & geburtsland_DE_vater == "DE" & geburtsland_DE_mutter == "DE" ~ 4,
  # mit MH, eingebürgert
  de_pass == "Ja" & geburtsland_DE_proband == "woanders" & !geburtsland_proband %in% c("Russland", "Weißrussland", "Kasachstan", "Ukraine") & (geburtsland_DE_vater != "DE" | geburtsland_DE_mutter != "DE") ~ 5,
  # mit MH, Aussiedler
  de_pass == "Ja" & geburtsland_DE_proband == "woanders" & geburtsland_proband %in% c("Russland", "Weißrussland", "Kasachstan", "Ukraine") & geburtsland_vater %in% sowjetunion & geburtsland_mutter %in% sowjetunion ~ 6,
  # Ausländer
  de_pass == "Nein" ~ 7,
  default = as.numeric(NA),
  data = d
)

d$aussiedler <- as.factor(d$aussiedler)
levels(d$aussiedler) <- c("1. PoM", "2. PoM (Vertriebene)", "3. PmM (Nachkommen)", "4. PoM (Ausland geboren)", "5. PmM (eingebürgert)", "6. PmM (Aussiedler)", "7. PmM (Ausländer)")

nako <- data_merge(
  nako,
  d[c("ID", "geburtsland_DE_proband", "geburtsland_DE_vater", "geburtsland_vater",
      "geburtsland_DE_mutter", "geburtsland_mutter",
      "de_pass", "geburtsland_proband", "aussiedler")]
  )

nako$aussiedler_2 <- recode_values(
  nako$aussiedler,
  recode = list(`Ohne MH` = c("1. PoM", "2. PoM (Vertriebene)", "4. PoM (Ausland geboren)"),
                `Ausländer` = "7. PmM (Ausländer)",
                `Eingebürgert` = "5. PmM (eingebürgert)",
                `Nachkommen (2. Gen)` = "3. PmM (Nachkommen)",
                `Aussiedler` = "6. PmM (Aussiedler)"),
  preserve_na = TRUE
)

nako$aussiedler_2 <- factor(nako$aussiedler_2, levels = c("Ohne MH", "Ausländer", "Eingebürgert", "Nachkommen (2. Gen)", "Aussiedler"))

nako$mig_status3 <- recode_values(
  nako$aussiedler_2,
  recode = list(
    `1` = "Ohne MH",
    `2` = c("Ausländer", "Eingebürgert", "Aussiedler"),
    `3` = "Nachkommen (2. Gen)"
  )
)
nako$mig_status3 <- as.factor(nako$mig_status3)
levels(nako$mig_status3) <- c("Ohne MH", "1. Generation", "2. Generation")

# migrationshintergrund / generation / mit staatsangehörigkeit ------------------

nako <- data_modify(nako, mig_staat = recode_into(
  mig_status3 == "Ohne MH" & d_se_n5_1 == 1 ~ 0,
  mig_status3 == "Ohne MH" & d_se_n5_1 == 2 ~ 1,
  mig_status3 == "1. Generation" & d_se_n5_1 == 1 ~ 2,
  mig_status3 == "1. Generation" & d_se_n5_1 == 2 ~ 3,
  mig_status3 == "2. Generation" & d_se_n5_1 == 1 ~ 4,
  mig_status3 == "2. Generation" & d_se_n5_1 == 2 ~ 5,
  default = as.numeric(NA)
))

nako$mig_staat <- assign_labels(
  nako$mig_staat,
  variable = "Migrationshintergrund",
  values = c(
    `0` = "Ohne MH, DE Staatsb.",
    `2` = "1. Generation, DE Staatsb.",
    `3` = "1. Generation, keine DE Staatsb.",
    `4` = "2. Generation, DE Staatsb."
  )
)

nako$mig_staat_num <- nako$mig_staat
nako$mig_staat <- to_factor(nako$mig_staat)

# mit wie vielen Jahren nach DE immigriert?
nako$alter_immigr <- nako$basis_age - nako$jahre_in_de
fehlerhaft <- which(nako$alter_immigr <= 1)
nako$alter_immigr[fehlerhaft] <- nako$alter_immigr[fehlerhaft] + 2

nako$alter_immigr <- assign_labels(nako$alter_immigr, variable = "Lebensphase Immigration")

nako$alter_immigr_gr <- recode_values(
  nako$alter_immigr,
  recode = list(`1` = 0:12, `2` = 13:18, `3` = 19:40, `4` = 41:60, `5` = "61:max"),
  preserve_na = TRUE
)
nako$alter_immigr_gr <- assign_labels(
  nako$alter_immigr_gr,
  variable = "Lebensphase Immigration (gruppiert)",
  values = c("Schulalter", "Jugendalter", "junges Erwachsenenalter", "Erwachsenenalter", "Ruhestand")
)

nako$lifetime_de <- round(100 * (nako$jahre_in_de / nako$basis_age))
nako$lifetime_de_cat <- recode_values(
  nako$lifetime_de,
  recode = list(`1` = "min:24", `2` = 25:49, `3` = 50:74, `4` = "75:max"),
  preserve_na = TRUE
)
# same as
# nako$lifetime_de_cat2 <- categorize(nako$lifetime_de, split = "equal_length", n_groups = 4)

nako$lifetime_de <- assign_labels(nako$lifetime_de, variable = "Anteil der Lebenszeit in DE")
nako$lifetime_de_cat <- assign_labels(
  nako$lifetime_de_cat,
  variable = "Anteil der Lebenszeit in DE (gruppiert)",
  values = c("bis 25%", "25-50%", "50-75%", "über 75%")
)


nako$lifetime_de_cat2 <- nako$lifetime_de_cat
nako$lifetime_de_cat2[nako$mig_status3 == "2. Generation" & is.na(nako$lifetime_de_cat2)] <- 0

nako$lifetime_de_cat2 <- assign_labels(
  nako$lifetime_de_cat2,
  variable = "Anteil der Lebenszeit in DE (gruppiert)",
  values = c("2. Generation", "bis 25%", "25-50%", "50-75%", "über 75%")
)

nako$jahre_in_de_cat2 <- categorize(nako$jahre_in_de, "equal_range", range = 10, lowest = 0) + 1
nako$jahre_in_de_cat2[nako$mig_status3 == "2. Generation" & is.na(nako$jahre_in_de_cat2)] <- 0
nako$jahre_in_de_cat2 <- assign_labels(
  nako$jahre_in_de_cat2,
  values = c(
    `0` = "2. Generation",
    `1` = "<10", `2` = "10-19", `3` = "20-29", `4` = "30-39",
    `5` = "40-49", `6` = "50-59", `7` = "60-69", `8` = "70+"    
  ),
  variable = "Aufenthaltsdauer in Deutschland (gruppiert)"
)
nako$jahre_in_de_cat_f2 <- to_factor(nako$jahre_in_de_cat2)
