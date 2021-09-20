codes <- readxl::read_excel("countrycodes.xlsx")
share$migrant_western <- NA
share$migrant_european <- NA

for (i in 1:nrow(codes)) {
  share$migrant_western[share$country_born_code == codes$code[i] & share$migrant == "migrant"] <- codes$`Western (1) Non-Western (2)`[i]
}
share$migrant_western[is.na(share$migrant_western) & share$migrant == "migrant"] <- 2
share$migrant_western[is.na(share$migrant_western)] <- 0
share$migrant_western <- factor(share$migrant_western, levels = c(0, 1, 2), labels = c("No Migrant", "Western", "Non-Western"))

for (i in 1:nrow(codes)) {
  share$migrant_european[share$country_born_code == codes$code[i] & share$migrant == "migrant"] <- codes$`European (1) Non-European (2)`[i]
}
share$migrant_european[is.na(share$migrant_european) & share$migrant == "migrant"] <- 2
share$migrant_european[is.na(share$migrant_european)] <- 0
share$migrant_european <- factor(share$migrant_european, levels = c(0, 1, 2), labels = c("No Migrant", "European", "Non-European"))

share$migrant_western[is.na(share$migrant)] <- NA
share$migrant_european[is.na(share$migrant)] <- NA
