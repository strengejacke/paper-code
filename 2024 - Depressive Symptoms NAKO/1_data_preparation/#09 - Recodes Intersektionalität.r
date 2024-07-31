library(easystats)

# recodes ---------------------------------------------------------------

# Alter in drei Gruppen
nako$age3 <- recode_values(
  nako$basis_age,
  recode = list(`1` = "min:39", `2` = "40:59", `3` = "60:max")
)
nako$age3 <- as.factor(nako$age3)
levels(nako$age3) <- c("<40", "40-59", "60+")

# Geschlecht umbenennen
nako$gender <- nako$basis_sex
nako$migstatus <- nako$mig_status3

# Bildung umbenennen, labeln
nako$education <- nako$a_ses_isced97_cat
levels(nako$education)  <- c("low", "medium", "high")

levels(nako$aussiedler_2)
levels(nako$age3)
levels(nako$gender)
levels(nako$education)

nako$intersec4 <- recode_into(
  # Ohne Migrationshintergrund
  aussiedler_2 == "Ohne MH" & age3 == "-39" & gender == "male" & education == "low" ~ "non-migrant, <40, male, low education",
  aussiedler_2 == "Ohne MH" & age3 == "40-59" & gender == "male" & education == "low" ~ "non-migrant, 40-59, male, low education",
  aussiedler_2 == "Ohne MH" & age3 == "60+" & gender == "male" & education == "low" ~ "non-migrant, 60+, male, low education",
  aussiedler_2 == "Ohne MH" & age3 == "-39" & gender == "female" & education == "low" ~ "non-migrant, <40, female, low education",
  aussiedler_2 == "Ohne MH" & age3 == "40-59" & gender == "female" & education == "low" ~ "non-migrant, 40-59, female, low education",
  aussiedler_2 == "Ohne MH" & age3 == "60+" & gender == "female" & education == "low" ~ "non-migrant, 60+, female, low education",
  aussiedler_2 == "Ohne MH" & age3 == "-39" & gender == "male" & education == "medium" ~ "non-migrant, <40, male, intermediate education",
  aussiedler_2 == "Ohne MH" & age3 == "40-59" & gender == "male" & education == "medium" ~ "non-migrant, 40-59, male, intermediate education",
  aussiedler_2 == "Ohne MH" & age3 == "60+" & gender == "male" & education == "medium" ~ "non-migrant, 60+, male, intermediate education",
  aussiedler_2 == "Ohne MH" & age3 == "-39" & gender == "female" & education == "medium" ~ "non-migrant, <40, female, intermediate education",
  aussiedler_2 == "Ohne MH" & age3 == "40-59" & gender == "female" & education == "medium" ~ "non-migrant, 40-59, female, intermediate education",
  aussiedler_2 == "Ohne MH" & age3 == "60+" & gender == "female" & education == "medium" ~ "non-migrant, 60+, female, intermediate education",
  aussiedler_2 == "Ohne MH" & age3 == "-39" & gender == "male" & education == "high" ~ "non-migrant, <40, male, high education",
  aussiedler_2 == "Ohne MH" & age3 == "40-59" & gender == "male" & education == "high" ~ "non-migrant, 40-59, male, high education",
  aussiedler_2 == "Ohne MH" & age3 == "60+" & gender == "male" & education == "high" ~ "non-migrant, 60+, male, high education",
  aussiedler_2 == "Ohne MH" & age3 == "-39" & gender == "female" & education == "high" ~ "non-migrant, <40, female, high education",
  aussiedler_2 == "Ohne MH" & age3 == "40-59" & gender == "female" & education == "high" ~ "non-migrant, 40-59, female, high education",
  aussiedler_2 == "Ohne MH" & age3 == "60+" & gender == "female" & education == "high" ~ "non-migrant, 60+, female, high education",
  # Ausländer
  aussiedler_2 == "Ausländer" & age3 == "-39" & gender == "male" & education == "low" ~ "1st generation migrant, <40, male, low education",
  aussiedler_2 == "Ausländer" & age3 == "40-59" & gender == "male" & education == "low" ~ "1st generation migrant, 40-59, male, low education",
  aussiedler_2 == "Ausländer" & age3 == "60+" & gender == "male" & education == "low" ~ "1st generation migrant, 60+, male, low education",
  aussiedler_2 == "Ausländer" & age3 == "-39" & gender == "female" & education == "low" ~ "1st generation migrant, <40, female, low education",
  aussiedler_2 == "Ausländer" & age3 == "40-59" & gender == "female" & education == "low" ~ "1st generation migrant, 40-59, female, low education",
  aussiedler_2 == "Ausländer" & age3 == "60+" & gender == "female" & education == "low" ~ "1st generation migrant, 60+, female, low education",
  aussiedler_2 == "Ausländer" & age3 == "-39" & gender == "male" & education == "medium" ~ "1st generation migrant, <40, male, intermediate education",
  aussiedler_2 == "Ausländer" & age3 == "40-59" & gender == "male" & education == "medium" ~ "1st generation migrant, 40-59, male, intermediate education",
  aussiedler_2 == "Ausländer" & age3 == "60+" & gender == "male" & education == "medium" ~ "1st generation migrant, 60+, male, intermediate education",
  aussiedler_2 == "Ausländer" & age3 == "-39" & gender == "female" & education == "medium" ~ "1st generation migrant, <40, female, intermediate education",
  aussiedler_2 == "Ausländer" & age3 == "40-59" & gender == "female" & education == "medium" ~ "1st generation migrant, 40-59, female, intermediate education",
  aussiedler_2 == "Ausländer" & age3 == "60+" & gender == "female" & education == "medium" ~ "1st generation migrant, 60+, female, intermediate education",
  aussiedler_2 == "Ausländer" & age3 == "-39" & gender == "male" & education == "high" ~ "1st generation migrant, <40, male, high education",
  aussiedler_2 == "Ausländer" & age3 == "40-59" & gender == "male" & education == "high" ~ "1st generation migrant, 40-59, male, high education",
  aussiedler_2 == "Ausländer" & age3 == "60+" & gender == "male" & education == "high" ~ "1st generation migrant, 60+, male, high education",
  aussiedler_2 == "Ausländer" & age3 == "-39" & gender == "female" & education == "high" ~ "1st generation migrant, <40, female, high education",
  aussiedler_2 == "Ausländer" & age3 == "40-59" & gender == "female" & education == "high" ~ "1st generation migrant, 40-59, female, high education",
  aussiedler_2 == "Ausländer" & age3 == "60+" & gender == "female" & education == "high" ~ "1st generation migrant, 60+, female, high education",
  # Eingebürgert
  aussiedler_2 == "Eingebürgert" & age3 == "-39" & gender == "male" & education == "low" ~ "1st generation naturalized, <40, male, low education",
  aussiedler_2 == "Eingebürgert" & age3 == "40-59" & gender == "male" & education == "low" ~ "1st generation naturalized, 40-59, male, low education",
  aussiedler_2 == "Eingebürgert" & age3 == "60+" & gender == "male" & education == "low" ~ "1st generation naturalized, 60+, male, low education",
  aussiedler_2 == "Eingebürgert" & age3 == "-39" & gender == "female" & education == "low" ~ "1st generation naturalized, <40, female, low education",
  aussiedler_2 == "Eingebürgert" & age3 == "40-59" & gender == "female" & education == "low" ~ "1st generation naturalized, 40-59, female, low education",
  aussiedler_2 == "Eingebürgert" & age3 == "60+" & gender == "female" & education == "low" ~ "1st generation naturalized, 60+, female, low education",
  aussiedler_2 == "Eingebürgert" & age3 == "-39" & gender == "male" & education == "medium" ~ "1st generation naturalized, <40, male, intermediate education",
  aussiedler_2 == "Eingebürgert" & age3 == "40-59" & gender == "male" & education == "medium" ~ "1st generation naturalized, 40-59, male, intermediate education",
  aussiedler_2 == "Eingebürgert" & age3 == "60+" & gender == "male" & education == "medium" ~ "1st generation naturalized, 60+, male, intermediate education",
  aussiedler_2 == "Eingebürgert" & age3 == "-39" & gender == "female" & education == "medium" ~ "1st generation naturalized, <40, female, intermediate education",
  aussiedler_2 == "Eingebürgert" & age3 == "40-59" & gender == "female" & education == "medium" ~ "1st generation naturalized, 40-59, female, intermediate education",
  aussiedler_2 == "Eingebürgert" & age3 == "60+" & gender == "female" & education == "medium" ~ "1st generation naturalized, 60+, female, intermediate education",
  aussiedler_2 == "Eingebürgert" & age3 == "-39" & gender == "male" & education == "high" ~ "1st generation naturalized, <40, male, high education",
  aussiedler_2 == "Eingebürgert" & age3 == "40-59" & gender == "male" & education == "high" ~ "1st generation naturalized, 40-59, male, high education",
  aussiedler_2 == "Eingebürgert" & age3 == "60+" & gender == "male" & education == "high" ~ "1st generation naturalized, 60+, male, high education",
  aussiedler_2 == "Eingebürgert" & age3 == "-39" & gender == "female" & education == "high" ~ "1st generation naturalized, <40, female, high education",
  aussiedler_2 == "Eingebürgert" & age3 == "40-59" & gender == "female" & education == "high" ~ "1st generation naturalized, 40-59, female, high education",
  aussiedler_2 == "Eingebürgert" & age3 == "60+" & gender == "female" & education == "high" ~ "1st generation naturalized, 60+, female, high education",
  # Nachkommen (2. Gen)
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "-39" & gender == "male" & education == "low" ~ "2nd generation, <40, male, low education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "40-59" & gender == "male" & education == "low" ~ "2nd generation, 40-59, male, low education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "60+" & gender == "male" & education == "low" ~ "2nd generation, 60+, male, low education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "-39" & gender == "female" & education == "low" ~ "2nd generation, <40, female, low education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "40-59" & gender == "female" & education == "low" ~ "2nd generation, 40-59, female, low education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "60+" & gender == "female" & education == "low" ~ "2nd generation, 60+, female, low education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "-39" & gender == "male" & education == "medium" ~ "2nd generation, <40, male, intermediate education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "40-59" & gender == "male" & education == "medium" ~ "2nd generation, 40-59, male, intermediate education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "60+" & gender == "male" & education == "medium" ~ "2nd generation, 60+, male, intermediate education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "-39" & gender == "female" & education == "medium" ~ "2nd generation, <40, female, intermediate education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "40-59" & gender == "female" & education == "medium" ~ "2nd generation, 40-59, female, intermediate education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "60+" & gender == "female" & education == "medium" ~ "2nd generation, 60+, female, intermediate education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "-39" & gender == "male" & education == "high" ~ "2nd generation, <40, male, high education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "40-59" & gender == "male" & education == "high" ~ "2nd generation, 40-59, male, high education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "60+" & gender == "male" & education == "high" ~ "2nd generation, 60+, male, high education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "-39" & gender == "female" & education == "high" ~ "2nd generation, <40, female, high education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "40-59" & gender == "female" & education == "high" ~ "2nd generation, 40-59, female, high education",
  aussiedler_2 == "Nachkommen (2. Gen)" & age3 == "60+" & gender == "female" & education == "high" ~ "2nd generation, 60+, female, high education",
  # Aussiedler
  aussiedler_2 == "Aussiedler" & age3 == "-39" & gender == "male" & education == "low" ~ "German resettler, <40, male, low education",
  aussiedler_2 == "Aussiedler" & age3 == "40-59" & gender == "male" & education == "low" ~ "German resettler, 40-59, male, low education",
  aussiedler_2 == "Aussiedler" & age3 == "60+" & gender == "male" & education == "low" ~ "German resettler, 60+, male, low education",
  aussiedler_2 == "Aussiedler" & age3 == "-39" & gender == "female" & education == "low" ~ "German resettler, <40, female, low education",
  aussiedler_2 == "Aussiedler" & age3 == "40-59" & gender == "female" & education == "low" ~ "German resettler, 40-59, female, low education",
  aussiedler_2 == "Aussiedler" & age3 == "60+" & gender == "female" & education == "low" ~ "German resettler, 60+, female, low education",
  aussiedler_2 == "Aussiedler" & age3 == "-39" & gender == "male" & education == "medium" ~ "German resettler, <40, male, intermediate education",
  aussiedler_2 == "Aussiedler" & age3 == "40-59" & gender == "male" & education == "medium" ~ "German resettler, 40-59, male, intermediate education",
  aussiedler_2 == "Aussiedler" & age3 == "60+" & gender == "male" & education == "medium" ~ "German resettler, 60+, male, intermediate education",
  aussiedler_2 == "Aussiedler" & age3 == "-39" & gender == "female" & education == "medium" ~ "German resettler, <40, female, intermediate education",
  aussiedler_2 == "Aussiedler" & age3 == "40-59" & gender == "female" & education == "medium" ~ "German resettler, 40-59, female, intermediate education",
  aussiedler_2 == "Aussiedler" & age3 == "60+" & gender == "female" & education == "medium" ~ "German resettler, 60+, female, intermediate education",
  aussiedler_2 == "Aussiedler" & age3 == "-39" & gender == "male" & education == "high" ~ "German resettler, <40, male, high education",
  aussiedler_2 == "Aussiedler" & age3 == "40-59" & gender == "male" & education == "high" ~ "German resettler, 40-59, male, high education",
  aussiedler_2 == "Aussiedler" & age3 == "60+" & gender == "male" & education == "high" ~ "German resettler, 60+, male, high education",
  aussiedler_2 == "Aussiedler" & age3 == "-39" & gender == "female" & education == "high" ~ "German resettler, <40, female, high education",
  aussiedler_2 == "Aussiedler" & age3 == "40-59" & gender == "female" & education == "high" ~ "German resettler, 40-59, female, high education",
  aussiedler_2 == "Aussiedler" & age3 == "60+" & gender == "female" & education == "high" ~ "German resettler, 60+, female, high education",
  data = nako,
  default = NA
)


nako$intersec3a <- recode_into(
  # Ohne Migrationshintergrund
  aussiedler_2 == "Ohne MH" & gender == "male" & education == "low" ~ "low education, male, non-migrant",
  aussiedler_2 == "Ohne MH" & gender == "female" & education == "low" ~ "low education, female, non-migrant",
  aussiedler_2 == "Ohne MH" & gender == "male" & education == "medium" ~ "intermediate education, male, non-migrant",
  aussiedler_2 == "Ohne MH" & gender == "female" & education == "medium" ~ "intermediate education, female, non-migrant",
  aussiedler_2 == "Ohne MH" & gender == "male" & education == "high" ~ "high education, male, non-migrant",
  aussiedler_2 == "Ohne MH" & gender == "female" & education == "high" ~ "high education, female, non-migrant",
  # Ausländer
  aussiedler_2 == "Ausländer" & gender == "male" & education == "low" ~ "low education, male, 1st generation migrant",
  aussiedler_2 == "Ausländer" & gender == "female" & education == "low" ~ "low education, female, 1st generation migrant",
  aussiedler_2 == "Ausländer" & gender == "male" & education == "medium" ~ "intermediate education, male, 1st generation migrant",
  aussiedler_2 == "Ausländer" & gender == "female" & education == "medium" ~ "intermediate education, female, 1st generation migrant",
  aussiedler_2 == "Ausländer" & gender == "male" & education == "high" ~ "high education, male, 1st generation migrant",
  aussiedler_2 == "Ausländer" & gender == "female" & education == "high" ~ "high education, female, 1st generation migrant",
  # Eingebürgert
  aussiedler_2 == "Eingebürgert" & gender == "male" & education == "low" ~ "low education, male, 1st generation naturalized",
  aussiedler_2 == "Eingebürgert" & gender == "female" & education == "low" ~ "low education, female, 1st generation naturalized",
  aussiedler_2 == "Eingebürgert" & gender == "male" & education == "medium" ~ "intermediate education, male, 1st generation naturalized",
  aussiedler_2 == "Eingebürgert" & gender == "female" & education == "medium" ~ "intermediate education, female, 1st generation naturalized",
  aussiedler_2 == "Eingebürgert" & gender == "male" & education == "high" ~ "high education, male, 1st generation naturalized",
  aussiedler_2 == "Eingebürgert" & gender == "female" & education == "high" ~ "high education, female, 1st generation naturalized",
  # Nachkommen (2. Gen)
  aussiedler_2 == "Nachkommen (2. Gen)" & gender == "male" & education == "low" ~ "low education, male, 2nd generation",
  aussiedler_2 == "Nachkommen (2. Gen)" & gender == "female" & education == "low" ~ "low education, female, 2nd generation",
  aussiedler_2 == "Nachkommen (2. Gen)" & gender == "male" & education == "medium" ~ "intermediate education, male, 2nd generation",
  aussiedler_2 == "Nachkommen (2. Gen)" & gender == "female" & education == "medium" ~ "intermediate education, female, 2nd generation",
  aussiedler_2 == "Nachkommen (2. Gen)" & gender == "male" & education == "high" ~ "high education, male, 2nd generation",
  aussiedler_2 == "Nachkommen (2. Gen)" & gender == "female" & education == "high" ~ "high education, female, 2nd generation",
  # Aussiedler
  aussiedler_2 == "Aussiedler" & gender == "male" & education == "low" ~ "low education, male, German resettler",
  aussiedler_2 == "Aussiedler" & gender == "female" & education == "low" ~ "low education, female, German resettler",
  aussiedler_2 == "Aussiedler" & gender == "male" & education == "medium" ~ "intermediate education, male, German resettler",
  aussiedler_2 == "Aussiedler" & gender == "female" & education == "medium" ~ "intermediate education, female, German resettler",
  aussiedler_2 == "Aussiedler" & gender == "male" & education == "high" ~ "high education, male, German resettler",
  aussiedler_2 == "Aussiedler" & gender == "female" & education == "high" ~ "high education, female, German resettler",
  data = nako,
  default = NA
)


nako$intersec3b <- recode_into(
  # Ohne Migrationshintergrund
  migstatus == "Ohne MH" & gender == "male" & education == "low" ~ "non-migrant, male, low education",
  migstatus == "Ohne MH" & gender == "female" & education == "low" ~ "non-migrant, female, low education",
  migstatus == "Ohne MH" & gender == "male" & education == "medium" ~ "non-migrant, male, intermediate education",
  migstatus == "Ohne MH" & gender == "female" & education == "medium" ~ "non-migrant, female, intermediate education",
  migstatus == "Ohne MH" & gender == "male" & education == "high" ~ "non-migrant, male, high education",
  migstatus == "Ohne MH" & gender == "female" & education == "high" ~ "non-migrant, female, high education",
  # Ausländer
  migstatus == "2. Generation" & gender == "male" & education == "low" ~ "2nd gen, male, low education",
  migstatus == "2. Generation" & gender == "female" & education == "low" ~ "2nd gen, female, low education",
  migstatus == "2. Generation" & gender == "male" & education == "medium" ~ "2nd gen, male, intermediate education",
  migstatus == "2. Generation" & gender == "female" & education == "medium" ~ "2nd gen, female, intermediate education",
  migstatus == "2. Generation" & gender == "male" & education == "high" ~ "2nd gen, male, high education",
  migstatus == "2. Generation" & gender == "female" & education == "high" ~ "2nd gen, female, high education",
  # Eingebürgert
  migstatus == "1. Generation" & gender == "male" & education == "low" ~ "1st gen, male, low education",
  migstatus == "1. Generation" & gender == "female" & education == "low" ~ "1st gen, female, low education",
  migstatus == "1. Generation" & gender == "male" & education == "medium" ~ "1st gen, male, intermediate education",
  migstatus == "1. Generation" & gender == "female" & education == "medium" ~ "1st gen, female, intermediate education",
  migstatus == "1. Generation" & gender == "male" & education == "high" ~ "1st gen, male, high education",
  migstatus == "1. Generation" & gender == "female" & education == "high" ~ "1st gen, female, high education",
  data = nako,
  default = NA
)

# Intersektionalität 3 Mig-Gruppen, Alter (3 Gruppen), Geschlecgt und Bildung
nako$intersec3c <- with(nako, paste0(migstatus, ", ", age3, ", ", gender, ", ", education))
nako$intersec3c[grepl("NA", nako$intersec3c, fixed = TRUE)] <- NA
nako$intersec3c <- gsub("Generation", "Gen", nako$intersec3c, fixed = TRUE)
nako$intersec3c <- gsub("Ohne MH", "non-migrant", nako$intersec3c, fixed = TRUE)
