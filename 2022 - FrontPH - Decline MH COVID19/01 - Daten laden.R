library(strengejacke)
library(dplyr)



# helper functions ------

rename_id <- function(x) {
  colnames(x)[colnames(x) == "mergeid...1"] <- "mergeid"
  # rename household ID
  colnames(x)[colnames(x) == "hhid1...2"] <- "hhid"
  colnames(x)[colnames(x) == "hhid2...2"] <- "hhid"
  colnames(x)[colnames(x) == "hhid3...2"] <- "hhid"
  colnames(x)[colnames(x) == "hhid4...2"] <- "hhid"
  colnames(x)[colnames(x) == "hhid5...2"] <- "hhid"
  colnames(x)[colnames(x) == "hhid6...2"] <- "hhid"
  colnames(x)[colnames(x) == "hhid7...2"] <- "hhid"
  colnames(x)[colnames(x) == "hhid8.x"] <- "hhid"
  # rename mergeid ID
  colnames(x)[colnames(x) == "mergeidp1...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp2...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp3...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp4...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp5...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp6...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp7...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp8.x"] <- "mergeidp"
  # rename couple ID
  colnames(x)[colnames(x) == "coupleid1...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid2...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid3...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid4...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid5...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid6...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid7...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid8.x"] <- "coupleid"
  # rename country
  colnames(x)[colnames(x) == "country.x"] <- "country"
  # rename n of chronic diseases
  colnames(x)[colnames(x) == "chronicw1"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw2"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw4"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw5"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw6c"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw7"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw7c"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw8c"] <- "chron_n"
  # rename chronic conditions
  colnames(x)[colnames(x) == "chronic2w1"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w2"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w4"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w5"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w6"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w7"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w8"] <- "chron_cond"
  x
}

rename_weights <- function(x) {
  # rename design weights
  colnames(x)[colnames(x) == "dw_w1"] <- "design_weights"
  colnames(x)[colnames(x) == "dw_w2"] <- "design_weights"
  colnames(x)[colnames(x) == "dw_w4"] <- "design_weights"
  colnames(x)[colnames(x) == "dw_w5"] <- "design_weights"
  colnames(x)[colnames(x) == "dw_w6"] <- "design_weights"
  colnames(x)[colnames(x) == "dw_w7"] <- "design_weights"
  colnames(x)[colnames(x) == "dw_w8"] <- "design_weights"
  # rename cross-calibrated household weights
  colnames(x)[colnames(x) == "cchw_w1"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w2"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w4"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w5"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w6"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w7"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w8"] <- "crosshh_weights"
  # rename cross-calibrated individual weights
  colnames(x)[colnames(x) == "cciw_w1"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w2"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w4"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w5"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w6"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w7"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w8"] <- "crossin_weights"
  colnames(x)[colnames(x) == "stratum1"] <- "stratum"
  x
}



# generated variables -----------------

variables_gv <- c("gali", "adl", "iadl", "iadl2", "casp", "eurod", "eurodcat",
                 "mobility", "sphus", "areabldgi", "isced1997_r", "gender",
                 "mobirth", "yrbirth", "partnerinhh", "hhsize", "education")



# generated variables: social network -----------------

variables_sn <- c("sizeofsocialnetwork", "sn_scale", "sn_satisfaction",
                  "sn_size_w8", "close_very", "close_mean", "contact_mean")



# health variables -----------------

variables_he <- c("bmi", "bmi2", "mobility2", "mobility3", "cusmoke", "drinkin2",
                  "phactiv")



# generated variables by wave  -----------------

variables_gv2 <- c("chronicw1", "chronicw2", "chronicw4", "chronicw5",
                   "chronicw6c", "chronicw7c", "chronicw7", "chronic2w1",
                   "chronic2w2", "chronic2w4", "chronic2w5", "chronic2w6",
                   "chronic2w7", "chronic2w8", "chronicw8c")



# additional variables -----------------

variables_ad <- c("ch001_", "ch007_", "co007_", "dn002_", "dn004_", "dn005_", "dn005c",
                  "dn006_", "dn007_", "dn008_", "dn014_", "dn042_", "ep005_", "hc029_",
                  "hc031_", "mh001_", "mh002_", "mh003_", "mh004_", "mh005_", "mh006_",
                  "mh007_", "mh008_", "mh009_", "mh010_", "mh011_", "mh012_", "mh013_",
                  "mh014_", "mh015_", "mh016_", "mh017_", "ph003_", "ph005_", "ph006_",
                  "ph009_", "ph048_", "ph049_", "sp020_", "sp021_")



# ID variables -----------------

variables_id <- c("mergeid", "hhid", "country", "country_name", "mergeidp",
                  "coupleid", "chron_n", "chron_cond", "design_weights",
                  "crosshh_weights", "crossin_weights", "psu", "stratum")

variables <- unique(c(variables_gv, variables_gv2, variables_ad, variables_id,
                      variables_he, variables_sn))



# SHARE wave 8 ---------------

share8_ch <- read_spss("Daten/sharew8_rel1-0-0_ch.sav", drop.labels = TRUE)
share8_co <- read_spss("Daten/sharew8_rel1-0-0_co.sav", drop.labels = TRUE)
share8_cv <- read_spss("Daten/sharew8_rel1-0-0_cv_r.sav", drop.labels = TRUE)
share8_dn <- read_spss("Daten/sharew8_rel1-0-0_dn.sav", drop.labels = TRUE)
share8_ep <- read_spss("Daten/sharew8_rel1-0-0_ep.sav", drop.labels = TRUE)
share8_he <- read_spss("Daten/sharew8_rel1-0-0_gv_health.sav", drop.labels = TRUE)
share8_nw <- read_spss("Daten/sharew8_rel1-0-0_gv_networks.sav", drop.labels = TRUE)
share8_is <- read_spss("Daten/sharew8_rel1-0-0_gv_isced.sav", drop.labels = TRUE)
share8_hc <- read_spss("Daten/sharew8_rel1-0-0_hc.sav", drop.labels = TRUE)
share8_mh <- read_spss("Daten/sharew8_rel1-0-0_mh.sav", drop.labels = TRUE)
share8_ph <- read_spss("Daten/sharew8_rel1-0-0_ph.sav", drop.labels = TRUE)
share8_sp <- read_spss("Daten/sharew8_rel1-0-0_sp.sav", drop.labels = TRUE)
share8_covid <- read_spss("Daten/sharew8_rel1-0-0_ca.sav", drop.labels = TRUE)
share8_covid2 <- read_spss("Daten/sharew8_rel1-0-0_ca_at.sav", drop.labels = TRUE)
share8_covid <- bind_rows(share8_covid, share8_covid2)
share8_weights <- read_spss("Daten/sharew8_rel1-0-0_gv_weights.sav", drop.labels = TRUE)


load("../2020 IJERPH Share Migration/share.RData")
share$edu_label <- to_label(share$education)

share8 <- left_join(share8_ch, share8_co, by = "mergeid")
share8 <- left_join(share8, share8_cv, by = "mergeid")
share8 <- left_join(share8, share8_dn, by = "mergeid")
share8 <- left_join(share8, share8_ep, by = "mergeid")
share8 <- left_join(share8, share8_he, by = "mergeid")
share8 <- left_join(share8, share8_is, by = "mergeid")
share8 <- left_join(share8, share8_hc, by = "mergeid")
share8 <- left_join(share8, share8_mh, by = "mergeid")
share8 <- left_join(share8, share8_ph, by = "mergeid")
share8 <- left_join(share8, share8_sp, by = "mergeid")
share8 <- left_join(share8, share8_nw, by = "mergeid")

xx <- share |>
  filter(wave > 3) |>
  group_by(mergeid) |>
  summarize(education = round(mean(as.numeric(education) - 1, na.rm = TRUE)))

share8 <- left_join(share8, xx, by = "mergeid")

share8 <- rename_id(share8)
share8 <- share8[intersect(variables, colnames(share8))]

# bind weights
share8_weights <- rename_weights(share8_weights)

share8_weights$crossin_weights <- share8_weights$cciw_w8_main
share8_weights$crossin_weights[is.na(share8_weights$crossin_weights)] <- share8_weights$cciw_w8_ca[is.na(share8_weights$crossin_weights)]
share8_weights$crossin_weights[is.na(share8_weights$crossin_weights)] <- share8_weights$cciw_w8_main_ca[is.na(share8_weights$crossin_weights)]

share8_weights$crosshh_weights <- share8_weights$cchw_w8_main
share8_weights$crosshh_weights[is.na(share8_weights$crosshh_weights)] <- share8_weights$cchw_w8_ca[is.na(share8_weights$crosshh_weights)]
share8_weights$crosshh_weights[is.na(share8_weights$crosshh_weights)] <- share8_weights$cchw_w8_main_ca[is.na(share8_weights$crosshh_weights)]

share8 <- dplyr::left_join(
  share8,
  share8_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
  by = "mergeid"
)

share8 <- left_join(share8, share8_covid, by = "mergeid")

share8$wave <- 8

# country name
share8$country <- share8$country.x
share8$country.x <- NULL
share8$country.y <- NULL
share8$country_name <- to_label(share8$country)

share8 <- set_na(share8, na = c(-9999999, -9999992, -9999991, -20, -9, -4, -2, -1))
share8 <- set_na(share8, isced1997_r, ep005_, na = c(97:99))
share8 <- drop_labels(share8)

# recodings -------------------
source("02 - Recodings.R")
source("03 - Oxford Stringency Index.R")
source("04 - COVID19 cases.R")
source("05 - Infected by age.R")

# save and clean-up
save(share8, file = "share8.RData")





# scores:

# gali: 0 = not limited, 1 = limited
# eurodcat: 0 = no depression, 1 = depression
# sphus: 1 = excellent, 5 = poor
# casp: 12 = low QoL, 48 = high QoL
# adl: number of limitations, higher = worse (i.e. lower mobility)
# iadl: number of instrumental limitations, higher = worse
