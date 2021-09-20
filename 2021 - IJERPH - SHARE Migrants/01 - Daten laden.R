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
  # rename mergeid ID
  colnames(x)[colnames(x) == "mergeidp1...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp2...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp3...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp4...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp5...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp6...3"] <- "mergeidp"
  colnames(x)[colnames(x) == "mergeidp7...3"] <- "mergeidp"
  # rename couple ID
  colnames(x)[colnames(x) == "coupleid1...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid2...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid3...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid4...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid5...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid6...4"] <- "coupleid"
  colnames(x)[colnames(x) == "coupleid7...4"] <- "coupleid"
  # rename country
  colnames(x)[colnames(x) == "country...5"] <- "country"
  # rename n of chronic diseases
  colnames(x)[colnames(x) == "chronicw1"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw2"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw4"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw5"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw6c"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw7"] <- "chron_n"
  colnames(x)[colnames(x) == "chronicw7c"] <- "chron_n"
  # rename chronic conditions
  colnames(x)[colnames(x) == "chronic2w1"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w2"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w4"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w5"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w6"] <- "chron_cond"
  colnames(x)[colnames(x) == "chronic2w7"] <- "chron_cond"
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
  # rename cross-calibrated household weights
  colnames(x)[colnames(x) == "cchw_w1"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w2"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w4"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w5"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w6"] <- "crosshh_weights"
  colnames(x)[colnames(x) == "cchw_w7"] <- "crosshh_weights"
  # rename cross-calibrated individual weights
  colnames(x)[colnames(x) == "cciw_w1"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w2"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w4"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w5"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w6"] <- "crossin_weights"
  colnames(x)[colnames(x) == "cciw_w7"] <- "crossin_weights"
  colnames(x)[colnames(x) == "stratum1"] <- "stratum"
  x
}



# generated variables -----------------

variables_gv <- c("gali", "adl", "iadl", "iadl2", "casp", "eurod", "eurodcat",
                 "mobility", "sphus", "areabldgi", "isced1997_r", "gender",
                 "mobirth", "yrbirth", "partnerinhh", "hhsize")



# generated variables by wave  -----------------

variables_gv2 <- c("chronicw1", "chronicw2", "chronicw4", "chronicw5",
                   "chronicw6c", "chronicw7c", "chronicw7", "chronic2w1",
                   "chronic2w2", "chronic2w4", "chronic2w5", "chronic2w6",
                   "chronic2w7")



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

variables <- unique(c(variables_gv, variables_gv2, variables_ad, variables_id))



# SHARE wave 1 ---------------

share1_ch <- read_spss("Daten/Share1/sharew1_rel7-1-0_ch.sav", drop.labels = TRUE)
share1_co <- read_spss("Daten/Share1/sharew1_rel7-1-0_co.sav", drop.labels = TRUE)
share1_cv <- read_spss("Daten/Share1/sharew1_rel7-1-0_cv_r.sav", drop.labels = TRUE)
share1_dn <- read_spss("Daten/Share1/sharew1_rel7-1-0_dn.sav", drop.labels = TRUE)
share1_ep <- read_spss("Daten/Share1/sharew1_rel7-1-0_ep.sav", drop.labels = TRUE)
share1_he <- read_spss("Daten/Share1/sharew1_rel7-1-0_gv_health.sav", drop.labels = TRUE)
share1_ho <- read_spss("Daten/Share1/sharew1_rel7-1-0_gv_housing.sav", drop.labels = TRUE)
share1_is <- read_spss("Daten/Share1/sharew1_rel7-1-0_gv_isced.sav", drop.labels = TRUE)
share1_hc <- read_spss("Daten/Share1/sharew1_rel7-1-0_hc.sav", drop.labels = TRUE)
share1_mh <- read_spss("Daten/Share1/sharew1_rel7-1-0_mh.sav", drop.labels = TRUE)
share1_ph <- read_spss("Daten/Share1/sharew1_rel7-1-0_ph.sav", drop.labels = TRUE)
share1_sp <- read_spss("Daten/Share1/sharew1_rel7-1-0_sp.sav", drop.labels = TRUE)
share1_weights <- read_spss("Daten/Share1/sharew1_rel7-1-0_gv_weights.sav", drop.labels = TRUE)

# unequal length
share1_cv <- share1_cv[na.omit(match(share1_cv$mergeid, share1_ch$mergeid)), ]

share1 <- bind_cols(share1_ch, share1_co, share1_cv, share1_dn, share1_ep, share1_he,
                    share1_ho, share1_is, share1_hc, share1_mh, share1_ph, share1_sp)

# country name
share1$country_name <- to_label(share1$country)

share1 <- rename_id(share1)

share1 <- share1[intersect(variables, colnames(share1))]
share1$wave <- 1

# bind weights
share1_weights <- rename_weights(share1_weights)
share1 <- dplyr::full_join(
  share1,
  share1_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
)

# save and clean-up
save(share1, file = "R-Daten/share1.RData")
rm(list = ls()[grepl("^share1_", ls())])



# SHARE wave 2 ---------------

share2_ch <- read_spss("Daten/Share2/sharew2_rel7-1-0_ch.sav", drop.labels = TRUE)
share2_co <- read_spss("Daten/Share2/sharew2_rel7-1-0_co.sav", drop.labels = TRUE)
share2_cv <- read_spss("Daten/Share2/sharew2_rel7-1-0_cv_r.sav", drop.labels = TRUE)
share2_dn <- read_spss("Daten/Share2/sharew2_rel7-1-0_dn.sav", drop.labels = TRUE)
share2_ep <- read_spss("Daten/Share2/sharew2_rel7-1-0_ep.sav", drop.labels = TRUE)
share2_he <- read_spss("Daten/Share2/sharew2_rel7-1-0_gv_health.sav", drop.labels = TRUE)
share2_ho <- read_spss("Daten/Share2/sharew2_rel7-1-0_gv_housing.sav", drop.labels = TRUE)
share2_is <- read_spss("Daten/Share2/sharew2_rel7-1-0_gv_isced.sav", drop.labels = TRUE)
share2_hc <- read_spss("Daten/Share2/sharew2_rel7-1-0_hc.sav", drop.labels = TRUE)
share2_mh <- read_spss("Daten/Share2/sharew2_rel7-1-0_mh.sav", drop.labels = TRUE)
share2_ph <- read_spss("Daten/Share2/sharew2_rel7-1-0_ph.sav", drop.labels = TRUE)
share2_sp <- read_spss("Daten/Share2/sharew2_rel7-1-0_sp.sav", drop.labels = TRUE)
share2_weights <- read_spss("Daten/Share2/sharew2_rel7-1-0_gv_weights.sav", drop.labels = TRUE)

# unequal length
share2_cv <- share2_cv[na.omit(match(share2_cv$mergeid, share2_ch$mergeid)), ]

share2 <- bind_cols(share2_ch, share2_co, share2_cv, share2_dn, share2_ep, share2_he,
                    share2_ho, share2_is, share2_hc, share2_mh, share2_ph, share2_sp)

# country name
share2$country_name <- to_label(share2$country)

share2 <- rename_id(share2)

share2 <- share2[intersect(variables, colnames(share2))]
share2$wave <- 2

# bind weights
share2_weights <- rename_weights(share2_weights)
share2 <- dplyr::full_join(
  share2,
  share2_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
)

# save and clean-up
save(share2, file = "R-Daten/share2.RData")
rm(list = ls()[grepl("^share2_", ls())])



# SHARE wave 4 ---------------

share4_ch <- read_spss("Daten/Share4/sharew4_rel7-1-0_ch.sav", drop.labels = TRUE)
share4_co <- read_spss("Daten/Share4/sharew4_rel7-1-0_co.sav", drop.labels = TRUE)
share4_cv <- read_spss("Daten/Share4/sharew4_rel7-1-0_cv_r.sav", drop.labels = TRUE)
share4_dn <- read_spss("Daten/Share4/sharew4_rel7-1-0_dn.sav", drop.labels = TRUE)
share4_ep <- read_spss("Daten/Share4/sharew4_rel7-1-0_ep.sav", drop.labels = TRUE)
share4_he <- read_spss("Daten/Share4/sharew4_rel7-1-0_gv_health.sav", drop.labels = TRUE)
share4_ho <- read_spss("Daten/Share4/sharew4_rel7-1-0_gv_housing.sav", drop.labels = TRUE)
share4_is <- read_spss("Daten/Share4/sharew4_rel7-1-0_gv_isced.sav", drop.labels = TRUE)
share4_hc <- read_spss("Daten/Share4/sharew4_rel7-1-0_hc.sav", drop.labels = TRUE)
share4_mh <- read_spss("Daten/Share4/sharew4_rel7-1-0_mh.sav", drop.labels = TRUE)
share4_ph <- read_spss("Daten/Share4/sharew4_rel7-1-0_ph.sav", drop.labels = TRUE)
share4_sp <- read_spss("Daten/Share4/sharew4_rel7-1-0_sp.sav", drop.labels = TRUE)
share4_weights <- read_spss("Daten/Share4/sharew4_rel7-1-0_gv_weights.sav", drop.labels = TRUE)

# unequal length
share4_cv <- share4_cv[na.omit(match(share4_cv$mergeid, share4_ch$mergeid)), ]

share4 <- bind_cols(share4_ch, share4_co, share4_cv, share4_dn, share4_ep, share4_he,
                    share4_ho, share4_is, share4_hc, share4_mh, share4_ph, share4_sp)

# country name
share4$country_name <- to_label(share4$country)

share4 <- rename_id(share4)

share4 <- share4[intersect(variables, colnames(share4))]
share4$wave <- 4

# bind weights
share4_weights <- rename_weights(share4_weights)
share4 <- dplyr::full_join(
  share4,
  share4_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
)

# save and clean-up
save(share4, file = "R-Daten/share4.RData")
rm(list = ls()[grepl("^share4_", ls())])



# SHARE wave 5 ---------------

share5_ch <- read_spss("Daten/Share5/sharew5_rel7-1-0_ch.sav", drop.labels = TRUE)
share5_co <- read_spss("Daten/Share5/sharew5_rel7-1-0_co.sav", drop.labels = TRUE)
share5_cv <- read_spss("Daten/Share5/sharew5_rel7-1-0_cv_r.sav", drop.labels = TRUE)
share5_dn <- read_spss("Daten/Share5/sharew5_rel7-1-0_dn.sav", drop.labels = TRUE)
share5_ep <- read_spss("Daten/Share5/sharew5_rel7-1-0_ep.sav", drop.labels = TRUE)
share5_he <- read_spss("Daten/Share5/sharew5_rel7-1-0_gv_health.sav", drop.labels = TRUE)
share5_ho <- read_spss("Daten/Share5/sharew5_rel7-1-0_gv_housing.sav", drop.labels = TRUE)
share5_is <- read_spss("Daten/Share5/sharew5_rel7-1-0_gv_isced.sav", drop.labels = TRUE)
share5_hc <- read_spss("Daten/Share5/sharew5_rel7-1-0_hc.sav", drop.labels = TRUE)
share5_mh <- read_spss("Daten/Share5/sharew5_rel7-1-0_mh.sav", drop.labels = TRUE)
share5_ph <- read_spss("Daten/Share5/sharew5_rel7-1-0_ph.sav", drop.labels = TRUE)
share5_sp <- read_spss("Daten/Share5/sharew5_rel7-1-0_sp.sav", drop.labels = TRUE)
share5_weights <- read_spss("Daten/Share5/sharew5_rel7-1-0_gv_weights.sav", drop.labels = TRUE)

# unequal length
share5_cv <- share5_cv[na.omit(match(share5_cv$mergeid, share5_ch$mergeid)), ]

share5 <- bind_cols(share5_ch, share5_co, share5_cv, share5_dn, share5_ep, share5_he,
                    share5_ho, share5_is, share5_hc, share5_mh, share5_ph, share5_sp)

# country name
share5$country_name <- to_label(share5$country)

# country name
share5$country_name <- to_label(share5$country)

share5 <- rename_id(share5)

share5 <- share5[intersect(variables, colnames(share5))]
share5$wave <- 5

# bind weights
share5_weights <- rename_weights(share5_weights)
share5 <- dplyr::full_join(
  share5,
  share5_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
)

# save and clean-up
save(share5, file = "R-Daten/share5.RData")
rm(list = ls()[grepl("^share5_", ls())])



# SHARE wave 6 ---------------

share6_ch <- read_spss("Daten/Share6/sharew6_rel7-1-0_ch.sav", drop.labels = TRUE)
share6_co <- read_spss("Daten/Share6/sharew6_rel7-1-0_co.sav", drop.labels = TRUE)
share6_cv <- read_spss("Daten/Share6/sharew6_rel7-1-0_cv_r.sav", drop.labels = TRUE)
share6_dn <- read_spss("Daten/Share6/sharew6_rel7-1-0_dn.sav", drop.labels = TRUE)
share6_ep <- read_spss("Daten/Share6/sharew6_rel7-1-0_ep.sav", drop.labels = TRUE)
share6_he <- read_spss("Daten/Share6/sharew6_rel7-1-0_gv_health.sav", drop.labels = TRUE)
share6_ho <- read_spss("Daten/Share6/sharew6_rel7-1-0_gv_housing.sav", drop.labels = TRUE)
share6_is <- read_spss("Daten/Share6/sharew6_rel7-1-0_gv_isced.sav", drop.labels = TRUE)
share6_hc <- read_spss("Daten/Share6/sharew6_rel7-1-0_hc.sav", drop.labels = TRUE)
share6_mh <- read_spss("Daten/Share6/sharew6_rel7-1-0_mh.sav", drop.labels = TRUE)
share6_ph <- read_spss("Daten/Share6/sharew6_rel7-1-0_ph.sav", drop.labels = TRUE)
share6_sp <- read_spss("Daten/Share6/sharew6_rel7-1-0_sp.sav", drop.labels = TRUE)
share6_weights <- read_spss("Daten/Share6/sharew6_rel7-1-0_gv_weights.sav", drop.labels = TRUE)

# unequal length
share6_cv <- share6_cv[na.omit(match(share6_cv$mergeid, share6_ch$mergeid)), ]

share6 <- bind_cols(share6_ch, share6_co, share6_cv, share6_dn, share6_ep, share6_he,
                    share6_ho, share6_is, share6_hc, share6_mh, share6_ph, share6_sp)

# country name
share6$country_name <- to_label(share6$country)

share6 <- rename_id(share6)

share6 <- share6[intersect(variables, colnames(share6))]
share6$wave <- 6

# bind weights
share6_weights <- rename_weights(share6_weights)
share6 <- dplyr::full_join(
  share6,
  share6_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
)

# save and clean-up
save(share6, file = "R-Daten/share6.RData")
rm(list = ls()[grepl("^share6_", ls())])



# SHARE wave 7 ---------------

share7_ch <- read_spss("Daten/share7/sharew7_rel7-1-0_ch.sav", drop.labels = TRUE)
share7_co <- read_spss("Daten/share7/sharew7_rel7-1-0_co.sav", drop.labels = TRUE)
share7_cv <- read_spss("Daten/share7/sharew7_rel7-1-0_cv_r.sav", drop.labels = TRUE)
share7_dn <- read_spss("Daten/share7/sharew7_rel7-1-0_dn.sav", drop.labels = TRUE)
share7_ep <- read_spss("Daten/share7/sharew7_rel7-1-0_ep.sav", drop.labels = TRUE)
share7_he <- read_spss("Daten/share7/sharew7_rel7-1-0_gv_health.sav", drop.labels = TRUE)
share7_ho <- read_spss("Daten/share7/sharew7_rel7-1-0_gv_housing.sav", drop.labels = TRUE)
share7_is <- read_spss("Daten/share7/sharew7_rel7-1-0_gv_isced.sav", drop.labels = TRUE)
share7_hc <- read_spss("Daten/share7/sharew7_rel7-1-0_hc.sav", drop.labels = TRUE)
share7_mh <- read_spss("Daten/share7/sharew7_rel7-1-0_mh.sav", drop.labels = TRUE)
share7_ph <- read_spss("Daten/share7/sharew7_rel7-1-0_ph.sav", drop.labels = TRUE)
share7_sp <- read_spss("Daten/share7/sharew7_rel7-1-0_sp.sav", drop.labels = TRUE)
share7_weights <- read_spss("Daten/share7/sharew7_rel7-1-0_gv_weights.sav", drop.labels = TRUE)

# unequal length
share7_cv <- share7_cv[na.omit(match(share7_cv$mergeid, share7_ch$mergeid)), ]

share7 <- bind_cols(share7_ch, share7_co, share7_cv, share7_dn, share7_ep, share7_he,
                    share7_ho, share7_is, share7_hc, share7_mh, share7_ph, share7_sp)

# country name
share7$country_name <- to_label(share7$country)

share7 <- rename_id(share7)

share7 <- share7[intersect(variables, colnames(share7))]
share7$wave <- 7

# bind weights
share7_weights <- rename_weights(share7_weights)
share7 <- dplyr::full_join(
  share7,
  share7_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
)

# save and clean-up
save(share7, file = "R-Daten/share7.RData")
rm(list = ls()[grepl("^share7_", ls())])



# final data set -----

share <- merge_df(share1, share2, share4, share5, share6, share7)
share <- drop_labels(set_na(share, na = c(-4, -2, -1, 97)))


country_labels <- c(
  get_labels(share1$country, values = "n"),
  get_labels(share2$country, values = "n"),
  get_labels(share4$country, values = "n"),
  get_labels(share5$country, values = "n"),
  get_labels(share6$country, values = "n"),
  get_labels(share7$country, values = "n")
)

share$country <- set_labels(share$country, labels = country_labels[!duplicated(country_labels)])

# recodings -------------------
source("02 - Recodings.R")
source("03 - Recodings Countries.R")
source("04 - Recodings Migrant (Non)Western.R")


share <- share %>%
  rename_variables(areabldgi = living, hh_make_ends_meet = "make_ends_meet") %>%
  to_label(living, gender, make_ends_meet)

levels(share$living) <- c("Metroplitan", "Metroplitan area", "Large City", "Small city", "Rural")

share <- standardize(share, select = "age_wave", append = TRUE)
share <- bind_cols(share, demean(share, c("age_wave", "age_wave_z"), "mergeid"))
share <- rename_variables(share, age_wave_z_within = "age_within", age_wave_z_between = "age_between")


save(share, file = "share.RData")




# scores:

# gali: 0 = not limited, 1 = limited
# eurodcat: 0 = no depression, 1 = depression
# sphus: 1 = excellent, 5 = poor
# casp: 12 = low QoL, 48 = high QoL
# adl: number of limitations, higher = worse (i.e. lower mobility)
# iadl: number of instrumental limitations, higher = worse
