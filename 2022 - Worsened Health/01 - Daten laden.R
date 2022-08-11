library(easystats)
library(strengejacke)



# SHARE COVID wave 8 ---------------

share8_covid1 <- read_spss("Daten/Share8-Corona/sharew8ca_rel8-0-0_ca.sav",
                           convert.factors = FALSE,
                           drop.labels = TRUE)
share8_covid1_at <- read_spss("Daten/Share8-Corona/sharew8ca_rel8-0-0_ca_at.sav",
                              convert.factors = FALSE,
                              drop.labels = TRUE)
# copy country variable
share8_covid1$countries <- datawizard::to_factor(share8_covid1$country)
share8_covid1_at$countries <- datawizard::to_factor(share8_covid1_at$country)
# add data from Austria
share8_covid1 <- data_merge(share8_covid1, share8_covid1_at, join = "bind")
share8_covid2 <- read_spss("Daten/Share8-Corona/sharew8ca_rel8-0-0_cv_r.sav",
                           convert.factors = FALSE,
                           drop.labels = TRUE)

# merge datasets
share8_covid <- data_merge(share8_covid1, share8_covid2, join = "left", by = "mergeid")

share8_weights <- read_spss("Daten/Share8-Corona/sharew8ca_rel8-0-0_gv_weights_ca.sav",
                            convert.factors = FALSE,
                            drop.labels = TRUE)

share8_weights <-   data_rename(share8_weights,
                                pattern = c("dw_w8", "cchw_w8_ca", "cciw_w8_ca", "stratum1"),
                                replacement = c("design_weights", "crosshh_weights", "crossin_weights", "stratum"))
share8_covid$wave <- 8

share8_covid <- data_merge(
  share8_covid,
  share8_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
  join = "left",
  by = "mergeid"
)



# SHARE COVID wave 9 ---------------

share9_covid1 <- read_spss("Daten/Share9-Corona/sharew9ca_rel8-0-0_ca.sav",
                           convert.factors = FALSE,
                           drop.labels = TRUE)
share9_covid2 <- read_spss("Daten/Share9-Corona/sharew9ca_rel8-0-0_cv_r.sav",
                           convert.factors = FALSE,
                           drop.labels = TRUE)

# copy country variable
share9_covid1$countries <- datawizard::to_factor(share9_covid1$country)

share9_covid <- data_merge(share9_covid1, share9_covid2, join = "left", by = "mergeid")
share9_covid$wave <- 9

share9_weights <- read_spss("Daten/Share9-Corona/sharew9ca_rel8-0-0_gv_weights_ca.sav",
                            convert.factors = FALSE,
                            drop.labels = TRUE)

share9_weights <-   data_rename(share9_weights,
                                pattern = c("dw_w9ca", "cchw_w9ca_ca", "cciw_w9ca_ca", "stratum1"),
                                replacement = c("design_weights", "crosshh_weights", "crossin_weights", "stratum"))

share9_covid <- data_merge(
  share9_covid,
  share9_weights[c("mergeid", "design_weights", "crosshh_weights", "crossin_weights", "stratum", "psu")],
  join = "left",
  by = "mergeid"
)



# Merge all waved ------------------

share_8_9 <- data_merge(share8_covid, share9_covid, join = "bind")



# some cleaning --------------

not_needed <- grepl("^rel_", colnames(share_8_9)) | grepl("(.*)\\.y$", colnames(share_8_9))
share_8_9 <- share_8_9[!not_needed]
colnames(share_8_9) <- gsub("(.*)\\.x$", "\\1", colnames(share_8_9))



# Add education --------------

load("Daten/isced.RData")

share8_is <- read_spss("Daten/Share8/sharew8_rel8-0-0_gv_isced.sav",
                       convert.factors = FALSE,
                       drop.labels = TRUE)
share8_is$isced1997_r <- convert_to_na(share8_is$isced1997_r, na = 95)
share8_is$education2 <- change_code(
  share8_is$isced1997_r,
  recode = list(`0` = 0:2, `1` = 3:4, `2` = 5:6) ,
  default = NA
)
share8_is$education2 <- datawizard::to_factor(set_labels(share8_is$education2, labels = c("low", "mid", "high")))

xx <- aggregate(education ~ mergeid,
                data = edu,
                FUN = function(i) round(mean(as.numeric(i) - 1, na.rm = TRUE)),
                na.action = na.pass)

xx <- data_merge(xx, share8_is[c("mergeid", "education2")], join = "left", by = "mergeid")
missing_ed <- is.na(xx$education) & !is.na(xx$education2)
xx$education[missing_ed] <- xx$education2[missing_ed]
xx$education[xx$education == 3] <- 2

share_8_9 <- data_merge(share_8_9, xx, join = "left", by = "mergeid")
share_8_9 <- drop_labels(share_8_9)
share <- share_8_9

# main file
save(share, file = "Daten/share_8_9.RData")

# intermediate step file
save(share, file = "Daten/share_8_9-step1.RData")

# recodings -------------------
source("02 - Recodings.R")
source("03 - Oxford Stringency Index.R")
source("04 - COVID19 cases.R")

# save and clean-up
save(share, file = "Daten/share.RData")

# separate data sets by wave
source("05 - Recode change in health.R")


to_remove <- c("cac003_1", "cac003_2", "cac003_3", "cac003_4", "cac003_5",
               "cac003_6", "cac003_7", "cac003_8", "cac003_97",
               "cac005_1", "cac005_2", "cac005_3", "cac005_4", "cac005_5",
               "cac005_6", "cac005_7", "cac005_8", "cac005_97",
               "cac011_1", "cac011_2", "cac011_3", "cac011_4", "cac011_5",
               "cac011_6", "cac011_7", "cac011_8", "cac011_97",
               "cac014_1", "cac014_2", "cac014_3", "cac014_4", "cac014_5",
               "cac014_6", "cac014_7", "cac014_8", "cac014_97",
               "caq006_1", "caq006_2", "caq006_3", "caq006_4", "caq006_97",
               "caq011_1", "caq011_2", "caq011_3", "caq011_4", "caq011_97",
               "caq016_1", "caq016_2", "caq016_3", "caq016_4", "caq016_97",
               "mergeidp9ca", "coupleid9ca",
               "V1_Vaccine Prioritisation (summary)",
               "V2A_Vaccine Availability (summary)",
               "V3_Vaccine Financial Support (summary)")
share <- data_remove(share, select = to_remove)
save(share, file = "share_short.RData")


# scores:

# gali: 0 = not limited, 1 = limited
# eurodcat: 0 = no depression, 1 = depression
# sphus: 1 = excellent, 5 = poor
# casp: 12 = low QoL, 48 = high QoL
# adl: number of limitations, higher = worse (i.e. lower mobility)
# iadl: number of instrumental limitations, higher = worse
