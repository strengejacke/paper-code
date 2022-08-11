library(easystats)
library(strengejacke)
library(dplyr)

# set missing -----------------------------

share <- convert_to_na(share, na = c(-9999999, -9999992, -9999991, -9, -2, -1), verbose = FALSE)
share$cas004_1 <- convert_to_na(share$cas004_1, na = 99)
share$cas004_2 <- convert_to_na(share$cas004_2, na = 99)
share$cas004_3 <- convert_to_na(share$cas004_3, na = 99)
share$cas004_4 <- convert_to_na(share$cas004_4, na = 99)



# remove non-used variables -----------------------

to_remove <- c(2:7, 11, 15:21, 28, 58:64, 75:98, 109:115, 125:131, 152:158,
               161:169, 175:184, 186:191, 194:195, 204:220, 227:229, 231, 234:237,
               239:240, 243:244, 247, 249:265, 278:279, 281:282, 285, 296, 318:324,
               335:341, 365:371, 381:387, 413:418, 422:427, 431:440, 445:449,
               451:464, 467:470, 472:485, 487:489, 507:541)

share <- data_remove(share, select = to_remove)



# recode labels, to match variables from both waves --------------------

share$cah121_1 <- change_code(share$cah121_1, recode = list(`1` = 3, `2` = 1, `3` = 2), preserve_na = TRUE)
share$cah121_1 <- set_labels(share$cah121_1, labels = get_labels(share$cah021_))

share$camh113_1 <- change_code(share$camh113_1, recode = list(`1` = 3, `2` = 1, `3` = 2), preserve_na = TRUE)
share$camh113_1 <- set_labels(share$camh113_1, labels = get_labels(share$camh802_))

share$camh118_1 <- change_code(share$camh118_1, recode = list(`1` = 3, `2` = 1, `3` = 2), preserve_na = TRUE)
share$camh118_1 <- set_labels(share$camh118_1, labels = get_labels(share$camh807_))

share$cah102_ <- change_code(share$cah102_, recode = list(`1` = 1, `2` = 3, `3` = 2), preserve_na = TRUE)
share$cah102_ <- set_labels(share$cah102_, labels = get_labels(share$cah002_))

share$camh148_ <- change_code(share$camh148_, recode = list(`1` = 3, `2` = 1, `3` = 2), preserve_na = TRUE)
share$camh148_ <- set_labels(share$camh148_, labels = get_labels(share$camh837_))



# merge variables from Wave 9 and 8 -----------------------

merge_vars <- function(x, vars) {
  for (i in 1:length(vars)) {
    labs1 <- attributes(x[[vars[[i]][1]]])$labels
    labs2 <- attributes(x[[vars[[i]][2]]])$labels
    labs1 <- paste0(labs1, ": ", names(labs1))
    labs2 <- paste0(labs2, ": ", names(labs2))

    if (length(labs1) != length(labs2) || !all(labs1 == labs2)) {
      stop(paste0("Problematic variables: ", vars[[i]][1], vars[[i]][2]))
    }

    if (all(labs1 == labs2)) {
      x[[vars[[i]][1]]][x$wave == 9] <- x[[vars[[i]][2]]][x$wave == 9]
      lab1 <- attr(x[[vars[[i]][1]]], "label", exact = TRUE)
      lab2 <- attr(x[[vars[[i]][2]]], "label", exact = TRUE)
      x[[vars[[i]][2]]] <- NULL
      if (!is.null(lab1) && !is.null(lab2) && grepl("outbreak$", lab1) && grepl("last 3", lab2, fixed = TRUE)) {
        lab <- paste0(lab1, "/past 3 months")
        attr(x[[vars[[i]][1]]], "label") <- lab
      } else if (!is.null(lab1) && !is.null(lab2)) {
        attr(x[[vars[[i]][1]]], "label") <- paste0(lab1, "<br><strong>and:</strong><br>", lab2)
      }
    }
  }
  x
}


# health: change in health

vars1 <- list(
  c("cah002_", "cah102_")
)
share <- merge_vars(share, vars1)


# health: forgone care

vars2 <- list(
  c("caq005_", "caq105_"), c("caq006_1", "caq106_1"), c("caq006_2", "caq106_2"),
  c("caq006_3", "caq106_3"), c("caq006_4", "caq106_4"), c("caq006_97", "caq106_97")
)
share <- merge_vars(share, vars2)


# health: postponed treatments

vars3 <- list(
  c("caq010_", "caq110_"), c("caq011_1", "caq111_1"), c("caq011_2", "caq111_2"),
  c("caq011_3", "caq111_3"), c("caq011_4", "caq111_4"), c("caq011_97", "caq111_97")
)
share <- merge_vars(share, vars3)


# health: denied treatments

vars4 <- list(
  c("caq015_", "caq115_"), c("caq016_1", "caq116_1"), c("caq016_2", "caq116_2"),
  c("caq016_3", "caq116_3"), c("caq016_4", "caq116_4"), c("caq016_97", "caq116_97")
)
share <- merge_vars(share, vars4)


# health: covid-behaviour

vars5 <- list(
  c("cah010_", "cah110_"), c("cah013_", "cah113_")
)
share <- merge_vars(share, vars5)


# covid: symptoms

vars6 <- list(
  c("cac002_", "cac102_"), c("cac003_1", "cac103_1"), c("cac003_2", "cac103_2"),
  c("cac003_3", "cac103_3"), c("cac003_4", "cac103_4"), c("cac003_5", "cac103_5"),
  c("cac003_6", "cac103_6"), c("cac003_7", "cac103_7"), c("cac003_8", "cac103_8"),
  c("cac003_97", "cac103_97")
)
share <- merge_vars(share, vars6)


# covid: tested positive

vars7 <- list(
  c("cac004_", "cac104_"), c("cac005_1", "cac105_1"), c("cac005_2", "cac105_2"),
  c("cac005_3", "cac105_3"), c("cac005_4", "cac105_4"), c("cac005_5", "cac105_5"),
  c("cac005_6", "cac105_6"), c("cac005_7", "cac105_7"), c("cac005_8", "cac105_8"),
  c("cac005_97", "cac105_97")
)
share <- merge_vars(share, vars7)


# covid: hospitalized

vars8 <- list(
  c("cac010_", "cac110_"), c("cac011_1", "cac111_1"), c("cac011_2", "cac111_2"),
  c("cac011_3", "cac111_3"), c("cac011_4", "cac111_4"), c("cac011_5", "cac111_5"),
  c("cac011_6", "cac111_6"), c("cac011_7", "cac111_7"), c("cac011_8", "cac111_8"),
  c("cac011_97", "cac111_97")
)
share <- merge_vars(share, vars8)


# covid: died

vars9 <- list(
  c("cac013_", "cac113_"), c("cac014_2", "cac114_2"), c("cac014_3", "cac114_3"),
  c("cac014_4", "cac114_4"), c("cac014_5", "cac114_5"), c("cac014_6", "cac114_6"),
  c("cac014_7", "cac114_7"), c("cac014_8", "cac114_8"), c("cac014_97", "cac114_97")
)
share <- merge_vars(share, vars9)


# Social: contact frequency

vars10 <- list(
  c("cas003_1", "cas103_1"), c("cas003_2", "cas103_2"), c("cas003_3", "cas103_3"),
  c("cas003_4", "cas103_4")
)
share <- merge_vars(share, vars10)


# Social: electronic contact frequency

vars11 <- list(
  c("cas004_1", "cas104_1"), c("cas004_2", "cas104_2"), c("cas004_3", "cas104_3"),
  c("cas004_4", "cas104_4")
)
share <- merge_vars(share, vars11)



# merge change in mental health variables ----------------

vars12 <- list(
  c("cah021_", "cah121_1"), c("camh802_", "camh113_1"),
  c("camh807_", "camh118_1"), c("camh837_", "camh148_")
)
share <- merge_vars(share, vars12)



# tell result ------------

vars_all <- sapply(
  c(vars1, vars2, vars3, vars4, vars5, vars6, vars7, vars8, vars9, vars10, vars11, vars12),
  function(i) i[2]
)

remaining <- intersect(vars_all, colnames(share))
if (length(remaining)) message(paste0("\nNot merged: ", paste0(remaining, collapse = ", ")))



# remove variables that are only measured at wave 8 ------------------

to_remove <- c(
  "cah003_", "cah004_2", "cah004_3", "cah004_4", "cah006_", "cah007_1",
  "cah007_2", "cah007_3", "cah007_4", "cah007_5", "cah011_2", "cah011_4",
  "cah012_", "cah014_", "cah015_", "caw010_", "caw012_", "caw013_",
  "caw016_", "caw017_", "cahh017e", "cae005e", "caco007_", "cas025_",
  "cas026_", "cas027_1", "cas027_2", "cas027_3", "cas028_", "waveid",
  "waveid_hh", "age_int", "agep2020", "mergeidp8",
  "coupleid8", "caho037_", "caph105_", "cah111_3", "cah111_6",
  "cah111_7", "cah111_8", "cah111_11", "cah116_", "cac140_", "cac142_",
  "cac120_1", "cac120_2", "cac120_3", "cac120_4", "cac120_5",
  "cac120_6", "cac120_7", "cac120_8", "cac120_97", "cac120_98",
  "cac122_", "cac130_", "cac131_", "caq130_1", "caq130_2", "caq130_3",
  "caq130_4", "caq130_97", "caq125_", "caq127_", "caq120_", "caq121_",
  "caq122_", "caq118_", "caq119_", "caep005_", "caw111_", "caw117_",
  "caw121_", "caw122_", "caw125_", "cae100_", "cae105e", "cae107e",
  "caco107_", "cas103_5", "cas110_1", "cas110_2", "cas110_3", "cas110_4",
  "cas111_1", "cas111_2", "cas111_3", "cas111_4", "cait104_", "cait105_",
  "cait106_3", "cait106_4", "cait106_5", "cait106_6"
)

to_remove <- setdiff(to_remove, vars_all)
share <- data_remove(share, select = to_remove)



# age ---------------------

share$age <- share$age2020
share$age[share$wave == 9] <- share$age2021[share$wave == 9]
share$age2020 <- NULL
share$age2021 <- NULL


# ID ---------------------

share$hhid <- share$hhid8
share$hhid[share$wave == 9] <- share$hhid9ca[share$wave == 9]
share$hhid8 <- NULL
share$hhid9ca <- NULL



# education -------

share$education2 <- datawizard::to_factor(set_labels(share$education, labels = c("low", "mid", "high")))
share$education2[share$education2 == "NaN"] <- "2"
share$education2 <- droplevels(share$education2)
levels(share$education2) <- c("low", "mid", "high")



# indicate those who participated in both waves ---------------------

singles <- table(share$mergeid)
unique_cases <- names(singles)[which(singles < 2)]
share$both_waves <- 2
share$both_waves[share$mergeid %in% unique_cases] <- 1
share$both_waves <- set_label(share$both_waves, label = "N of waves participated")
share$both_waves <- set_labels(share$both_waves, labels = c("one", "both"))




# COVID severeness, global -------------------

s2 <- data_filter(share, both_waves == 2)

s2 <- s2 |>
  group_by(mergeid) |>
  mutate(cac003_1 = if (isTRUE(any(cac003_1 == 1))) 1
         else if (all(is.na(cac003_1))) NA
         else 0,
         cac005_1 = if (isTRUE(any(cac005_1 == 1))) 1
         else if (all(is.na(cac005_1))) NA
         else 0,
         cac011_1 = if (isTRUE(any(cac011_1 == 1))) 1
         else if (all(is.na(cac011_1))) NA
         else 0) |>
  ungroup()

s2$covid_symptoms_ever <- s2$cac003_1
s2$covid_positive_ever <- s2$cac005_1
s2$covid_hospital_ever <- s2$cac011_1

share <- data_merge(
  share, 
  s2[c("mergeid", "wave", "covid_symptoms_ever", "covid_positive_ever", "covid_hospital_ever")],
  by = c("mergeid", "wave"), 
  join = "left"
)

share$covid_affected_ever <- dplyr::case_when(
  # never covid?
  (is.na(share$covid_symptoms_ever) | share$covid_symptoms_ever == 0) &
  (is.na(share$covid_positive_ever) | share$covid_positive_ever == 0) &
  (is.na(share$covid_hospital_ever) | share$covid_hospital_ever == 0) ~ 1,
  # ever positive, but no symptoms?
  share$covid_positive_ever == 1 &
  (is.na(share$covid_symptoms_ever) | share$covid_symptoms_ever == 0) &
  (is.na(share$covid_hospital_ever) | share$covid_hospital_ever == 0) ~ 2,
  # ever symptoms?
  share$covid_symptoms_ever == 1 &
  (is.na(share$covid_hospital_ever) | share$covid_hospital_ever == 0) ~ 3,
  # hpstitalized?
  share$covid_hospital_ever == 1 ~ 4,
  TRUE ~ as.numeric(NA)
)
share$covid_affected_ever <- set_label(share$covid_affected_ever, label = "Ever affected by Covid, and how severe")
share$covid_affected_ever <- set_labels(share$covid_affected_ever, labels = c("never", "tested positive, no symptoms", "symptoms", "hospitalized"))



# COVID severeness, Waves 8 and 9 -------------------

s8 <- data_filter(share, wave == 8)

s8$covid_symptoms8 <- s8$cac003_1
s8$covid_positive8 <- s8$cac005_1
s8$covid_hospital8 <- s8$cac011_1

s8$covid_affected8 <- dplyr::case_when(
  (is.na(s8$covid_symptoms8) | s8$covid_symptoms8 == 0) & (is.na(s8$covid_positive8) | s8$covid_positive8 == 0) & (is.na(s8$covid_hospital8) | s8$covid_hospital8 == 0) ~ 1,
  s8$covid_positive8 == 1 & (is.na(s8$covid_symptoms8) | s8$covid_symptoms8 == 0) & (is.na(s8$covid_hospital8) | s8$covid_hospital8 == 0) ~ 2,
  s8$covid_symptoms8 == 1 & (is.na(s8$covid_hospital8) | s8$covid_hospital8 == 0) ~ 3,
  s8$covid_hospital8 == 1 ~ 4,
  TRUE ~ as.numeric(NA)
)
s8$covid_affected8 <- set_label(s8$covid_affected8, label = "Affected by Covid until mid 2020, and how severe")
s8$covid_affected8 <- set_labels(s8$covid_affected8, labels = c("never", "tested positive, no symptoms", "symptoms", "hospitalized"))



s9 <- data_filter(share, wave == 9)

s9$covid_symptoms9 <- s9$cac003_1
s9$covid_positive9 <- s9$cac005_1
s9$covid_hospital9 <- s9$cac011_1

s9$covid_affected9 <- dplyr::case_when(
  (is.na(s9$covid_symptoms9) | s9$covid_symptoms9 == 0) & (is.na(s9$covid_positive9) | s9$covid_positive9 == 0) & (is.na(s9$covid_hospital9) | s9$covid_hospital9 == 0) ~ 1,
  s9$covid_positive9 == 1 & (is.na(s9$covid_symptoms9) | s9$covid_symptoms9 == 0) & (is.na(s9$covid_hospital9) | s9$covid_hospital9 == 0) ~ 2,
  s9$covid_symptoms9 == 1 & (is.na(s9$covid_hospital9) | s9$covid_hospital9 == 0) ~ 3,
  s9$covid_hospital9 == 1 ~ 4,
  TRUE ~ as.numeric(NA)
)
s9$covid_affected9 <- set_label(s9$covid_affected9, label = "Affected by Covid until mid 2021, and how severe")
s9$covid_affected9 <- set_labels(s9$covid_affected9, labels = c("never", "tested positive, no symptoms", "symptoms", "hospitalized"))


share <- data_merge(share, s8[c("mergeid", "wave", "covid_symptoms8", "covid_positive8", "covid_hospital8", "covid_affected8")], by = c("mergeid", "wave"), join = "left")
share <- data_merge(share, s9[c("mergeid", "wave", "covid_symptoms9", "covid_positive9", "covid_hospital9", "covid_affected9")], by = c("mergeid", "wave"), join = "left")


# affected by covid over time?

s8$covid_affected <- s8$covid_affected8
s9$covid_affected <- s9$covid_affected9

s89 <- data_join(
  s8[c("mergeid", "covid_affected", "wave")],
  s9[c("mergeid", "covid_affected", "wave")],
  by = NULL,
  join = "bind"
)

share <- data_join(share, s89, by = c("mergeid", "wave"), join = "left")



# social contact frequencies --------------------

# copy original variables

share$contact_children <- share$cas003_1
share$contact_parents <- share$cas003_2
share$contact_relatives <- share$cas003_3
share$contact_friends <- share$cas003_4

# family isolation

share$isolation_family <- NA
share$isolation_family[share$contact_children %in% 1:3 | share$contact_relatives %in% 1:3 |  share$contact_parents %in% 1:3] <- 0
share$isolation_family[share$contact_children > 3 & share$contact_relatives > 3 & share$contact_parents > 3] <- 1
share$isolation_family <- factor(share$isolation_family, labels = c("no", "yes"))

share$isolation_non_family <- NA
share$isolation_non_family[share$contact_friends %in% 1:3] <- 0
share$isolation_non_family[share$contact_friends > 3] <- 1
share$isolation_non_family <- factor(share$isolation_non_family, labels = c("no", "yes"))

share$isolation_any <- NA
share$isolation_any[share$isolation_family == "no" | share$isolation_non_family == "no"] <- 0
share$isolation_any[share$isolation_family == "yes" & share$isolation_non_family == "yes"] <- 1
share$isolation_any <- factor(share$isolation_any, labels = c("no", "yes"))

# social isolation

share$social_isolation <- share$isolation_any
share$social_isolation[share$partnerinhh == 1] <- "no"
share$social_isolation[is.na(share$social_isolation) & share$living_alone == "yes"] <- "yes"



# health ---------------------------

# sphus2

share$sphus2 <- change_code(share$caph003_, recode = list(`0` = 3:5, `1` = 1:2))
share$sphus2 <- set_labels(share$sphus2, labels = c("poor", "excellent"))

share$health_past3months <- change_code(
  share$cah002_,
  recode = list(`0` = c(1, 3), `1` = 2)
)
share$health_past3months <- set_labels(
  share$health_past3months,
  labels = c("same or improved", "worsened")
)



# save data ----------------------------

# main file
save(share, file = "Daten/share_8_9.RData")

# intermediate step file
save(share, file = "Daten/share_8_9-step2.RData")
