library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(parameters)
library(effectsize)

# Daten laden -----------------------

load("DEAS 1996-2017 (nur Drop-Off).RData")



# Ausnahmen -----------------------------

deas_2008$ehramt_weit_08 <- deas_2008$ehramt_08



# Variablenübersicht aus Excel einlesen -------------------------

uebersicht_drop_off <- readxl::read_excel("../DEAS Übersicht 1996-2017/DEAS_1996-2017_Variablenkorrespondenz_de_en.xlsx", sheet = 3)
uebersicht_capi <- readxl::read_excel("../DEAS Übersicht 1996-2017/DEAS_1996-2017_Variablenkorrespondenz_de_en.xlsx", sheet = 2)
uebersicht_additional <- readxl::read_excel("../DEAS Übersicht 1996-2017/DEAS_1996-2017_Variablenkorrespondenz_de_en.xlsx", sheet = 4)



# Variablen 2017 ---------------------

vars_2017 <- c("fallnum", "stich", "natdeutsch_17", "migrat_17", "westost_17",
               "bland_17", "bikgk10_17", "alter_17", "isced_17", "erw_17",
               "siops_17", "siops_kat_17", "isei_17", "schicht_17", "aee_oecd_17",
               "partner_17", "wekind_17", "nwgroesse_17", "bmi_17", "sf36_17",
               "anzphy_17", "depressiv_17", "lone6_17", "lz_17", "pa_17", "na_17",
               "optimismus_17", "autonomie_17", "sok_17", "ehramt_17", "ehramt_weit_17",
               "selbstwert_17", "selbstwirk_17", "zzrscore_17", "famstand_17",
               "ic526a_1", "ic526a_2", "ic526a_3", "id24_1", "exklusion_17",
               "ic501", "ic539", "ic503aa", paste0("ic511_", 1:15), "exkind_17", "anzkind_17",
               paste0("ic518a", 1:18), paste0("id28_", 1:12, "1"), "id42_1", "id61"
               )



# Zeilenindices aus Excel für Variablen aus 2017 auslesen --------------------

# 14 Variablen aus dem drop-off
vars_2017_dropoff <- uebersicht_drop_off[[14]] %in% vars_2017
# 36 Variablen aus dem capi
vars_2017_capi <- uebersicht_capi[[14]] %in% vars_2017
# 30 Variablen aus dem drop-off
vars_2017_additional <- uebersicht_additional[[8]] %in% vars_2017

# Variablen neu sortieren

vars_2017 <- uebersicht_drop_off[[14]][vars_2017_dropoff]
vars_2017 <- c(vars_2017, uebersicht_capi[[14]][vars_2017_capi])
vars_2017 <- c(vars_2017, uebersicht_additional[[8]][vars_2017_additional])

deas_2017 <- dplyr::select(deas_2017, !! vars_2017)


# variablen 2014 ---------------------

# Nun Variablennamen aus 2014 aus Excel auslesen,
# basierend auf Variablennamen aus 2017

vars_2014 <- uebersicht_drop_off[[12]][vars_2017_dropoff]
vars_2014 <- c(vars_2014, uebersicht_capi[[12]][vars_2017_capi])
vars_2014 <- c(vars_2014, uebersicht_additional[[7]][vars_2017_additional])
vars_2014_complete <- unique(na.omit(vars_2014))

deas_2014 <- dplyr::select(deas_2014, !! vars_2014_complete)



# variablen 2011 ---------------------

# 13, es fehlt id42_1
vars_2011 <- uebersicht_drop_off[[10]][vars_2017_dropoff]
# 33, es fehlen ic503aa, ic511_11 und ic511_13
vars_2011 <- c(vars_2011, uebersicht_capi[[10]][vars_2017_capi])
# 28, es fehlen "autonomie" und "sok"
vars_2011 <- c(vars_2011, uebersicht_additional[[6]][vars_2017_additional])
vars_2011_complete <- unique(na.omit(vars_2011))

deas_2011 <- dplyr::select(deas_2011, !! vars_2011_complete)



# variablen 2008 ---------------------

# 13, es fehlt id42_1
vars_2008 <- uebersicht_drop_off[[8]][vars_2017_dropoff]
# 33, es fehlen ic503aa, ic511_11 und ic511_13
vars_2008 <- c(vars_2008, uebersicht_capi[[8]][vars_2017_capi])
# 27,  fehlen "autonomie", "sok" und "ehramt_weit_17"
vars_2008 <- c(vars_2008, uebersicht_additional[[5]][vars_2017_additional])
vars_2008_complete <- unique(na.omit(vars_2008))

deas_2008 <- dplyr::select(deas_2008, !! vars_2008_complete)



# variablen 2002 ---------------------

# brauchen wir nur, um missings aufzufüllen
vars_2002 <- uebersicht_drop_off[[6]][vars_2017_dropoff]
vars_2002 <- c(vars_2002, uebersicht_capi[[6]][vars_2017_capi])
vars_2002 <- c(vars_2002, uebersicht_additional[[4]][vars_2017_additional])
vars_2002_complete <- unique(na.omit(vars_2002))

deas_2002 <- dplyr::select(deas_2002, !! vars_2002_complete)



# variablen 1996 ---------------------

# brauchen wir nur, um missings aufzufüllen
vars_1996 <- uebersicht_drop_off[[4]][vars_2017_dropoff]
vars_1996 <- c(vars_1996, uebersicht_capi[[4]][vars_2017_capi])
vars_1996 <- c(vars_1996, uebersicht_additional[[3]][vars_2017_additional])
vars_1996_complete <- unique(na.omit(vars_1996))

deas_1996 <- dplyr::select(deas_1996, !! vars_1996_complete)



# Variablennamen vereinheitlichen --------------------

colnames(deas_1996) <- vars_2017[!is.na(vars_1996)]
colnames(deas_2002) <- vars_2017[!is.na(vars_2002)]
colnames(deas_2008) <- vars_2017[!is.na(vars_2008)]
colnames(deas_2011) <- vars_2017[!is.na(vars_2011)]
colnames(deas_2014) <- vars_2017[!is.na(vars_2014)]



# Gesamtdatensatz erstellen ---------------------------

deas_2008$year <- 2008
deas_2011$year <- 2011
deas_2014$year <- 2014
deas_2017$year <- 2017

deas_gesamt <- merge_df(deas_2008, deas_2011, deas_2014, deas_2017, id = "Jahr")
deas_gesamt <- drop_labels(deas_gesamt)



# missings auffüllen -----------------

items <- c("isced_17", "natdeutsch_17", "migrat_17", "siops_17", "siops_kat_17",
           "isei_17", "schicht_17", "wekind_17")

for (item in items) {

  # nur Fallnummer und variable auswählen aus allen Wellen

  if (item != "migrat_17") {
    l <- list(
      deas_1996[, c("fallnum", item)],
      deas_2002[, c("fallnum", item)],
      deas_2008[, c("fallnum", item)],
      deas_2011[, c("fallnum", item)],
      deas_2014[, c("fallnum", item)],
      deas_2017[, c("fallnum", item)]
    )
  } else {
    l <- list(
      deas_2002[, c("fallnum", item)],
      deas_2008[, c("fallnum", item)],
      deas_2011[, c("fallnum", item)],
      deas_2014[, c("fallnum", item)],
      deas_2017[, c("fallnum", item)]
    )
  }


  # Alle Datensätze mergen, nach Fallnummer

  x <- Reduce(function(x, y) merge(x, y, by = "fallnum", all = TRUE, sort = TRUE), l)


  # nur die variablen-werte, die keinen missing haben

  items_complete <- data.frame(
    fallnum = x$fallnum,
    new_item = unlist(apply(x[, -1], 1, function(i) {
      i <- i[!is.na(i)]
      if (length(i) > 0)
        max(i)
      else
        NA
    }))
  )


  # Neue ISCED mit aufgefüllten missings nach Fallnummer mit Hauptdaten mergen

  dummy <- merge(deas_gesamt, items_complete, by = "fallnum")
  deas_gesamt[[item]] <- dummy[["new_item"]]
}



# Datensatz mit Variablen, die in allen Wellen vorhanden sind -------------------

complete_vars <- c("year", "Jahr", vars_2008)
vars_2017 <- c("year", "Jahr", vars_2017)
deas_gesamt_complete <- deas_gesamt[vars_2017[!is.na(complete_vars)]]



# DEAS Meta Daten anhängen -----------------------

omit_from_meta <- c("part_96", "part_02", "part_08", "part_11", "part_14",
                    "part_17", "part", "lastpart", "n_obs", "fiktiv_kreis_96",
                    "fiktiv_kreis_02", "fiktiv_kreis_08", "fiktiv_kreis_11",
                    "fiktiv_kreis_14", "fiktiv_kreis_17", "rlc2002_kat", "rlc2008_kat",
                    "rlc2011_kat", "rlc2014_kat", "rlc2017_kat")
deas_meta[omit_from_meta] <- NULL

deas_gesamt <- suppressWarnings(left_join(deas_gesamt, deas_meta, by = "fallnum"))
deas_gesamt_complete <- suppressWarnings(left_join(deas_gesamt_complete, deas_meta, by = "fallnum"))



# DEAS Panel-Gewichte erstellen -----------------------

# siehe "DEAS-Datengewichtung_Stand_6-2019.pdf"
create_panel_weights <- function(dw) {
  # Gewichtung für Teilnahme ab 1998

  gew96 <- dw$lsdrop96_02
  i <- which(!is.na(gew96) & !is.na(dw$pbleibdrop_02))
  gew96[i] <- gew96[i] * dw$pbleibdrop_02[i]
  i <- which(!is.na(gew96) & !is.na(dw$pbleibdrop_08))
  gew96[i] <- gew96[i] * dw$pbleibdrop_08[i]
  i <- which(!is.na(gew96) & !is.na(dw$pbleibdrop_11))
  gew96[i] <- gew96[i] * dw$pbleibdrop_11[i]
  i <- which(!is.na(gew96) & !is.na(dw$pbleibdrop_14))
  gew96[i] <- gew96[i] * dw$pbleibdrop_14[i]
  i <- which(!is.na(gew96) & !is.na(dw$pbleibdrop_17))
  gew96[i] <- gew96[i] * dw$pbleibdrop_17[i]

  # Gewichtung für Teilnahme ab 2002

  gew02 <- dw$qspsdrop_02
  i <- which(!is.na(gew02) & !is.na(dw$pbleibdrop_08))
  gew02[i] <- gew02[i] * dw$pbleibdrop_08[i]
  i <- which(!is.na(gew02) & !is.na(dw$pbleibdrop_11))
  gew02[i] <- gew02[i] * dw$pbleibdrop_11[i]
  i <- which(!is.na(gew02) & !is.na(dw$pbleibdrop_14))
  gew02[i] <- gew02[i] * dw$pbleibdrop_14[i]
  i <- which(!is.na(gew02) & !is.na(dw$pbleibdrop_17))
  gew02[i] <- gew02[i] * dw$pbleibdrop_17[i]

  # Gewichtung für Teilnahme ab 2008

  gew08 <- dw$qspsdrop_08
  i <- which(!is.na(gew08) & !is.na(dw$pbleibdrop_11))
  gew08[i] <- gew08[i] * dw$pbleibdrop_11[i]
  i <- which(!is.na(gew08) & !is.na(dw$pbleibdrop_14))
  gew08[i] <- gew08[i] * dw$pbleibdrop_14[i]
  i <- which(!is.na(gew08) & !is.na(dw$pbleibdrop_17))
  gew08[i] <- gew08[i] * dw$pbleibdrop_17[i]

  # Gewichtung für Teilnahme ab 2011

  gew11 <- dw$qspsdrop_11
  i <- which(!is.na(gew11) & !is.na(dw$pbleibdrop_14))
  gew11[i] <- gew11[i] * dw$pbleibdrop_14[i]
  i <- which(!is.na(gew11) & !is.na(dw$pbleibdrop_17))
  gew11[i] <- gew11[i] * dw$pbleibdrop_17[i]

  # Gewichtung für Teilnahme ab 2014

  gew14 <- dw$qspsdrop_14
  i <- which(!is.na(gew14) & !is.na(dw$pbleibdrop_17))
  gew14[i] <- gew14[i] * dw$pbleibdrop_17[i]

  # Gewichtung für Teilnahme ab 2017

  gew17 <- dw$qspsdrop_17

  # Gesamtgewicht

  gew <- gew96
  gew[is.na(gew)] <- gew02[is.na(gew)]
  gew[is.na(gew)] <- gew08[is.na(gew)]
  gew[is.na(gew)] <- gew11[is.na(gew)]
  gew[is.na(gew)] <- gew14[is.na(gew)]
  gew[is.na(gew)] <- gew17[is.na(gew)]

  gew
}

dw <- read_spss("../SPSS-Daten/DEAS1996-2017_Weights_en_SPSS.sav")
dw <- dplyr::filter(dw, fallnum %in% deas_gesamt$fallnum)
dw$pdrop_weight <- create_panel_weights(dw)

deas_gesamt <- suppressWarnings(left_join(deas_gesamt, dw[c("fallnum", "pdrop_weight")], by = "fallnum"))
deas_gesamt_complete <- suppressWarnings(left_join(deas_gesamt_complete, dw[c("fallnum", "pdrop_weight")], by = "fallnum"))

drop_weight <- rescale_weights(deas_gesamt, group = "bland_17", probability_weights = "pdrop_weight")$pweights_a
deas_gesamt$drop_weight <- drop_weight
deas_gesamt_complete$drop_weight <- drop_weight

set_label(deas_gesamt$pdrop_weight) <- "Panel Probability Weights (Dropoff)"
set_label(deas_gesamt$drop_weight) <- "Panel Rescaled Weights (Dropoff)"

set_label(deas_gesamt_complete$pdrop_weight) <- "Panel Probability Weights (Dropoff)"
set_label(deas_gesamt_complete$drop_weight) <- "Panel Rescaled Weights (Dropoff)"



# Kreistyp als eine Variable erstellen ----------------------

kreistyp_erstellen <- function(x) {
  x$bbsr_kreistyp <- x$bbsr_kreistyp96
  x$bbsr_kreistyp[!is.na(x$bbsr_kreistyp02)] <- x$bbsr_kreistyp02[!is.na(x$bbsr_kreistyp02)]
  x$bbsr_kreistyp[!is.na(x$bbsr_kreistyp08)] <- x$bbsr_kreistyp08[!is.na(x$bbsr_kreistyp08)]
  x$bbsr_kreistyp[!is.na(x$bbsr_kreistyp11)] <- x$bbsr_kreistyp11[!is.na(x$bbsr_kreistyp11)]
  x$bbsr_kreistyp[!is.na(x$bbsr_kreistyp14)] <- x$bbsr_kreistyp14[!is.na(x$bbsr_kreistyp14)]
  x$bbsr_kreistyp[!is.na(x$bbsr_kreistyp17)] <- x$bbsr_kreistyp17[!is.na(x$bbsr_kreistyp17)]
  x
}

deas_gesamt <- kreistyp_erstellen(deas_gesamt)
deas_gesamt_complete <- kreistyp_erstellen(deas_gesamt_complete)



# Doppelte Variablen entfernen -----------------

doppelte_entfernen <- function(x) {
  x$isced_17 <- NULL
  x$migrat_17 <- NULL
  x$natdeutsch_17 <- NULL
  x$stich <- x$stich.y
  x$stich.x <- NULL
  x$stich.y <- NULL
  x$bbsr_kreistyp96 <- NULL
  x$bbsr_kreistyp02 <- NULL
  x$bbsr_kreistyp08 <- NULL
  x$bbsr_kreistyp11 <- NULL
  x$bbsr_kreistyp14 <- NULL
  x$bbsr_kreistyp17 <- NULL
  x
}

deas_gesamt <- doppelte_entfernen(deas_gesamt)
deas_gesamt_complete <- doppelte_entfernen(deas_gesamt_complete)



# Alter letzte Welle -----------------

deas_gesamt$alter_aktuell <- 2017 - deas_gesamt$yob
deas_gesamt_complete$alter_aktuell <- 2017 - deas_gesamt_complete$yob

set_label(deas_gesamt$alter_aktuell) <- "Alter (letzte Erhebungswelle 2017)"
set_label(deas_gesamt_complete$alter_aktuell) <- "Alter (letzte Erhebungswelle 2017)"



# Variable Welle, und Zeit in Jahren -----------------

zeit_variablen <- function(x) {
  x$Jahr <- as.factor(x$Jahr)
  x$welle <- as.numeric(x$Jahr)
  x$zeit <- recode_to(x$year)

  set_label(x$zeit) <- "Zeitverlauf in Jahren (t0 = 2008)"
  set_label(x$welle) <- "Erhebungswelle (1-4)"
  x$welle <- set_labels(x$welle, labels = c("2008", "2011", "2014", "2017"))

  x
}


deas_gesamt <- zeit_variablen(deas_gesamt)
deas_gesamt_complete <- zeit_variablen(deas_gesamt_complete)



# Recodings und in kategorial umwandel speichern -----------------

in_kategorial <- function(x) {
  x$optimismus100 <- 100 * normalize(x$optimismus)
  x <- x %>%
    drop_labels() %>%
    rec(migrat, rec = "0=0;1,2=1", as.num = FALSE, val.labels = c("Kein Migrant", "Migrant")) %>%
    to_factor(
      id28_11:id28_121, ic511_1:ic539, isced, westost_17, bland_17, erw_17,
      partner_17, exkind_17, ehramt_17, gender, natdeutsch, migrat, vitalstatus_last
    )

  x$exkind_17 <- set_labels(x$exkind_17, labels = c("keine Kinder", "Kinder"))
  x$srh <- rec(x$ic501, rec = "rev")

  if ("famstand_17" %in% colnames(x)) {
    x$marital_status <- rec(x$famstand_17, rec = "1=0;2:5=1;else=copy", var.label = "Marital Status", val.labels = c("Living together", "not in same household"), as.num = FALSE)
  }

  x
}


deas_gesamt <- in_kategorial(deas_gesamt)
deas_gesamt_complete <- in_kategorial(deas_gesamt_complete)



# "_17" von Variablennamen entfernen -----------------

label_cleaning <- function(x) {
  colnames(x) <- gsub("(.*)_17$", "\\1", colnames(x))

  set_label(x$siops) <- "SIOPS Berufsprestige (paarbezogen)"
  set_label(x$siops_kat) <- "SIOPS Berufsprestige (paarbezogen, kategorisiert)"
  set_label(x$isei) <- "ISEI-Status (paarbezogen)"
  set_label(x$schicht) <- "Sozialschicht (paarbezogen)"
  set_label(x$wekind) <- "Wohnentfernung naechstes Kind"
  set_label(x$aee_oecd) <- "Equi income (in EUR)"
  set_label(x$isced) <- "Bildungsstatus (ISCED)"
  set_label(x$erw) <- "Labour status"
  set_label(x$partner) <- "Partnership"
  set_label(x$migrat) <- "Migratory background"
  set_label(x$migrat_r) <- "Migratory background"
  set_label(x$ehramt) <- "Voluntary work"
  set_label(x$lone6) <- "Loneliness"
  set_label(x$srh) <- "Self-rated health"
  set_label(x$zzrscore) <- "Cognitive Functioning"

  x$isced <- set_labels(x$isced, labels = c("low", "middle", "high"))
  x$migrat <- set_labels(x$migrat, labels = c("Kein Migrant", "Immigriert", "Migrant (nicht imm.)"))
  x$partner <- set_labels(x$partner, labels = c("Single", "selber Haushalt", "anderer Haushalt"))
  x$erw <- set_labels(x$erw, labels = c("working", "retired", "not employed"))

  for (i in 1:ncol(x)) {
    label <- attr(x[[i]], "label", exact = TRUE)
    if (!is.null(label)) {
      label <- gsub("f([0-9a-z_]+)(\\s+)(.*)", "\\3", label)
      label <- gsub("x([0-9a-z_]+)(\\s+)(.*)", "\\3", label)
      label <- gsub("- ", "", label, fixed = TRUE)
      attr(x[[i]], "label") <- label
    }
  }

  x
}

deas_gesamt <- label_cleaning(deas_gesamt)
deas_gesamt_complete <- label_cleaning(deas_gesamt_complete)



# outlier raus ------------------------------------

deas_gesamt <- deas_gesamt[is.na(deas_gesamt$aee_oecd) | deas_gesamt$aee_oecd < 10000, ]
deas_gesamt_complete <- deas_gesamt_complete[is.na(deas_gesamt_complete$aee_oecd) | deas_gesamt_complete$aee_oecd < 10000, ]



# z-standardisieren, de-meaning -----------------

deas_gesamt <- cbind(
  deas_gesamt,
  demean(deas_gesamt, select = c("anzphy", "sf36", "optimismus", "optimismus100", "lone6", "aee_oecd", "ic501", "srh", "depressiv", "bmi", "lz", "selbstwert", "selbstwirk", "zzrscore", "exklusion", "alter"), group = "fallnum")
)
deas_gesamt_complete <- cbind(
  deas_gesamt_complete,
  demean(deas_gesamt_complete, select = c("anzphy", "sf36", "optimismus", "optimismus100", "lone6", "aee_oecd", "ic501", "srh", "depressiv", "bmi", "lz", "selbstwert", "selbstwirk", "zzrscore", "alter"), group = "fallnum")
)

deas_gesamt <- std(
  deas_gesamt, alter_aktuell, anzphy, aee_oecd, aee_oecd_within, aee_oecd_between,
  siops, isei, schicht, wekind, nwgroesse, bmi, depressiv, sf36, lz, optimismus,
  optimismus_within, optimismus_between, optimismus100_within, optimismus100_between,
  optimismus100, lone6, lone6_within, lone6_between, pa, na, srh, srh_within,
  srh_between, ic501, ic501_within, ic501_between, depressiv, depressiv_within,
  depressiv_between, bmi_between, bmi_within, lz_between, lz_within, selbstwert,
  selbstwert_within, selbstwert_between, selbstwirk, selbstwirk_within,
  selbstwirk_between, zzrscore, zzrscore_within, zzrscore_between, exklusion,
  exklusion_within, exklusion_between, anzphy_within, anzphy_between,
  sf36_within, sf36_between, alter_between, alter_within, alter
)
deas_gesamt_complete <- std(
  deas_gesamt_complete, alter_aktuell, anzphy, aee_oecd, aee_oecd_within, aee_oecd_between,
  siops, isei, schicht, wekind, nwgroesse, bmi, depressiv, sf36, lz, optimismus,
  optimismus_within, optimismus_between, optimismus100_within, optimismus100_between,
  optimismus100, lone6, lone6_within, lone6_between, pa, na, srh, srh_within,
  srh_between, ic501, ic501_within, ic501_between, depressiv, depressiv_within,
  depressiv_between, bmi_between, bmi_within, lz_between, lz_within, selbstwert,
  selbstwert_within, selbstwert_between, selbstwirk, selbstwirk_within,
  selbstwirk_between, zzrscore, zzrscore_within, zzrscore_between,
  anzphy_within, anzphy_between, sf36_within, sf36_between,
  alter_between, alter_within, alter
)

deas_gesamt <- std(
  deas_gesamt, alter_aktuell, anzphy, aee_oecd, aee_oecd_within, aee_oecd_between,
  siops, isei, schicht, wekind, nwgroesse, bmi, depressiv, sf36, lz, optimismus,
  optimismus_within, optimismus_between, optimismus100_within, optimismus100_between,
  optimismus100, lone6, lone6_within, lone6_between, pa, na, srh, srh_within,
  srh_between, ic501, ic501_within, ic501_between, depressiv, depressiv_within,
  depressiv_between, bmi_between, bmi_within, lz_between, lz_within, selbstwert,
  selbstwert_within, selbstwert_between, selbstwirk, selbstwirk_within,
  selbstwirk_between, zzrscore, zzrscore_within, zzrscore_between, exklusion,
  exklusion_within, exklusion_between, anzphy_within, anzphy_between,
  sf36_within, sf36_between, alter_between, alter_within, alter,
  robust = "2sd", suffix = "_z2"
)
deas_gesamt_complete <- std(
  deas_gesamt_complete, alter_aktuell, anzphy, aee_oecd, aee_oecd_within, aee_oecd_between,
  siops, isei, schicht, wekind, nwgroesse, bmi, depressiv, sf36, lz, optimismus,
  optimismus_within, optimismus_between, optimismus100_within, optimismus100_between,
  optimismus100, lone6, lone6_within, lone6_between, pa, na, srh, srh_within,
  srh_between, ic501, ic501_within, ic501_between, depressiv, depressiv_within,
  depressiv_between, bmi_between, bmi_within, lz_between, lz_within, selbstwert,
  selbstwert_within, selbstwert_between, selbstwirk, selbstwirk_within,
  selbstwirk_between, zzrscore, zzrscore_within, zzrscore_between,
  anzphy_within, anzphy_between, sf36_within, sf36_between,
  alter_between, alter_within, alter,
  robust = "2sd", suffix = "_z2"
)



# Daten speichern -----------------

save(deas_gesamt, deas_gesamt_complete, file = "DEAS 2008-2017, Dropoff, Variablenauswahl.RData")

write_spss(deas_gesamt, "DEAS 2008-2017, Dropoff, nicht alle Wellen complete.sav")
write_spss(deas_gesamt_complete, "DEAS 2008-2017, Dropoff, alle Wellen complete.sav")

# in deas_gesamt_complete fehlen:
# id42_1, ic503aa, ic511_11, ic511_13, "autonomie", "sok", "ehramt_weit_17"



# Descriptive Statistik -----------------

descr(deas_gesamt_complete, show = c("label", "n", "NA.prc", "mean", "sd", "range"), out = "b")
# descr(deas_gesamt, show = c("label", "n", "NA.prc", "mean", "sd", "range"), out = "b")



# Codeplan ----------------------------

view_df(deas_gesamt_complete, show.values = TRUE, show.frq = TRUE, show.prc = TRUE, show.na = TRUE)
