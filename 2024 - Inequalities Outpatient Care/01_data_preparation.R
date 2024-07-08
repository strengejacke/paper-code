library(easystats)

forsa <- data_read("Daten/Forsa_20230913.sav", convert_factors = FALSE)
depr <- data_read("Daten/GISD_PLZ_5_final_2019.xlsx")

forsa$f31_neu <- convert_to_na(forsa$f31, na = 5)
forsa$f33_neu <- convert_to_na(forsa$f33, na = 5)
forsa$f36_neu <- convert_to_na(forsa$f36, na = 5)
forsa$f40_neu <- convert_to_na(forsa$f40, na = 5)
forsa$f41_neu <- convert_to_na(forsa$f41, na = 5)
forsa$f42_neu <- convert_to_na(forsa$f42, na = 3)
forsa$f49_neu <- convert_to_na(forsa$f49, na = 5)
forsa$f51_neu <- convert_to_na(forsa$f51, na = 5)
forsa$f54_neu <- convert_to_na(forsa$f54, na = 5)
forsa$f58_neu <- convert_to_na(forsa$f58, na = 5)
forsa$f59_neu <- convert_to_na(forsa$f59, na = 5)

forsa$gp_score_comm <- forsa |>
  data_select(c("f33_neu", "f36_neu", "f40_neu", "f41_neu")) |>
  row_means(min_valid = 2) |>
  round(3)

forsa$gp_score_comm <- assign_labels(
  forsa$gp_score_comm,
  variable = "Score Aspects of Communication (GP)"
)

forsa$spec_score_comm <- forsa |>
  data_select(c("f51_neu", "f54_neu", "f58_neu", "f59_neu")) |>
  row_means(min_valid = 2) |>
  round(3)

forsa$spec_score_comm <- assign_labels(
  forsa$spec_score_comm,
  variable = "Score Aspects of Communication (Specialist)"
)

# reverse scores
forsa$spec_score_comm_r <- 5 - forsa$spec_score_comm
forsa$gp_score_comm_r <- 5 - forsa$gp_score_comm

forsa$insurance <- recode_values(forsa$f1_neu, recode = list(`1` = 1, `2` = 2:3))
forsa$insurance <- assign_labels(
  forsa$insurance,
  values = c("gesetzlich", "privat"),
  variable = "Versicherungsstatus"
)

forsa <- data_rename(
  forsa,
  pattern = c(
    "nf26_neu", "nf44_neu", "nf27_neu", "nf45_neu", "nf28_neu", "nf46_neu",
    "ges", "ISCED_dreistufig", "ISCED_97", "ISCED_NAKO", "Alter_gruppiert",
    "MH_Gen", "Aequi_terz", "Aequi_quart", "Aequi_quint", "f43ausw_neu",
    "xweight1", "f42_neu", "f31_neu", "f49_neu", "CASMIN_vierstufig",
    "CASMIN_deistufig", "CASMIN_deistufig_1"
  ),
  replacement = c(
    "gp_traveltime", "spec_traveltime", "gp_waitingtime", "spec_waitingtime",
    "gp_consultationtime", "spec_consultationtime", "sex", "education3",
    "education8", "education4", "age3", "migbackground", "income3", "income4",
    "income5", "spec_type", "weights", "spec_usage", "gp_enough_time",
    "spec_enough_time", "casmin4", "casmin3", "casmin3_gesis"
  )
)

forsa <- to_factor(
  forsa,
  select = c(
    "education3", "casmin4", "age3", "sex", "migbackground", "income3",
    "income4", "insurance", "spec_type", "spec_usage", "casmin3_gesis",
    "casmin3"
  )
)

levels(forsa$spec_type) <- c(
  "Augenarzt", "Chirurgen", "Frauenarzt", "Hals-Nasen-Ohren-Arzt",
  "Hautarzt", "Internisten", "Neurologen, Psychiater", "Orthopäden",
  "Radiologen", "Urologen", "Anderer Facharzt"
)

depr <- data_filter(depr, year == 2019)

# create new variable "deprindex" in forsa data
forsa$deprindex <- NA

# find matching PLZ5 in forsa data "plz_neu" copy value from depr$gsid_5 into forsa$deprindex
for (i in seq_len(nrow(forsa))) {
  for (j in seq_len(nrow(depr))) {
    if (forsa$plz_neu[i] == depr$PLZ5[j]) {
      forsa$deprindex[i] <- depr$gisd_5[j]
      break
    }
  }
}


forsa$deprindex_k <- NA
for (i in seq_len(nrow(forsa))) {
  for (j in seq_len(nrow(depr))) {
    if (forsa$plz_neu[i] == depr$PLZ5[j]) {
      forsa$deprindex_k[i] <- depr$gisd_k[j]
      break
    }
  }
}

forsa_small <- data_select(
  forsa,
  select = c(
    "gp_traveltime", "spec_traveltime", "gp_waitingtime", "spec_waitingtime",
    "gp_consultationtime", "spec_consultationtime", "gp_score_comm",
    "spec_score_comm", "gp_score_comm_r", "spec_score_comm_r", "spec_type",
    "spec_usage", "gp_enough_time", "spec_enough_time", "deprindex",
    "deprindex_k", "sex", "education3", "education8", "education4", "casmin3",
    "casmin3_gesis", "casmin4", "age3", "migbackground", "income3", "income4",
    "income5", "insurance", "weights"
  )
)

save(forsa, forsa_small, file = "Daten/forsa.RData")
data_write(forsa_small, "forsa_klein.sav")