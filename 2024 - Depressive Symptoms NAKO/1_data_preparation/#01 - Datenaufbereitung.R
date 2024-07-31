library(easystats)

# Daten laden
nako <- data_read("./Daten/NAKO-452_export_baseline_14062023.zip")

# Excel mit labels laden
nako_label <- data_read("./Daten/export_baseline_Metadaten.csv")
# nako_label <- readr::read_csv2("./Daten/export_baseline_Metadaten.csv")

# variablen umbenennen
colnames(nako_label) <- c("variable_name", "variable_label", "variable_values", "value_labels")
nako_label <- nako_label[which(nako_label$variable_name %in% colnames(nako)), ]

# variablen label setzen
for (i in nako_label$variable_name) {
  if (i %in% colnames(nako)) {
    nako[[i]] <- assign_labels(
      nako[[i]],
      variable = nako_label$variable_label[nako_label$variable_name == i][1]
    )
  }
}

# missing setzen
nako <- convert_to_na(nako, na = c(-99, -88, -9, 7775:7777, 8886:8888, 9999), verbose = FALSE)
nako <- convert_to_na(nako, na = c(88, 99), select = c(30:39, 107:108), verbose = FALSE)

# werte label setzen
for (i in nako_label$variable_name) {
  if (i %in% colnames(nako)) {
    value_labels <- nako_label$value_labels[nako_label$variable_name == i]
    value_units <- nako_label$variable_values[nako_label$variable_name == i]
    lab <- tryCatch(
      {
        setNames(as.numeric(value_units), gsub("'", "", value_labels, fixed = TRUE))
      },
      warning = function(w) {
      },
      error = function(e) {
      }
    )
    lab <- lab[lab %in% nako[[i]]]
    attr(nako[[i]], "labels") <- lab
  }
}

nako$basis_sex <- as.factor(nako$basis_sex)
levels(nako$basis_sex) <- c("male", "female")

nako <- sjlabelled::drop_labels(nako)

# save(nako, nako_label, file = "Daten/nako.RData")
# sjlabelled::write_spss(nako, "Daten/nako.sav")


# sjPlot::view_df(nako)
