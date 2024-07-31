nako[] <- lapply(nako, function(i) {
  value_lab <- attr(i, "labels", exact = TRUE)
  if (!is.null(value_lab)) {
    value_lab <- setNames(value_lab, gsub("'", "", names(value_lab), fixed = TRUE))
    names(value_lab) <- sjmisc::shorten_string(names(value_lab), 30)
    attr(i, "labels") <- value_lab
  }
  i
})

# speichern ------------------
# save(nako, nako_label, file = "Daten/nako.RData")
