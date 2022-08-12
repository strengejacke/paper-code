# gruppierter Datensatz, gruppieren nach Welle und Land, danach
# für jede Welle in jedem Land eine logistische Regression
ess_nested_non_mi <- ess %>%
  group_by(wave, country) %>%
  tidyr::nest() %>%
  ungroup()


# Modellberechnung muss manuell durchgeführt werden, da es Länder und Zeitpunkte
# gibt, in denen alle Fälle aufgrund von Missings wegfallen
res <- list()
for (i in seq_len(nrow(ess_nested_non_mi))) {
  x <- ess_nested_non_mi$data[[i]]
  dummy <- na.omit(data.frame(x$hlthhmp_d, x$aquinc_med, x$agea, x$gndr_n, x$dweight))
  if (nrow(dummy) > 2) {
    fit <- glm(hlthhmp_d ~ aquinc_med + agea + gndr_n,
               family = binomial(link = "logit"),
               data = x,
               weight = dweight)
  } else {
    fit <- NA
  }
  res[[length(res) + 1]] <- fit
}

# Fertig gerechnete Modelle als Variable speichern
ess_nested_non_mi$model.glm <- res


# absolute inequalities für jedes land und jede welle berechnen -----
ess_nested_non_mi$abs_inequ <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  return(100 * (low - hi))
}))

# relative inequalities für jedes land und jede welle berechnen -----
ess_nested_non_mi$rel_inequ <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  return(low / hi)
}))


# absolute inequalities für jedes land und jede welle berechnen, nur männer -----
ess_nested_non_mi$abs_inequ_m <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = 1), type = "response")
  hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = 1), type = "response")
  return(100 * (low - hi))
}))

# relative inequalities für jedes land und jede welle berechnen, nur männer -----
ess_nested_non_mi$rel_inequ_m <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = 1), type = "response")
  hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = 1), type = "response")
  return(low / hi)
}))


# absolute inequalities für jedes land und jede welle berechnen, nur frauen -----
ess_nested_non_mi$abs_inequ_w <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = 2), type = "response")
  hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = 2), type = "response")
  return(100 * (low - hi))
}))

# relative inequalities für jedes land und jede welle berechnen, nur frauen -----
ess_nested_non_mi$rel_inequ_w <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = 2), type = "response")
  hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = 2), type = "response")
  return(low / hi)
}))

# prävalenz functional limitations für jedes land und jede welle berechnen -----
ess_nested_non_mi$fun_lim <-
  100 * unlist(lapply(ess_nested_non_mi$data, function(x) {
    prop.table(table(x$hlthhmp_d))[2]
  }))

# prävalenz functional limitations für jedes land und jede welle berechnen -----
ess_nested_non_mi$fun_lim_m <-
  100 * unlist(lapply(ess_nested_non_mi$data, function(x) {
    prop.table(table(x$hlthhmp_d[x$gndr_n == 1]))[2]
  }))

# prävalenz functional limitations für jedes land und jede welle berechnen -----
ess_nested_non_mi$fun_lim_w <-
  100 * unlist(lapply(ess_nested_non_mi$data, function(x) {
    prop.table(table(x$hlthhmp_d[x$gndr_n == 2]))[2]
  }))

# numerice zeitvariable
ess_nested_non_mi$time <- to_value(ess_nested_non_mi$wave)
