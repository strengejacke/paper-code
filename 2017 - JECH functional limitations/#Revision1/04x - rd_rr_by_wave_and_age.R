rd_rr_by_wave_and_gender <- function(welle = 7, alter = 0) {
  # gruppierter Datensatz, gruppieren nach Welle und Land, danach
  # für jede Welle in jedem Land eine logistische Regression
  ess_nested <- ess %>%
    filter(wave == welle) %>%
    group_by(country) %>%
    tidyr::nest() %>%
    ungroup()


  # Modellberechnung muss manuell durchgeführt werden, da es Länder und Zeitpunkte
  # gibt, in denen alle Fälle aufgrund von Missings wegfallen
  res <- list()
  for (i in seq_len(nrow(ess_nested))) {
    x <- filter(ess_nested$data[[i]], age_dicho2 == alter)
    dummy <- na.omit(data.frame(x$hlthhmp_d, x$aquinc_med, x$agea, x$gndr_n))
    if (nrow(dummy) > 2) {
      fit <- glm(hlthhmp_d ~ aquinc_med + agea + gndr_n,
                 family = binomial(link = "logit"),
                 data = x)
    } else {
      fit <- NA
    }
    res[[length(res) + 1]] <- fit
  }

  # Fertig gerechnete Modelle als Variable speichern
  ess_nested$model.glm <- res


  # absolute inequalities für jedes land und jede welle berechnen -----
  ess_nested$abs_inequ <- unlist(lapply(ess_nested$model.glm, function(x) {
    if (is.na(x)) return(NA)
    mf <- model.frame(x)
    low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
    hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
    return(100 * (low - hi))
  }))

  # relative inequalities für jedes land und jede welle berechnen -----
  ess_nested$rel_inequ <- unlist(lapply(ess_nested$model.glm, function(x) {
    if (is.na(x)) return(NA)
    mf <- model.frame(x)
    low <- predict(x, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
    hi <- predict(x, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
    return(low / hi)
  }))


  t1 <- ess_nested %>%
    select(country, abs_inequ)

  t1$country <- to_character(t1$country)
  t1 <- t1[order(t1$country), ]
  t1$abs_inequ <- sprintf("%.2f", t1$abs_inequ)

  t2 <- ess_nested %>%
    select(country, rel_inequ)

  t2$country <- to_character(t2$country)
  t2 <- t2[order(t2$country), ]
  t2$rel_inequ <- sprintf("%.2f", t2$rel_inequ)

  t3 <- ess_nested %>%
    ungroup() %>%
    mutate(pval = unlist(lapply(model.glm, function(x) {
      summary(x)$coefficients[2, 4]
    })))

  t3 <- data.frame(country = to_character(t3$country), pval = t3$pval)
  t3 <- t3[order(t3$country), ]
  t3$pval <- sprintf("%.3f", t3$pval)

  list(t1 = t1, t2 = t2, t3 = t3)
}
