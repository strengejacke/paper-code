land_reihe <- c(2, 9, 7, 13, 3, 16, 4, 10, 12, 11, 15, 8, 14, 5, 6, 1)

dummy1 <- ess %>%
  group_by(country) %>%
  filter(gndr_n == 1) %>%
  summarise(`Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))))

# ------------------------------------------------------------------
# Datensatz nach Ländern gruppieren und Wellen gruppieren und dann
# den Range von Alter, Einkommensmissings, Anteil weibl. Geschlecht,
# Anteil funct. limit. über ale Wellen berechnen
# ------------------------------------------------------------------
dummy2 <- ess %>%
  group_by(country, wave) %>%
  filter(gndr_n == 1) %>%
  summarise(FL = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d))))) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(min.fl = min(FL), max.fl = max(FL))

dummy3 <- ess %>%
  ungroup() %>%
  filter(gndr_n == 1) %>%
  summarise(`Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))))

deskr <- cbind(dummy1, dummy2[, -1])[land_reihe, ]
funlim <- sprintf("%i (%i-%i)", deskr$`Function Limitations`, deskr$min.fl, deskr$max.fl)

gesamt <- tibble::tibble(country = c(as.character(deskr$country), "All"),
                     `Function Limitations (male, in %)` = c(funlim, dummy3$`Function Limitations`))


dummy1 <- ess %>%
  group_by(country) %>%
  filter(gndr_n == 2) %>%
  summarise(`Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))))

# ------------------------------------------------------------------
# Datensatz nach Ländern gruppieren und Wellen gruppieren und dann
# den Range von Alter, Einkommensmissings, Anteil weibl. Geschlecht,
# Anteil funct. limit. über ale Wellen berechnen
# ------------------------------------------------------------------
dummy2 <- ess %>%
  group_by(country, wave) %>%
  filter(gndr_n == 2) %>%
  summarise(FL = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d))))) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(min.fl = min(FL), max.fl = max(FL))

dummy3 <- ess %>%
  ungroup() %>%
  filter(gndr_n == 2) %>%
  summarise(`Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))))

deskr <- cbind(dummy1, dummy2[, -1])[land_reihe, ]
funlim <- sprintf("%i (%i-%i)", deskr$`Function Limitations`, deskr$min.fl, deskr$max.fl)


gesamt2 <- tibble::tibble(`Function Limitations (female, in %)` = c(funlim, dummy3$`Function Limitations`))

sjPlot::tab_df(bind_cols(gesamt, gesamt2))
