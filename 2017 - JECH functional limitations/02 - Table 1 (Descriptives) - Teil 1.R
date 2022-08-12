library(strengejacke)
library(tidyverse)


# ------------------------------------------------------------------
# Datensatz nach Ländern gruppieren (also je ein Land über alle Wellen
# aggregiert) und dann Alter, Einkommensmissings, Anteil weibl. Geschlecht,
# Anteil funct. limit. über ale Wellen berechnen
# ------------------------------------------------------------------
dummy1 <- ess %>%
  group_by(country) %>%
  summarise(N = n(), `Mean Age` = round(mean(agea, na.rm = TRUE)),
            `Income, missing` = round(100 * (sum(is.na(aquinc_med)) / length(aquinc_med))),
            `Female persons` = round(100 * (table(gndr)[2] / sum(table(gndr)))),
            `Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))),
            `Function Limitations, male` = round(100 * prop.table(table(hlthhmp_d, gndr))[2, 1]),
            `Function Limitations, female` = round(100 * prop.table(table(hlthhmp_d, gndr))[2, 2]),
            `Response rate` = round(unique(resrate)),
            `rrmin` = round(unique(rrmin)),
            `rrmax` = round(unique(rrmax)),
            incmean = mean(aquinc_euro, na.rm = TRUE))


# ------------------------------------------------------------------
# Datensatz nach Ländern gruppieren und Wellen gruppieren und dann
# den Range von Alter, Einkommensmissings, Anteil weibl. Geschlecht,
# Anteil funct. limit. über ale Wellen berechnen
# ------------------------------------------------------------------
dummy2 <- ess %>%
  group_by(country, wave) %>%
  summarise(N = n(), FL = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))),
            FLM = round(100 * prop.table(table(hlthhmp_d, gndr))[2, 1]),
            FLF = round(100 * prop.table(table(hlthhmp_d, gndr))[2, 2]),
            IMa = round(100 * (sum(is.na(aquinc_med)) / length(aquinc_med))),
            MA = round(mean(agea, na.rm = TRUE)),
            FP = round(100 * (table(gndr)[2] / sum(table(gndr)))),
            `incmin_1` = mean(aquinc_euro, na.rm = TRUE),
            `incmax_1` = mean(aquinc_euro, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(IM = ifelse(IMa == 100, NA, IMa)) %>%
  group_by(country) %>%
  summarise(min.n = min(N), max.n = max(N), min.fl = min(FL), max.fl = max(FL),
            min.flm = min(FLM), max.flm = max(FLM), min.flf = min(FLF), max.flf = max(FLF),
            min.incna = min(IM, na.rm = TRUE), max.incna = max(IM, na.rm = TRUE),
            min.age = min(MA), max.age = max(MA), min.fp = min(FP), max.fp = max(FP),
            incmin = min(incmin_1, na.rm = TRUE), incmax = max(incmax_1, na.rm = TRUE))

# ------------------------------------------------------------------
# total zeile
# ------------------------------------------------------------------
dummy3 <- ess %>%
  ungroup() %>%
  summarise(N = n(), `Mean Age` = round(mean(agea, na.rm = TRUE)),
            `Income, missing` = round(100 * (sum(is.na(aquinc_med)) / length(aquinc_med))),
            `Female persons` = round(100 * (table(gndr)[2] / sum(table(gndr)))),
            `Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))),
            `Function Limitations, male` = round(100 * prop.table(table(hlthhmp_d, gndr))[2, 1]),
            `Function Limitations, female` = round(100 * prop.table(table(hlthhmp_d, gndr))[2, 2]),
            `Response rate` = round(mean(unique(resrate))),
            incmean = round(mean(aquinc_euro, na.rm = TRUE)))

# ------------------------------------------------------------------
# in welchen Ländern fehlt in welcher Welle das Einkommen komplett?
# ------------------------------------------------------------------
for (i in 1:7) {
  for (j in unique(ess$country)) {
    d1 <- subset(ess, subset = (country == j & essround == i))
    if (all(is.na(d1$aquinc_med)))
      cat(sprintf("%s (wave %i)\n", j, i))
  }
}

# ------------------------------------------------------------------
# Für Länder, in denen in einer Welle Einkommen vollständig fehlt,
# diese für Mittelwert rausnehmen
# ------------------------------------------------------------------
dummy1$`Income, missing`[dummy1$country == "France"] <- (ess %>%
                                                           filter(country == "France", wave != 1) %>%
                                                           summarise(`Income, missing` = round(100 * (sum(is.na(aquinc_med)) / length(aquinc_med)))))[[1]]

dummy1$`Income, missing`[dummy1$country == "Ireland"] <- (ess %>%
                                                            filter(country == "Ireland", wave != 1) %>%
                                                            summarise(`Income, missing` = round(100 * (sum(is.na(aquinc_med)) / length(aquinc_med)))))[[1]]

dummy1$`Income, missing`[dummy1$country == "Portugal"] <- (ess %>%
                                                             filter(country == "Portugal", wave != 5) %>%
                                                             summarise(`Income, missing` = round(100 * (sum(is.na(aquinc_med)) / length(aquinc_med)))))[[1]]

dummy1$`Income, missing`[dummy1$country == "Hungary"] <- (ess %>%
                                                            filter(country == "Hungary", wave %in% c(2, 4, 5, 6, 7)) %>%
                                                            summarise(`Income, missing` = round(100 * (sum(is.na(aquinc_med)) / length(aquinc_med)))))[[1]]


gesamt <- bind_cols(dummy1, dummy2[, -1]) # , inc_euro[, 3])
gesamt$incmean <- round(gesamt$incmean)
gesamt$incmin <- round(gesamt$incmin)
gesamt$incmax <- round(gesamt$incmax)
gesamt$country <- as.character(gesamt$country)
gesamt$`N (min/max)` <- sprintf("%i-%i", dummy2$min.n, dummy2$max.n)
gesamt$`Function Limitations (in %)` <- sprintf("%i (%i-%i)", dummy1$`Function Limitations`, dummy2$min.fl, dummy2$max.fl)
gesamt$`Income, missing (in %)` <- sprintf("%i (%i-%i)", dummy1$`Income, missing`, dummy2$min.incna, dummy2$max.incna)
gesamt$`Mean Age` <- sprintf("%i (%i-%i)", gesamt$`Mean Age`, dummy2$min.age, dummy2$max.age)
gesamt$`Female persons` <- sprintf("%i (%i-%i)", gesamt$`Female persons`, dummy2$min.fp, dummy2$max.fp)
gesamt$`Response Rate` <- sprintf("%s (%s-%s)", gesamt$`Response rate`, dummy1$rrmin, dummy1$rrmax)
gesamt$`Mean Income` <- sprintf("%s (%s-%s)", gesamt$incmean, gesamt$incmin, gesamt$incmax)
gesamt$`Function Limitations (male, in %)` <- sprintf("%i (%i-%i)", dummy1$`Function Limitations, male`, dummy2$min.flm, dummy2$max.flm)
gesamt$`Function Limitations (female, in %)` <- sprintf("%i (%i-%i)", dummy1$`Function Limitations, female`, dummy2$min.flf, dummy2$max.flf)

# ------------------------------------------------------------------
# Länder markieren, in denen bei einer Welle komplett das Einkommen fehlt
# ------------------------------------------------------------------
gesamt$`Income, missing (in %)`[gesamt$country == "Ireland"] <-
  paste0(gesamt$`Income, missing (in %)`[gesamt$country == "Ireland"], "*")
gesamt$`Income, missing (in %)`[gesamt$country == "France"] <-
  paste0(gesamt$`Income, missing (in %)`[gesamt$country == "France"], "*")
gesamt$`Income, missing (in %)`[gesamt$country == "Hungary"] <-
  paste0(gesamt$`Income, missing (in %)`[gesamt$country == "Hungary"], "**")
gesamt$`Income, missing (in %)`[gesamt$country == "Portugal"] <-
  paste0(gesamt$`Income, missing (in %)`[gesamt$country == "Portugal"], "*")


gesamt_neu <- gesamt[, c(1, 2, 29, 32, 5, 3, 30, 34, 35, 33, 31)]
colnames(gesamt_neu)[4] <- "Response rate (in %)"
colnames(gesamt_neu)[5] <- "Female persons (in %)"
colnames(gesamt_neu)[10] <- "Mean income (in EUROS)"

dummy3 <- as.data.frame(lapply(dummy3, as.character))
gesamt_total <- data.frame(cbind(country = "All", dummy3[, 1], "-", dummy3[, c(8, 4, 2, 5, 6, 7, 9, 3)]))

colnames(gesamt_total) <- colnames(gesamt_neu)
gesamt_total$N <- as.numeric(as.character(gesamt_total$N))

land_reihe <- c(2, 9, 7, 13, 3, 16, 4, 10, 12, 11, 15, 8, 14, 5, 6, 1)
deskr <- bind_rows(gesamt_neu, gesamt_total)[c(land_reihe, 17), ]

sjPlot::tab_df(deskr)
