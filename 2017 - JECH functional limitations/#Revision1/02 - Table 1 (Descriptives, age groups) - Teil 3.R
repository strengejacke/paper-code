library(strengejacke)
library(tidyverse)


# ------------------------------------------------------------------
# Datensatz nach Ländern gruppieren (also je ein Land über alle Wellen
# aggregiert) und dann Alter, Einkommensmissings, Anteil weibl. Geschlecht,
# Anteil funct. limit. über ale Wellen berechnen
# ------------------------------------------------------------------
# 0 für bis 74, 1 für 75+ Jahre
alter <- 1


dummy1 <- ess %>%
  filter(age_dicho2 == alter) %>%
  group_by(country) %>%
  summarise(N = n(),
            `Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))))


# ------------------------------------------------------------------
# Datensatz nach Ländern gruppieren und Wellen gruppieren und dann
# den Range von Alter, Einkommensmissings, Anteil weibl. Geschlecht,
# Anteil funct. limit. über ale Wellen berechnen
# ------------------------------------------------------------------
dummy2 <- ess %>%
  filter(age_dicho2 == alter) %>%
  group_by(country, wave) %>%
  summarise(N = n(), FL = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d))))) %>%
  summarise(min.n = min(N), max.n = max(N), min.fl = min(FL), max.fl = max(FL))

# ------------------------------------------------------------------
# total zeile
# ------------------------------------------------------------------
dummy3 <- ess %>%
  filter(age_dicho2 == alter) %>%
  ungroup() %>%
  summarise(N = n(), `Function Limitations` = round(100 * (table(hlthhmp_d)[2] / sum(table(hlthhmp_d)))))

gesamt <- bind_cols(dummy1, dummy2[, -1])
gesamt$country <- as.character(gesamt$country)
gesamt$`N (min/max)` <- sprintf("%i-%i", dummy2$min.n, dummy2$max.n)
gesamt$`Function Limitations (in %)` <- sprintf("%i (%i-%i)", dummy1$`Function Limitations`, dummy2$min.fl, dummy2$max.fl)
gesamt <- gesamt[, c(1, 8, 9)]

gesamt_total <- data.frame("All", "19222", "53")

colnames(gesamt_total) <- colnames(gesamt)
land_reihe <- c(2, 9, 7, 13, 3, 16, 4, 10, 12, 11, 15, 8, 14, 5, 6, 1)
gesamt_neu <- bind_rows(gesamt, gesamt_total)[c(land_reihe, 17), ]

sjPlot::tab_df(gesamt_neu)
