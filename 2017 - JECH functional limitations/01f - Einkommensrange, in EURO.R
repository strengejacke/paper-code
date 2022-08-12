library(readxl)
library(dplyr)

eink <- read_excel("../Sonstige Daten/Einkommensgruppen in EURO.xlsx")

# Eine leere Zeile entfernen
eink <- eink[-161, ]

# Schweden und Polen wurden noch nicht umgerechnet
eink$Final[eink$Land == "Sweden"] <- eink$Final[eink$Land == "Sweden"] * .11
eink$Final[eink$Land == "Poland"] <- eink$Final[eink$Land == "Poland"] / 4.16


# range median income nach ländern
eink_range <- ess %>%
  group_by(country, wave) %>%
  summarise(income = round(median(aquinc, na.rm = TRUE))) %>%
  group_by(country) %>%
  summarise(min_income = min(income, na.rm = TRUE), max_income = max(income, na.rm = TRUE))

euros <- c()
for (i in seq_len(nrow(eink_range))) {
  dummy <- eink %>%
    filter(Land == eink_range$country[i],
           Einkommensgruppe == eink_range$min_income[i]) %>%
    mutate(EUR = Final) %>%
    select(EUR)
  euros <- c(euros, dummy[[1]])
}
eink_range <- cbind(eink_range, min_euros = euros)

euros <- c()
for (i in seq_len(nrow(eink_range))) {
  dummy <- eink %>%
    filter(Land == eink_range$country[i],
           Einkommensgruppe == eink_range$max_income[i]) %>%
    mutate(EUR = Final) %>%
    select(EUR)
  euros <- c(euros, dummy[[1]])
}
eink_range <- cbind(eink_range, max_euros = euros)
eink_range

# total range median income
ess %>%
  group_by(wave) %>%
  summarise(income = round(median(aquinc, na.rm = TRUE))) %>%
  summarise(min_income = min(income, na.rm = TRUE), max_income = max(income, na.rm = TRUE))

ess$id <- seq_len(nrow(ess))

get_equi <- function(...) {
  equi <- rep(NA, time = nrow(ess))
  for (i in unique(eink$Land)) {
    tmp <- filter(ess, country == i)
    eink_tmp <- filter(eink, Land == i)
    for (j in unique(tmp$aquinc)) {
      if (!is.na(j)) {
        index <- tmp$id[tmp$aquinc == j]
        if (j > 10) {
          equi[index] <- j * eink_tmp$Final[eink_tmp$Einkommensgruppe == 10] / 10
        } else if (j < 1) {
          equi[index] <- j * eink_tmp$Final[eink_tmp$Einkommensgruppe == 1]
        } else if (j %% 1 == 0) {
          equi[index] <- eink_tmp$Final[eink_tmp$Einkommensgruppe == j]
        } else {
          ganz_j <- round(j)
          halb_j <- j %% 1
          round_inc <- eink_tmp$Final[eink_tmp$Einkommensgruppe == ganz_j]
          addit <- eink_tmp$Final[eink_tmp$Einkommensgruppe == ganz_j + 1] - round_inc
          equi[index] <- round_inc + halb_j * addit
        }
      }
    }
  }
  equi
}

ess$aquinc_euro <- get_equi()
set_label(ess$aquinc_euro) <- "Household Equivalent Netincome, EUROS"
