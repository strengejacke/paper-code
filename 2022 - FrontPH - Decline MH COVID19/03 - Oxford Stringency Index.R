library(dplyr)

# Datensatz laden
oxford <- readr::read_csv("Daten/OxCGRT_latest.csv")

countries <- c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy", "France",
               "Denmark", "Greece", "Switzerland", "Belgium", "Israel", "Czech Republic",
               "Poland", "Luxembourg", "Hungary", "Slovenia", "Estonia",
               "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland", "Latvia",
               "Malta", "Romania", "Slovakia")

# SHARE 8 Covid field work by country, as numeric to match oxford data
# taking start of fieldwork
field_work <- 20200000 + c(720, 619, 617, 619, 611, 609, 616, 610, 612, 609, 608,
                           604, 608, 608, 625, 618, 608, 608, 615, 613,
                           702, 611, 612, 624, 611, 609, 612)

# SHARE 8 Covid field work by country, as numeric to match oxford data
# taking middle of fieldwork
# field_work <- 20200000 + c(930, 803, 814, 731, 810, 731, 731, 807, 807, 806, 810,
#                            805, 806, 805, 805, 811, 812, 727, 809, 731,
#                            814, 810, 810, 811, 810, 811, 730)

# match fieldwork dates and country names
d <- data.frame(Fieldwork = field_work, CountryName = countries, stringsAsFactors = FALSE)

# fix country name for Slovakia, to match Oxford and Share
oxford$CountryName[oxford$CountryName == "Slovak Republic"] <- "Slovakia"
oxford <- oxford |>
  dplyr::filter(CountryName %in% countries) |>
  dplyr::select(-ends_with("ForDisplay"))

oxford <- merge(oxford, d, sort = FALSE)

# only use those entries until fieldwork in Share startet,
# i.e. remove newer data
oxford_reduced <- oxford |>
  group_by(CountryName) |>
  filter(Date <= Fieldwork) |>
  ungroup()

# create daily index values

oxford_reduced$C1_Index <- 100 * (oxford_reduced$`C1_School closing` - .5 * (1 - oxford_reduced$C1_Flag)) / 3
oxford_reduced$C2_Index <- 100 * (oxford_reduced$`C2_Workplace closing` - .5 * (1 - oxford_reduced$C2_Flag)) / 3
oxford_reduced$C3_Index <- 100 * (oxford_reduced$`C3_Cancel public events` - .5 * (1 - oxford_reduced$C3_Flag)) / 2
oxford_reduced$C4_Index <- 100 * (oxford_reduced$`C4_Restrictions on gatherings` - .5 * (1 - oxford_reduced$C4_Flag)) / 4
oxford_reduced$C5_Index <- 100 * (oxford_reduced$`C5_Close public transport` - .5 * (1 - oxford_reduced$C5_Flag)) / 2
oxford_reduced$C6_Index <- 100 * (oxford_reduced$`C6_Stay at home requirements` - .5 * (1 - oxford_reduced$C6_Flag)) / 3
oxford_reduced$C7_Index <- 100 * (oxford_reduced$`C7_Restrictions on internal movement` - .5 * (1 - oxford_reduced$C7_Flag)) / 2
oxford_reduced$C8_Index <- 100 * (oxford_reduced$`C8_International travel controls`) / 4
oxford_reduced$H1_Index <- 100 * (oxford_reduced$`H1_Public information campaigns` - .5 * (1 - oxford_reduced$H1_Flag)) / 2

oxford_reduced$C1_Index[is.na(oxford_reduced$C1_Flag)] <- 0
oxford_reduced$C2_Index[is.na(oxford_reduced$C2_Flag)] <- 0
oxford_reduced$C3_Index[is.na(oxford_reduced$C3_Flag)] <- 0
oxford_reduced$C4_Index[is.na(oxford_reduced$C4_Flag)] <- 0
oxford_reduced$C5_Index[is.na(oxford_reduced$C5_Flag)] <- 0
oxford_reduced$C6_Index[is.na(oxford_reduced$C6_Flag)] <- 0
oxford_reduced$C7_Index[is.na(oxford_reduced$C7_Flag)] <- 0
oxford_reduced$H1_Index[is.na(oxford_reduced$H1_Flag)] <- 0

oxford_reduced <- sjmisc::row_means(oxford_reduced, C1_Index:H1_Index, n = 1, var = "daily_index")

oxford_reduced <- oxford_reduced |>
  group_by(CountryName) |>
  mutate(avg_str_index = mean(daily_index, na.rm = TRUE),
         mean_str_index = mean(StringencyIndex, na.rm = TRUE),
         mean_gov_resp_index = mean(GovernmentResponseIndex, na.rm = TRUE),
         mean_health_index = mean(ContainmentHealthIndex, na.rm = TRUE)) |>
  ungroup()

save(oxford, oxford_reduced, file = "oxford-stringency-index.RData")

oxford_for_merge <- oxford_reduced |>
  group_by(CountryName) |>
  filter(Date <= Fieldwork) |>
  filter(Date == max(Date, na.rm = TRUE)) |>
  ungroup() |>
  select(-c(2:39, 48:58), -ends_with("ForDisplay"), -ends_with("Index", FALSE)) |>
  sjmisc::rename_columns(CountryName = "country")

save(oxford_for_merge, file = "oxford_for_merging.RData")

share8$land <- as.character(share8$country)
oxford_for_merge$land <- oxford_for_merge$country
oxford_for_merge$country <- NULL
share8 <- dplyr::full_join(share8, oxford_for_merge, by = "land")


# COVID Regime 3 Gruppen

qunt <- quantile(share8$mean_str_index, c(.33, .67))
share8$covid_regime_si3 <- NA
share8$covid_regime_si3[share8$mean_str_index <= qunt[1]] <- "low"
share8$covid_regime_si3[share8$mean_str_index > qunt[1] & share8$mean_str_index <= qunt[2]] <- "middle"
share8$covid_regime_si3[share8$mean_str_index > qunt[2]] <- "high"

share8$covid_regime_si3 <- factor(
  share8$covid_regime_si3,
  levels = c("low", "middle", "high")
)
share8$covid_regime_si3 <- set_label(share8$covid_regime_si3, label = "Stringency Index 3 Groups")


qunt <- quantile(share8$mean_gov_resp_index, c(.33, .67))
share8$covid_regime_gr3 <- NA
share8$covid_regime_gr3[share8$mean_gov_resp_index <= qunt[1]] <- "low"
share8$covid_regime_gr3[share8$mean_gov_resp_index > qunt[1] & share8$mean_gov_resp_index <= qunt[2]] <- "middle"
share8$covid_regime_gr3[share8$mean_gov_resp_index > qunt[2]] <- "high"

share8$covid_regime_gr3 <- factor(
  share8$covid_regime_gr3,
  levels = c("low", "middle", "high")
)
share8$covid_regime_gr3 <- set_label(share8$covid_regime_gr3, label = "Government Response Index 3 Groups")


qunt <- quantile(share8$mean_health_index, c(.33, .67))
share8$covid_regime_ch3 <- NA
share8$covid_regime_ch3[share8$mean_health_index <= qunt[1]] <- "low"
share8$covid_regime_ch3[share8$mean_health_index > qunt[1] & share8$mean_health_index <= qunt[2]] <- "middle"
share8$covid_regime_ch3[share8$mean_health_index > qunt[2]] <- "high"

share8$covid_regime_ch3 <- factor(
  share8$covid_regime_ch3,
  levels = c("low", "middle", "high")
)
share8$covid_regime_ch3 <- set_label(share8$covid_regime_ch3, label = "Containment and Health Index 3 Groups")


# COVID Regime 3 Gruppen, nach cutpoints

qunt <- c(40, 45)
share8$index_str <- NA
share8$index_str[share8$mean_str_index <= qunt[1]] <- "low"
share8$index_str[share8$mean_str_index > qunt[1] & share8$mean_str_index <= qunt[2]] <- "middle"
share8$index_str[share8$mean_str_index > qunt[2]] <- "high"

share8$index_str <- factor(
  share8$index_str,
  levels = c("low", "middle", "high")
)
share8$index_str <- set_label(share8$index_str, label = "Stringency Index 3 Groups (split at 40/45)")


qunt <- c(35, 40)
share8$index_govrep <- NA
share8$index_govrep[share8$mean_gov_resp_index <= qunt[1]] <- "low"
share8$index_govrep[share8$mean_gov_resp_index > qunt[1] & share8$mean_gov_resp_index <= qunt[2]] <- "middle"
share8$index_govrep[share8$mean_gov_resp_index > qunt[2]] <- "high"

share8$index_govrep <- factor(
  share8$index_govrep,
  levels = c("low", "middle", "high")
)
share8$index_govrep <- set_label(share8$index_govrep, label = "Government Response Index 3 Groups (split at 35/40)")


qunt <- c(35, 40)
share8$index_conhealth <- NA
share8$index_conhealth[share8$mean_health_index <= qunt[1]] <- "low"
share8$index_conhealth[share8$mean_health_index > qunt[1] & share8$mean_health_index <= qunt[2]] <- "middle"
share8$index_conhealth[share8$mean_health_index > qunt[2]] <- "high"

share8$index_conhealth <- factor(
  share8$index_conhealth,
  levels = c("low", "middle", "high")
)
share8$index_conhealth <- set_label(share8$index_conhealth, label = "Containment and Health Index 3 Groups  (split at 35/40)")


## COVID Regime nach Cutpoints, 4 Gruppen

qunt <- c(40, 45, 50)
share8$covid_regime_si4 <- NA
share8$covid_regime_si4[share8$mean_str_index <= qunt[1]] <- "low"
share8$covid_regime_si4[share8$mean_str_index > qunt[1] & share8$mean_str_index <= qunt[2]] <- "middle"
share8$covid_regime_si4[share8$mean_str_index > qunt[2] & share8$mean_str_index <= qunt[3]] <- "high"
share8$covid_regime_si4[share8$mean_str_index > qunt[3]] <- "very high"

share8$covid_regime_si4 <- factor(
  share8$covid_regime_si4,
  levels = c("low", "middle", "high", "very high")
)
share8$covid_regime_si4 <- set_label(share8$covid_regime_si4, label = "Stringency Index (4 Groups)")



## COVID Regime nach Cutpoints, 5 Gruppen

qunt <- c(37.5, 40, 45, 47.5)
share8$covid_regime_si5 <- NA
share8$covid_regime_si5[share8$mean_str_index <= qunt[1]] <- "very low"
share8$covid_regime_si5[share8$mean_str_index > qunt[1] & share8$mean_str_index <= qunt[2]] <- "low"
share8$covid_regime_si5[share8$mean_str_index > qunt[2] & share8$mean_str_index <= qunt[3]] <- "middle"
share8$covid_regime_si5[share8$mean_str_index > qunt[3] & share8$mean_str_index <= qunt[4]] <- "high"
share8$covid_regime_si5[share8$mean_str_index > qunt[4]] <- "very high"

share8$covid_regime_si5 <- factor(
  share8$covid_regime_si5,
  levels = c("very low", "low", "middle", "high", "very high")
)
share8$covid_regime_si5 <- set_label(share8$covid_regime_si5, label = "Stringency Index (5 Groups)")
