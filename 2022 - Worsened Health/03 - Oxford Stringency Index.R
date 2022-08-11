library(easystats)
library(strengejacke)
library(dplyr)

# Datensatz laden
oxford <- data_read("Daten/OxCGRT_latest.zip")

# fix country name for Slovakia, to match Oxford and Share
oxford$CountryName[oxford$CountryName == "Slovak Republic"] <- "Slovakia"

# participating countries in SHARE
countries <- c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy", "France",
               "Denmark", "Greece", "Switzerland", "Belgium", "Israel", "Czech Republic",
               "Poland", "Luxembourg", "Hungary", "Slovenia", "Estonia",
               "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland", "Latvia",
               "Malta", "Romania", "Slovakia", "Portugal")

# filter oxford data to remain only SHARE countries
oxford <- oxford |>
  data_filter(CountryName %in% countries & Jurisdiction == "NAT_TOTAL") |>
  data_select(-ends_with("ForDisplay"))

# save raw data
original_oxford <- oxford


# SHARE 8 Covid field work by country, as numeric to match oxford data
# taking start of fieldwork
field_work <- 20200000 + c(720, 619, 617, 619, 611, 609, 616, 610, 612, 609, 608,
                           604, 608, 608, 625, 618, 608, 608, 615, 613,
                           702, 611, 612, 624, 611, 609, 612, 611)

field_work8 <- field_work

# match fieldwork dates and country names
d <- data.frame(Fieldwork = field_work, CountryName = countries, stringsAsFactors = FALSE)
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

oxford_for_merge <- oxford_reduced |>
  group_by(CountryName) |>
  filter(Date <= Fieldwork) |>
  filter(Date == max(Date, na.rm = TRUE)) |>
  ungroup() |>
  select(-c(2:39, 42:47, 56:65), -ends_with("ForDisplay"), -ends_with("Index", FALSE)) |>
  sjmisc::rename_columns(CountryName = "country")

oxford_for_merge$land <- oxford_for_merge$country
oxford_for_merge$country <- NULL
oxford_for_merge$wave <- 8

# SI Index for wave 8
oxford_for_merge_wave8 <- oxford_for_merge



# SHARE wave 9 -------------------------------------------

oxford <- original_oxford

# SHARE 9 Covid field work by country, as numeric to match oxford data
# taking start of fieldwork
field_work <- 20210000 + c(622, 629, 628, 610, 609, 609, 608, 620, 621, 610, 608,
                           602, 603, 609, 614, 616, 604, 610, 617, 608,
                           609, 608, 616, 611, 621, 613, 610, 605)

# match fieldwork dates and country names
d <- data.frame(Fieldwork = field_work, CountryName = countries, Fieldwork8 = field_work8, stringsAsFactors = FALSE)
oxford <- merge(oxford, d, sort = FALSE)

# only use those entries until fieldwork in Share startet,
# i.e. remove newer data
oxford_reduced <- oxford |>
  group_by(CountryName) |>
  filter(Date > Fieldwork8 & Date <= Fieldwork) |>
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

oxford_for_merge <- oxford_reduced |>
  group_by(CountryName) |>
  filter(Date > Fieldwork8 & Date <= Fieldwork) |>
  filter(Date == max(Date, na.rm = TRUE)) |>
  ungroup() |>
  select(-c(2:39, 42:47, 56:65), -ends_with("ForDisplay"), -ends_with("Index", FALSE), -Fieldwork8) |>
  sjmisc::rename_columns(CountryName = "country")

oxford_for_merge$land <- oxford_for_merge$country
oxford_for_merge$country <- NULL
oxford_for_merge$wave <- 9

# SI Index for wave 9
oxford_for_merge_wave9 <- oxford_for_merge




# overall average SI -------------------------------------------

oxford <- original_oxford

# SHARE 9 Covid field work by country, as numeric to match oxford data
# taking start of fieldwork
field_work <- 20210000 + c(622, 629, 628, 610, 609, 609, 608, 620, 621, 610, 608,
                           602, 603, 609, 614, 616, 604, 610, 617, 608,
                           609, 608, 616, 611, 621, 613, 610, 605)

# match fieldwork dates and country names
d <- data.frame(Fieldwork = field_work, CountryName = countries, stringsAsFactors = FALSE)
oxford <- merge(oxford, d, sort = FALSE)

# only use those entries until fieldwork in Share started,
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

oxford_for_merge <- oxford_reduced |>
  group_by(CountryName) |>
  filter(Date <= Fieldwork) |>
  filter(Date == max(Date, na.rm = TRUE)) |>
  ungroup() |>
  select(-c(2:39, 42:47, 56:65), -ends_with("ForDisplay"), -ends_with("Index", FALSE)) |>
  sjmisc::rename_columns(CountryName = "country")


# global SI Index for waves 8 and 9
oxford_for_merge_wave8$global_mean_str_index <- oxford_for_merge$mean_str_index
oxford_for_merge_wave9$global_mean_str_index <- oxford_for_merge$mean_str_index
oxford_for_merge_wave8$global_mean_gov_resp_index <- oxford_for_merge$mean_gov_resp_index
oxford_for_merge_wave9$global_mean_gov_resp_index <- oxford_for_merge$mean_gov_resp_index
oxford_for_merge_wave8$global_mean_health_index <- oxford_for_merge$mean_health_index
oxford_for_merge_wave9$global_mean_health_index <- oxford_for_merge$mean_health_index



# final data ------------------------------
oxford_for_merge <- bind_rows(oxford_for_merge_wave8, oxford_for_merge_wave9)



share$land <- as.character(share$countries)
share <- dplyr::full_join(share, oxford_for_merge, by = c("land", "wave"))


# COVID Regime 3 Gruppen

share$covid_regime_si3 <- NA
# wave 8
qunt <- quantile(share$mean_str_index[share$wave == 8], c(.33, .67))
share$covid_regime_si3[share$wave == 8 & share$mean_str_index <= qunt[1]] <- "low"
share$covid_regime_si3[share$wave == 8 & share$mean_str_index > qunt[1] & share$mean_str_index <= qunt[2]] <- "middle"
share$covid_regime_si3[share$wave == 8 & share$mean_str_index > qunt[2]] <- "high"

# wave 9
qunt <- quantile(share$mean_str_index[share$wave == 9], c(.33, .67))
share$covid_regime_si3[share$wave == 9 & share$mean_str_index <= qunt[1]] <- "low"
share$covid_regime_si3[share$wave == 9 & share$mean_str_index > qunt[1] & share$mean_str_index <= qunt[2]] <- "middle"
share$covid_regime_si3[share$wave == 9 & share$mean_str_index > qunt[2]] <- "high"

share$covid_regime_si3 <- factor(
  share$covid_regime_si3,
  levels = c("low", "middle", "high")
)
share$covid_regime_si3 <- set_label(share$covid_regime_si3, label = "Stringency Index 3 Groups")

share$global_covid_regime_si3 <- NA
# global
qunt <- quantile(share$global_mean_str_index, c(.33, .67))
share$global_covid_regime_si3[share$global_mean_str_index <= qunt[1]] <- "low"
share$global_covid_regime_si3[share$global_mean_str_index > qunt[1] & share$global_mean_str_index <= qunt[2]] <- "middle"
share$global_covid_regime_si3[share$global_mean_str_index > qunt[2]] <- "high"

share$global_covid_regime_si3 <- factor(
  share$global_covid_regime_si3,
  levels = c("low", "middle", "high")
)
share$global_covid_regime_si3 <- set_label(share$global_covid_regime_si3, label = "Global Stringency Index 3 Groups")



share$covid_regime_gr3 <- NA
# wave 8
qunt <- quantile(share$mean_gov_resp_index[share$wave == 8], c(.33, .67))
share$covid_regime_gr3[share$wave == 8 & share$mean_gov_resp_index <= qunt[1]] <- "low"
share$covid_regime_gr3[share$wave == 8 & share$mean_gov_resp_index > qunt[1] & share$mean_gov_resp_index <= qunt[2]] <- "middle"
share$covid_regime_gr3[share$wave == 8 & share$mean_gov_resp_index > qunt[2]] <- "high"

# wave 9
qunt <- quantile(share$mean_gov_resp_index[share$wave == 9], c(.33, .67))
share$covid_regime_gr3[share$wave == 9 & share$mean_gov_resp_index <= qunt[1]] <- "low"
share$covid_regime_gr3[share$wave == 9 & share$mean_gov_resp_index > qunt[1] & share$mean_gov_resp_index <= qunt[2]] <- "middle"
share$covid_regime_gr3[share$wave == 9 & share$mean_gov_resp_index > qunt[2]] <- "high"

share$covid_regime_gr3 <- factor(
  share$covid_regime_gr3,
  levels = c("low", "middle", "high")
)
share$covid_regime_gr3 <- set_label(share$covid_regime_gr3, label = "Government Response Index 3 Groups")

share$global_covid_regime_gr3 <- NA
# global
qunt <- quantile(share$global_mean_gov_resp_index, c(.33, .67))
share$global_covid_regime_gr3[share$global_mean_gov_resp_index <= qunt[1]] <- "low"
share$global_covid_regime_gr3[share$global_mean_gov_resp_index > qunt[1] & share$global_mean_gov_resp_index <= qunt[2]] <- "middle"
share$global_covid_regime_gr3[share$global_mean_gov_resp_index > qunt[2]] <- "high"

share$global_covid_regime_gr3 <- factor(
  share$global_covid_regime_gr3,
  levels = c("low", "middle", "high")
)
share$global_covid_regime_gr3 <- set_label(share$global_covid_regime_gr3, label = "Global Government Response Index 3 Groups")




share$covid_regime_ch3 <- NA
# wave 8
qunt <- quantile(share$mean_health_index[share$wave == 8], c(.33, .67))
share$covid_regime_ch3[share$wave == 8 & share$mean_health_index <= qunt[1]] <- "low"
share$covid_regime_ch3[share$wave == 8 & share$mean_health_index > qunt[1] & share$mean_health_index <= qunt[2]] <- "middle"
share$covid_regime_ch3[share$wave == 8 & share$mean_health_index > qunt[2]] <- "high"

# wave 9
qunt <- quantile(share$mean_health_index[share$wave == 9], c(.33, .67))
share$covid_regime_ch3[share$wave == 9 & share$mean_health_index <= qunt[1]] <- "low"
share$covid_regime_ch3[share$wave == 9 & share$mean_health_index > qunt[1] & share$mean_health_index <= qunt[2]] <- "middle"
share$covid_regime_ch3[share$wave == 9 & share$mean_health_index > qunt[2]] <- "high"

share$covid_regime_ch3 <- factor(
  share$covid_regime_ch3,
  levels = c("low", "middle", "high")
)
share$covid_regime_ch3 <- set_label(share$covid_regime_ch3, label = "Containment and Health Index 3 Groups")

share$global_covid_regime_ch3 <- NA
# global
qunt <- quantile(share$global_mean_health_index, c(.33, .67))
share$global_covid_regime_ch3[share$global_mean_health_index <= qunt[1]] <- "low"
share$global_covid_regime_ch3[share$global_mean_health_index > qunt[1] & share$global_mean_health_index <= qunt[2]] <- "middle"
share$global_covid_regime_ch3[share$global_mean_health_index > qunt[2]] <- "high"

share$global_covid_regime_ch3 <- factor(
  share$global_covid_regime_ch3,
  levels = c("low", "middle", "high")
)
share$global_covid_regime_ch3 <- set_label(share$global_covid_regime_ch3, label = "Global Containment and Health Index 3 Groups")




# main file
save(share, file = "Daten/share_8_9.RData")

# intermediate step file
save(share, file = "Daten/share_8_9-step3.RData")
