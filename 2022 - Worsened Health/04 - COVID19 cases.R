library(dplyr)

data(coronavirus, package = "coronavirus")

coronavirus$country[coronavirus$country == "Czechia"] <- "Czech Republic"
coronavirus <- coronavirus[coronavirus$country %in% as.character(unique(share$land)), ]

countries <- c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy", "France",
               "Denmark", "Greece", "Switzerland", "Belgium", "Israel", "Czech Republic",
               "Poland", "Luxembourg", "Hungary", "Slovenia", "Estonia",
               "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland", "Latvia",
               "Malta", "Romania", "Slovakia", "Portugal")

# SHARE 8 Covid field work by country, as numeric to match oxford data
field_work <- 20200000 + c(720, 619, 617, 619, 611, 609, 616, 610, 612, 609, 608,
                           604, 608, 608, 625, 618, 608, 608, 615, 613,
                           702, 611, 612, 624, 611, 609, 612, 611)

# match fieldwork dates and country names
d <- data.frame(Fieldwork = field_work, country = countries, stringsAsFactors = FALSE)

coronavirus$date_num <- as.numeric(gsub("-", "", coronavirus$date, fixed = TRUE))
coronavirus <- merge(coronavirus, d, sort = FALSE)

coronavirus <- coronavirus |>
  group_by(country) |>
  filter(date_num <= Fieldwork, type == "confirmed") |>
  ungroup()

total_cases <- coronavirus |>
  group_by(country) |>
  summarise(total_covid_cases = sum(cases))

population_size <- data.frame(
  country = c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy",
              "France", "Denmark", "Greece", "Switzerland", "Belgium", "Israel",
              "Czech Republic", "Poland", "Luxembourg", "Hungary", "Slovenia",
              "Estonia", "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland",
              "Latvia", "Malta", "Romania", "Slovakia", "Portugal"),
  population_size = c(8.9, 83.2, 10.4, 17.4, 47.1, 60.25, 65.1, 5.8, 10.71, 8.67,
                      11.5, 9.2, 10.69, 37.96, 0.626, 9.77, 2.1, 1.33, 4.05, 2.8,
                      6.91, 0.886, 5.53, 1.9, 0.515, 19.4, 5.46, 10.3),
  stringsAsFactors = FALSE
)

total_cases <- merge(total_cases, population_size, by = "country")
total_cases$covid_percent <- total_cases$total_covid_cases / (total_cases$population_size * 10000)

total_cases$population_size <- sjlabelled::set_label(total_cases$population_size, label = "Population size (in millions)")
total_cases$covid_percent <- sjlabelled::set_label(total_cases$covid_percent, label = "Percentage of population infected with COVID")
total_cases$wave <- 8

# final data for wave 8
total_cases_wave8 <- total_cases





data(coronavirus, package = "coronavirus")

coronavirus$country[coronavirus$country == "Czechia"] <- "Czech Republic"
coronavirus <- coronavirus[coronavirus$country %in% as.character(unique(share$land)), ]

countries <- c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy", "France",
               "Denmark", "Greece", "Switzerland", "Belgium", "Israel", "Czech Republic",
               "Poland", "Luxembourg", "Hungary", "Slovenia", "Estonia",
               "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland", "Latvia",
               "Malta", "Romania", "Slovakia", "Portugal")

# SHARE 8 Covid field work by country, as numeric to match oxford data
field_work <- 20210000 + c(622, 629, 628, 610, 609, 609, 608, 620, 621, 610, 608,
                           602, 603, 609, 614, 616, 604, 610, 617, 608,
                           609, 608, 616, 611, 621, 613, 610, 605)

# match fieldwork dates and country names
d <- data.frame(Fieldwork = field_work, country = countries, stringsAsFactors = FALSE)

coronavirus$date_num <- as.numeric(gsub("-", "", coronavirus$date, fixed = TRUE))
coronavirus <- merge(coronavirus, d, sort = FALSE)

coronavirus <- coronavirus |>
  group_by(country) |>
  filter(date_num <= Fieldwork, type == "confirmed") |>
  ungroup()

total_cases <- coronavirus |>
  group_by(country) |>
  summarise(total_covid_cases = sum(cases))

population_size <- data.frame(
  country = c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy",
              "France", "Denmark", "Greece", "Switzerland", "Belgium", "Israel",
              "Czech Republic", "Poland", "Luxembourg", "Hungary", "Slovenia",
              "Estonia", "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland",
              "Latvia", "Malta", "Romania", "Slovakia", "Portugal"),
  population_size = c(8.9, 83.2, 10.4, 17.4, 47.1, 60.25, 65.1, 5.8, 10.71, 8.67,
                      11.5, 9.2, 10.69, 37.96, 0.626, 9.77, 2.1, 1.33, 4.05, 2.8,
                      6.91, 0.886, 5.53, 1.9, 0.515, 19.4, 5.46, 10.3),
  stringsAsFactors = FALSE
)

total_cases <- merge(total_cases, population_size, by = "country")
total_cases$covid_percent <- total_cases$total_covid_cases / (total_cases$population_size * 10000)

total_cases$population_size <- sjlabelled::set_label(total_cases$population_size, label = "Population size (in millions)")
total_cases$covid_percent <- sjlabelled::set_label(total_cases$covid_percent, label = "Percentage of population infected with COVID")
total_cases$wave <- 9

# final data for wave 9
total_cases_wave9 <- total_cases


total_cases <- bind_rows(total_cases_wave8, total_cases_wave9)
colnames(total_cases)[1] <- "land"


share <- left_join(share, total_cases, by = c("land", "wave"))

# main file
save(share, file = "Daten/share_8_9.RData")

# intermediate step file
save(share, file = "Daten/share_8_9-step4.RData")
