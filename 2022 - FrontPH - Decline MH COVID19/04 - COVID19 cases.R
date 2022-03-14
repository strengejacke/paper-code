library(dplyr)

data(coronavirus, package = "coronavirus")

coronavirus$country[coronavirus$country == "Czechia"] <- "Czech Republic"
coronavirus <- coronavirus[coronavirus$country %in% as.character(unique(share8$country)), ]

countries <- c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy", "France",
               "Denmark", "Greece", "Switzerland", "Belgium", "Israel", "Czech Republic",
               "Poland", "Luxembourg", "Hungary", "Slovenia", "Estonia",
               "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland", "Latvia",
               "Malta", "Romania", "Slovakia")

# SHARE 8 Covid field work by country, as numeric to match oxford data
field_work <- 20200000 + c(720, 619, 617, 619, 611, 609, 616, 610, 612, 609, 608,
                           604, 608, 608, 625, 618, 608, 608, 615, 613,
                           702, 611, 612, 624, 611, 609, 612)

# SHARE 8 Covid field work by country, as numeric to match oxford data
# taking middle of fieldwork
# field_work <- 20200000 + c(930, 803, 814, 731, 810, 731, 731, 807, 807, 806, 810,
#                            805, 806, 805, 805, 811, 812, 727, 809, 731,
#                            814, 810, 810, 811, 810, 811, 730)

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
              "Latvia", "Malta", "Romania", "Slovakia"),
  population_size = c(8.9, 83.2, 10.4, 17.4, 47.1, 60.25, 65.1, 5.8, 10.71, 8.67,
                      11.5, 9.2, 10.69, 37.96, 0.626, 9.77, 2.1, 1.33, 4.05, 2.8,
                      6.91, 0.886, 5.53, 1.9, 0.515, 19.4, 5.46),
  stringsAsFactors = FALSE
)

total_cases <- merge(total_cases, population_size, by = "country")
total_cases$covid_percent <- total_cases$total_covid_cases / (total_cases$population_size * 10000)

total_cases$population_size <- sjlabelled::set_label(total_cases$population_size, label = "Population size (in millions)")
total_cases$covid_percent <- sjlabelled::set_label(total_cases$covid_percent, label = "Percentage of population infected with COVID")

share8 <- left_join(share8, total_cases, by = "country")
