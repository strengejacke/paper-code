library(ggplot2)
library(dplyr)
library(see)
library(ggrepel)
library(tidyr)



# Download Corona Data from Johns Hopkins

raw_conf <- read.csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
  stringsAsFactors = FALSE
)

# Fixing typo
raw_conf$X2.6.20[which(raw_conf$Country.Region == "Japan")] <- 25

df_confirmed <- raw_conf %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("X"),
    names_to = "date_temp",
    values_to = "cases"
  )

# Parsing the date
df_confirmed$month <- sub("X", "", strsplit(df_confirmed$date_temp, split = "\\.") %>% purrr::map_chr(~.x[1]) )
df_confirmed$day <- strsplit(df_confirmed$date_temp, split = "\\.") %>% purrr::map_chr(~.x[2])
df_confirmed$date <- as.Date(paste("2020", df_confirmed$month, df_confirmed$day, sep = "-"))

df_confirmed <- df_confirmed %>%
  dplyr::rename(country = Country.Region, province = Province.State) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(type = "confirmed")



# Pulling death cases ----------------------------------------------------

raw_death <- read.csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
  stringsAsFactors = FALSE
)


df_death <- raw_death %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("X"),
    names_to = "date_temp",
    values_to = "cases"
  )

# Parsing the date
df_death$month <- sub("X", "", strsplit(df_death$date_temp, split = "\\.") %>% purrr::map_chr(~.x[1]) )
df_death$day <- strsplit(df_death$date_temp, split = "\\.") %>% purrr::map_chr(~.x[2])
df_death$date <- as.Date(paste("2020", df_death$month, df_death$day, sep = "-"))

df_death <- df_death %>%
  dplyr::rename(country = Country.Region, province = Province.State) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(type = "death")



# Pulling recovered cases ----------------------------------------------------

raw_recovered <- read.csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
  stringsAsFactors = FALSE
)

df_recovered <- raw_recovered %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("X"),
    names_to = "date_temp",
    values_to = "cases"
  )

# Parsing the date
df_recovered$month <- sub("X", "", strsplit(df_recovered$date_temp, split = "\\.") %>% purrr::map_chr(~.x[1]) )
df_recovered$day <- strsplit(df_recovered$date_temp, split = "\\.") %>% purrr::map_chr(~.x[2])
df_recovered$date <- as.Date(paste("2020", df_recovered$month, df_recovered$day, sep = "-"))

df_recovered <- df_recovered %>%
  dplyr::rename(country = Country.Region, province = Province.State) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(type = "recovered")



# create data set ---------------------

coronavirus <- dplyr::bind_rows(df_confirmed, df_death, df_recovered) %>%
  dplyr::arrange(country, date) %>%
  dplyr::ungroup()



# prepare data for plot ------------------

dat <- dplyr::filter(coronavirus, country == "Germany", type == "confirmed", cases > 100, date <= "2020-04-01")
# dat2 <- npi %>% dplyr::filter(category != "Governance and socio-economic measures", date_implemented <= "2020-03-29")
dat2 <- npi %>% dplyr::filter(date_implemented <= "2020-03-29")
dat2 <- dat2[-c(1, 10, 16, 17, 23:26, 29:30), ]
dat2$date <- as.Date(dat2$date_implemented)

dat2$category[dat2$category == "Governance and socio-economic measures"] <- "Socio-economic measures"

dat <- merge(dat, dat2, by = "date", all = TRUE, sort = FALSE)



# create figure ------------------------

ggplot() +
  geom_line(data = dat, mapping = aes(x = date, y = cases), size = .8, colour = "#607D8B") +
  geom_ribbon(
    data = dplyr::filter(dat, date >= "2020-03-16" & date <= "2020-03-29"),
    mapping = aes(x = date, ymin = 0, ymax = cases),
    alpha = .15,
    fill = "#03A9F4"
  ) +
  geom_linerange(
    data = dplyr::filter(dat, date == "2020-03-16" | date == "2020-03-29"),
    mapping = aes(x = date, y = 0, ymin = 0, ymax = cases),
    colour = "#03A9F4",
    linetype = "dashed"
  ) +
  geom_linerange(
    data = dat[!is.na(dat$category), ],
    mapping = aes(x = date, ymin = 0, ymax = cases, colour = category),
    size = .9,
    position = position_dodge(1.2)
  ) +
  geom_point(
    data = dat[!is.na(dat$category), ],
    mapping = aes(x = date, y = cases, colour = category),
    size = 2.5,
    position = position_dodge(1.2)
  ) +
  ggrepel::geom_label_repel(
    data = dat[!is.na(dat$category) & !duplicated(dat$date), ],
    mapping = aes(x = date, y = cases, label = as.character(date)),
    size = 3,
    nudge_y = .4
  ) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d.%m.", expand = c(0, 0)) +
  scale_y_log10(breaks = c(100, 1000, 10000, 100000), limits = c(100, 200000), expand = c(0, 0)) +
  scale_color_brewer(palette = "Set1") +
  see::theme_lucid() +
  labs(x = NULL, y = "Number of confirmed cases", colour = "Measure") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

ggsave(filename = "NPI.tiff", width = 20, height = 14, units = "cm", dpi = 600, compress = "lzw")
