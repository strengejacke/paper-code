library(dplyr)
library(sjlabelled)
library(sjmisc)


# load GESIS data ---------------------

# GESIS data upon request from
# https://www.gesis.org/gesis-panel/coronavirus-outbreak/public-use-file-puf

d <- read_spss("Daten/ZA5667_v1-1-0.sav") %>%
  set_na(na = c(-111, -99, -77, -33, -22, -88, 98)) %>%
  drop_labels() %>%
  to_label(sex, education_cat, marstat)



# download NPI data -------------------

# Package "tidycovid19" from
# https://github.com/joachim-gassen/tidycovid19

npi <- tidycovid19::download_acaps_npi_data() %>%
  dplyr::ungroup() %>%
  dplyr::filter(country == "Germany") %>%
  dplyr::select(date_implemented, log_type, category, measure, targeted_pop_group, comments) %>%
  dplyr::arrange(date_implemented)



# recodes -------------------

d$bildung <- d$education_cat
d$alter <- d$age_cat
d$hh_groesse <- d$household

d$avoidplaces <- d$hzcy006a
d$keptdistance <- d$hzcy007a
d$worksituation <- d$hzcy008a
d$quarantinesymptoms <- d$hzcy009a
d$quarantinenosymptoms <- d$hzcy010a
d$handwashing <- d$hzcy011a
d$desinfectant <- d$hzcy012a
d$stocksincreased <- d$hzcy013a
d$reducesocial <- d$hzcy014a
d$facemask <- d$hzcy015a
d$nomeasures <- d$hzcy018a

levels(d$sex) <- c("male", "female")



# create weighting variable ----

# Package iterake from
# https://github.com/ttrodrigz/iterake

library(iterake)

# Official statistic

# Geschlecht (w in %)	50.71 [1]
#
# Altersgruppen [2]
# 18 - ≤ 24 Jahre	7.72
# 25 - ≤ 39 Jahre	19.02
# 40 - ≤ 59 Jahre	29.12
# 60 - ≤ 64 Jahre	6.52
# ≥ 65 Jahre	21.42
#
# Familienstand (%) [3]
# Ledig	30.93
# Verheiratet	42.03
# In Trennung	3.73
# Geschieden 	11.83
# Verwitwet	11.63
#
# Haushaltsgröße [4]
# 1-Personen Haushalt	41.94
# 2-Personen Haushalt	33.84
# 3-Personen Haushalt	11.94
# 4-Personen Haushalt	9.14
# 5 und mehr	3.44
#
# Bildung (%) [5]
# ≤ 9 Jahre	35.75
# 10 Jahre	30. 95
# ≥ 12 Jahre	33.15

# [1] destatis: Statistisches Jahrbuch 2019, S. 26:
# [2] destatis: Statistisches Jahrbuch 2019, S. 31;
# [3] destatis: Haushalte und Familien 2018, S. 34;
# [4] destatis: Haushalte und Familien 2018, S. 34;
# [5] destatis: Bildungsstand der Bevölkerung 2018, S. 21


alter <- c(
  "unter 25" = 7.7,
  "25 bis 39 Jahre" = 19.0,
  "40 bis 59 Jahre" = 29.1,
  "60 bis 64 Jahre" = 6.5,
  "aelter als 65" = 21.4
)

alter_gesis <- rec(
  d$alter,
  rec = "1=1;2,3,4=2;5,6,7=3;8=4;9,10=5",
  val.labels = names(alter)
)
d$age_for_weighting <- alter_gesis

familienstand <- c(
  verheiratet = 42.0,
  ledig = 30.9,
  geschieden = 11.8,
  verwitwet = 11.6
)


uni <- universe(

  data = d[, c("id", "sex", "age_for_weighting", "marstat", "education_cat", "hh_groesse")],

  # Verteilung Geschlecht in der Bevölkerung
  category(
    name = "sex",
    buckets = c("male", "female"),
    targets = c(.493, .507)
  ),

  # Verteilung Altersgruppen in der Bevölkerung
  category(
    name = "age_for_weighting",
    buckets = 1:5,
    targets = unname(prop.table(alter))
  ),

  # Verteilung Familienstand in der Bevölkerung
  category(
    name = "marstat",
    buckets = c("Verheiratet", "Ledig", "Geschieden", "Verwitwet"),
    targets = unname(prop.table(familienstand))
  ),

  # Verteilung Bildung in der Bevölkerung
  category(
    name = "education_cat",
    buckets = c("Gering", "Mittel", "Hoch"),
    targets = prop.table(c(.357, .309, .331))
  ),

  # Verteilung Haushaltsgröße in der Bevölkerung
  category(
    name = "hh_groesse",
    buckets = c(1, 2, 3),
    targets = prop.table(c(41.9, 33.8, 24.3))
  )
)


d_wgt <- iterake(universe = uni, max.iter = 200)
d$weight <- d_wgt$weight
