library(easystats)
library(dplyr)
library(sjlabelled)
library(sjmisc)
library(easystats)
library(survey)
library(ggeffects)
library(ggplot2)
library(patchwork)
library(emmeans)


# Daten laden -------------------

load("oxford_for_figures.RData")
load("share8.RData")



# Prepare data (variale selection, coerce to categorical) -------------------

d <- filter(share8, !is.na(crossin_weights)) |>
  to_label(gender, isolation_any) |>
  standardise(select = "age_2020", append = TRUE) |>
  rename_variables(index_str = "Stringency",
                   age_2020_z = "Age",
                   isolation_any = "few_contacts")

# Stringency Index as factor, set labels
d$index_str <- factor(
  d$Stringency,
  labels = c("low response", "mid response", "high response")
)

# normalize percentage
d$perc_covid <- normalize(d$covid_percent)


# subset of all cases for more depressed
d_depressed <- select(d, more_depressed2, Age, gender, Education,
                      living_alone, few_contacts, Stringency, perc_covid,
                      mergeid, country, crossin_weights)

d_depressed <- d_depressed[complete.cases(d_depressed), ]


# subset of all cases for more nervous
d_nervous <- select(d, more_nervous2, Age, gender, Education, living_alone,
                    few_contacts, Stringency, perc_covid, mergeid,
                    country, crossin_weights)

d_nervous <- d_nervous[complete.cases(d_nervous), ]



# Setup Survey-Design ------------

## pooled sample

des <- svydesign(
  ids = ~mergeid,
  strata = ~country,
  weights = ~crossin_weights,
  data = d
)


## pooled sample, exclude all missings (so all models have same number of observations)

design_depression <- svydesign(
  ids = ~mergeid,
  strata = ~country,
  weights = ~crossin_weights,
  data = d_depressed
)


## pooled sample, exclude all missings (so all models have same number of observations)

design_nervous <- svydesign(
  ids = ~mergeid,
  strata = ~country,
  weights = ~crossin_weights,
  data = d_nervous
)






# Abbildungen ------------------------

## Trend Stringency Index ----------------------

qunt <- c(40, 45)
oxford_reduced$index_str <- NA
oxford_reduced$index_str[oxford_reduced$mean_str_index <= qunt[1]] <- "low stringency"
oxford_reduced$index_str[oxford_reduced$mean_str_index > qunt[1] & oxford_reduced$mean_str_index <= qunt[2]] <- "medium stringency"
oxford_reduced$index_str[oxford_reduced$mean_str_index > qunt[2]] <- "high stringency"

oxford_reduced$index_str <- factor(
  oxford_reduced$index_str,
  levels = c("low stringency", "medium stringency", "high stringency")
)

oxford_reduced$index_str <- set_label(
  oxford_reduced$index_str,
  label = "Stringency Index 3 Groups (split at 40/45)"
)

oxford_reduced$days <- as.Date(as.character(oxford_reduced$Date), "%Y%m%d")

index_trend <- oxford_reduced |>
  group_by(days, index_str) |>
  summarise(mean_str_index = mean(StringencyIndex, na.rm = TRUE))

ggplot(index_trend, aes(x = days, y = mean_str_index, colour = index_str)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month") +
  labs(x = NULL, y = "Stringency Index", colour = "COVID-Regime") +
  theme_lucid(legend.position = "bottom", axis.text.angle = 90) +
  scale_color_see()




## Einteilung Länder nach Stringency Index --------------------

x <- d[!duplicated(d$country), ]
x$country <- factor(x$country, levels = x$country[order(x$mean_str_index)])

ggplot(x, aes(x = mean_str_index, y = country, colour = Stringency)) +
  geom_point() +
  theme_lucid(legend.position = "bottom") +
  labs(y = NULL, x = "Average Stringency Index", colour = "COVID-Regime") +
  scale_color_see()







# Tabelle: Sample description by country, more depressed ------------------------

## low Stringency countries

d |>
  to_label(more_depressed2) |>
  filter(!is.na(more_depressed2), Stringency == "low") |>
  report_sample(
    group_by = "country",
    select = c("age_2020", "Education", "gender",
               "few_contacts", "living_alone", "more_depressed2"),
    digits = 1
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")


## mid Stringency countries

d |>
  to_label(more_depressed2) |>
  filter(!is.na(more_depressed2), Stringency == "middle") |>
  report_sample(
    group_by = "country",
    select = c("age_2020", "Education", "gender",
               "few_contacts", "living_alone", "more_depressed2"),
    digits = 1
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")


## high Stringency countries

d |>
  to_label(more_depressed2) |>
  filter(!is.na(more_depressed2), Stringency == "high") |>
  report_sample(
    group_by = "country",
    select = c("age_2020", "Education", "gender",
               "few_contacts", "living_alone", "more_depressed2"),
    digits = 1
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")





# Tabelle: Sample description by country, more nervous ------------------------

## low Stringency countries

d |>
  to_label(more_nervous2) |>
  filter(!is.na(more_nervous2), Stringency == "low") |>
  report_sample(
    group_by = "country",
    select = c("age_2020", "Education", "gender",
               "few_contacts", "living_alone", "more_nervous2"),
    digits = 1
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")


## mid Stringency countries

d |>
  to_label(more_nervous2) |>
  filter(!is.na(more_nervous2), Stringency == "middle") |>
  report_sample(
    group_by = "country",
    select = c("age_2020", "Education", "gender",
               "few_contacts", "living_alone", "more_nervous2"),
    digits = 1
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")


## high Stringency countries

d |>
  to_label(more_nervous2) |>
  filter(!is.na(more_nervous2), Stringency == "high") |>
  report_sample(
    group_by = "country",
    select = c("age_2020", "Education", "gender",
               "few_contacts", "living_alone", "more_nervous2"),
    digits = 1
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")






# Tabelle: Sample description by country, infection rate and mean SI ------------------------

## low Stringency countries

d |>
  select(country, mean_str_index, covid_percent, Stringency) |>
  unique() |>
  filter(Stringency == "low") |>
  report_sample(
    group_by = "country",
    select = c("mean_str_index", "covid_percent"),
    digits = 2
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")

## mid Stringency countries

d |>
  select(country, mean_str_index, covid_percent, Stringency) |>
  unique() |>
  filter(Stringency == "middle") |>
  report_sample(
    group_by = "country",
    select = c("mean_str_index", "covid_percent"),
    digits = 2
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")

## high Stringency countries

d |>
  select(country, mean_str_index, covid_percent, Stringency) |>
  unique() |>
  filter(Stringency == "high") |>
  report_sample(
    group_by = "country",
    select = c("mean_str_index", "covid_percent"),
    digits = 2
  ) |>
  sjmisc::rotate_df(rn = TRUE, cn = TRUE) |>
  export_table(format = "html")



# Tabelle: Finale Regressionsmodelle --------------------------

## Stepwise more depressed --------------------------

# individual characteristics
m1 <- svyglm(more_depressed2 ~ Age + gender + Education + living_alone + few_contacts,
             design = design_depression,
             family = quasibinomial())

# macro indicators
m2 <- svyglm(more_depressed2 ~ Stringency + perc_covid,
             design = design_depression,
             family = quasibinomial())

# full model
m3 <- svyglm(more_depressed2 ~ Age + gender + Education + living_alone + few_contacts + Stringency + perc_covid,
             design = design_depression,
             family = quasibinomial())


check_collinearity(m1)
check_collinearity(m2)
check_collinearity(m3)

sjPlot::tab_model(
  m1, m2, m3,
  df.method = "wald",
  pred.labels = c("Intercept", "Age", "Female gender", "Education: mid",
                  "Education: high", "Living alone", "Social loneliness",
                  "SI mdi", "SI high", "% Covid cases")
)





## Stepwise more nervous --------------------------

# individual characteristics
m1 <- svyglm(more_nervous2 ~ Age + gender + Education + living_alone + few_contacts,
             design = design_nervous,
             family = quasibinomial())

# macro indicators
m2 <- svyglm(more_nervous2 ~ Stringency + perc_covid,
             design = design_nervous,
             family = quasibinomial())

# full model
m3 <- svyglm(more_nervous2 ~ Age + gender + Education + living_alone + few_contacts + Stringency + perc_covid,
             design = design_nervous,
             family = quasibinomial())

check_collinearity(m1)
check_collinearity(m2)
check_collinearity(m3)

sjPlot::tab_model(
  m1, m2, m3,
  df.method = "wald",
  pred.labels = c("Intercept", "Age", "Female gender", "Education: mid",
                  "Education: high", "Living alone", "Social loneliness",
                  "SI mdi", "SI high", "% Covid cases")
)
