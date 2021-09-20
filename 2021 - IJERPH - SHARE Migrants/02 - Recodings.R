# Filterfrage wegen marital status?

# Recode age ------------------

share$wave_year <- rec(share$wave, rec = "1=2004;2=2006;4=2010;5=2013;6=2015;7=2017", as.num = TRUE)

share <- share %>%
  group_by(wave) %>%
  mutate(age_wave = wave_year - yrbirth) %>%
  ungroup()

share$age_2020 <- 2020 - share$yrbirth
share$age_groups10 <- group_var(share$age_wave, size = 10)
share$age_groups10 <- set_labels(share$age_groups10, labels = group_labels(share$age_wave, size = 10))

# exception: merge small groups
share$age_groups10[share$age_groups10 > 10] <- 10

share$age_groups5 <- group_var(share$age_wave, size = 5)
share$age_groups5 <- set_labels(share$age_groups5, labels = group_labels(share$age_wave, size = 5))

# exception: merge small groups
share$age_groups5[share$age_groups5 > 22] <- 22

# only people 50+
share <- share %>% filter(age_wave >= 50)

# split at 70
share$age_dicho <- rec(share$age_wave,
                       rec = "min:69=0;70:max=1;else=copy",
                       var.label = "Age, dichotomized",
                       val.labels = c("under 70", "70 and older"),
                       as.num = FALSE)





# Recode migration education ------------------

share$isced1997_r <- set_na(share$isced1997_r, na = 95)
share$education <- rec(
  share$isced1997_r,
  rec = "0:2=0;3:4=1;5:6=2;else=NA",
  as.num = FALSE,
  var.label = "Educational level (ISCED)",
  val.labels = c("low (lower/upper secondary)",
                 "mid (post-secondary)",
                 "high (tertiary)"))




# Label gender ------------------------------

share$gender <- set_labels(share$gender, labels = c("1" = "male", "2" = "female"))




# Recode migration background ------------------

# share$migrant <- share$dn004_
# ids <- unique(share$mergeid)
# pb <- utils::txtProgressBar(min = 0, max = length(ids), style = 3)
# for (i in 1:length(ids)) {
#   mig <- share$migrant[share$mergeid == ids[i]]
#   value <- unique(mig[!is.na(mig)])[1]
#   if (!is.na(value) && length(value)) {
#     share$migrant[share$mergeid == ids[i]] <- value
#   }
#   utils::setTxtProgressBar(pb, i)
# }

share$migrant <- share$dn004_
share$country_born <- share$dn005c
share$marital <- share$dn014_
share$got_help <- share$sp020_

share <- share %>%
  group_by(mergeid) %>%
  tidyr::fill(migrant, country_born, marital, got_help, .direction = "downup") %>%
  ungroup()


share$migrant <- factor(share$migrant)
levels(share$migrant) <- c("non-migrant", "migrant")

share$came_to_country <- share$dn006_
share <- share %>%
  group_by(wave) %>%
  mutate(years_in_country = wave_year - came_to_country) %>%
  ungroup()

share$years_in_country[share$years_in_country < 0] <- 0





# Recode age groups -------------------------

share$age_groups5b <- share$age_groups5
share$age_groups5b[share$age_groups5b > 17] <- 18
share$age_groups5b <- drop_labels(share$age_groups5b)

share$age_groups10b <- share$age_groups10
share$age_groups10b[share$age_groups10b > 9] <- 9
share$age_groups10b <- drop_labels(share$age_groups10b)




# other recodings --------------------------

share$iadl_d <- rec(
  share$iadl,
  rec = "0=0;1:max=1;else=copy",
  as.num = FALSE,
  val.labels = c("no IADL limitations", "1+ IADL limitations"),
  var.label = "IADL limitations"
)

share$sphus_d <- rec(
  share$sphus,
  rec = "1:2=0;3:5=1;else=copy",
  as.num = FALSE,
  val.labels = c("(very) good health", "poorer health"),
  var.label = "Self-perceived health"
)

share$limited_adl_d <- rec(
  share$limited_adl,
  rec = "1,2=1;3=0;else=copy",
  as.num = FALSE,
  val.labels = c("not limited", "(severly) limited"),
  var.label = "Limited ADL"
)




# rename categories of wave ----------------

share$welle <- factor(share$wave)
levels(share$welle) <- c("2004", "2006", "2010", "2013", "2015", "2017")




# rename variables -------------------

share <- rename_variables(
  share, ch001_ = "n_of_child", co007_ = "hh_make_ends_meet", dn002_ = "birth_month",
  dn004_ = "born_in_country", dn007_ = "citizen_of_country", dn042_ = "sex",
  ep005_ = "job_situation", hc029_ = "hc_nursing_home", hc031_ = "hc_weeks_in_nursing_home",
  ph003_ = "general_health", ph005_ = "limited_adl"
)

share <- drop_labels(share)
