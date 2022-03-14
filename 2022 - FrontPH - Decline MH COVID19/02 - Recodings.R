# COVID recodings -------------------------------


# generate "reduced contacts" ------------

share8 <- set_na(share8, cah011_1:cah011_4, na = 5)

from <- which(colnames(share8) == "cah011_1")
to <- which(colnames(share8) == "cah011_4")
share8$reduced_public <- mean_n(share8[from:to], n = 2)

from <- which(colnames(share8) == "cah011_3")
to <- which(colnames(share8) == "cah011_4")
share8$reduced_contacts <- mean_n(share8[from:to], n = 1)

from <- which(colnames(share8) == "cah012_")
to <- which(colnames(share8) == "cah013_")
share8$kept_distance <- mean_n(share8[from:to], n = 1)




# generate "reduced contacts" variants ------------

share8$reduced_contacts_d <- dicho(share8$reduced_contacts,
                                        dich.by = 2,
                                        as.num = FALSE,
                                        var.label = "Reduced (public) contacts",
                                        val.labels = c("Reduced", "Same or increased"))
share8$reduced_contacts_d <- rec(share8$reduced_contacts_d, rec = "rev")
share8$reduced_contacts_d <- to_label(share8$reduced_contacts_d)



share8$reduced_public_d <- dicho(share8$reduced_public,
                                      dich.by = 2,
                                      as.num = FALSE,
                                      var.label = "Reduced being in public",
                                      val.labels = c("Reduced", "Same or increased"))
share8$reduced_public_d <- rec(share8$reduced_public_d, rec = "rev")
share8$reduced_public_d <- to_label(share8$reduced_public_d)



share8$kept_distance_d <- dicho(share8$kept_distance,
                                     dich.by = 2,
                                     as.num = FALSE,
                                     var.label = "Wore mask and/or kept distance",
                                     val.labels = c("Often or always", "Sometimes or never"))
share8$kept_distance_d <- rec(share8$kept_distance_d, rec = "rev")
share8$kept_distance_d <- to_label(share8$kept_distance_d)




# covid contacts ------------

share8$contact_symptoms <- factor(share8$cac002_, labels = c("yes", "no"))
share8$contact_symptoms <- set_label(share8$contact_symptoms,
                                          label = "Contact to anyone had COVID-19 symptoms")

share8$contact_positive <- factor(share8$cac004_, labels = c("yes", "no"))
share8$contact_positive <- set_label(share8$contact_positive,
                                          label = "Contact to anyone tested positive for COVID-19")

share8$contact_symptoms2 <- rec(share8$cac002_, rec = "5=0;1=1;else=copy",
                                     val.labels = c("no", "yes"), as.num = FALSE,
                                     var.label = "Contact to anyone had COVID-19 symptoms")

share8$contact_positive2 <- rec(share8$cac004_, rec = "5=0;1=1;else=copy",
                                     val.labels = c("no", "yes"), as.num = FALSE,
                                     var.label = "Contact to anyone tested positive for COVID-19")




# place of work / employment -------------

share8$workplace <- to_label(share8$caw010_)
share8$employment <- rec(share8$ep005_,
                              rec = "2=1;1,3,4,5=0;else=copy",
                              var.label = "Employment status",
                              val.labels = c("unemployed or retired", "employed"),
                              as.num = FALSE)




# hygiene measures ----------------

share8$hygiene_measures <- case_when(
  share8$cah014_ == 1 ~ 1,
  share8$cah015_ == 1 ~ 1,
  share8$cah016_ == 1 ~ 1,
  TRUE ~ 0
)

share8$hygiene_wash_and_disinfect <- case_when(
  share8$cah014_ == 1 & share8$cah015_ == 1 ~ 1,
  TRUE ~ 0
)



# factors ---------------

share8$gender <- to_label(share8$gender)
share8$country <- to_label(share8$country)

share8$hygiene_wash_and_disinfect <- factor(share8$hygiene_wash_and_disinfect, labels = c("no", "yes"))
share8$hygiene_measures <- factor(share8$hygiene_measures, labels = c("no", "yes"))




# mental health and wellbeing ------------------


## nervous -------------


# dichotom
share8$more_nervous <- rec(share8$cah021_,
                                rec = "1=1;else=0;NA=0",
                                var.label = "felt more nervous since outbreak",
                                val.labels = c("no", "yes"))
# set back original NA
share8$more_nervous[is.na(share8$cah020_)] <- NA

# dichotom, nervous last month only
share8$more_nervous2 <- share8$more_nervous
share8$more_nervous2[share8$cah020_ == 5] <- NA

# ordinal
share8$more_nervous3 <- rec(share8$cah021_,
                                 rec = "1=2;2,3=1;NA=0",
                                 as.num = FALSE,
                                 var.label = "felt more nervous since outbreak",
                                 val.labels = c("never nervous", "no", "yes"))
share8$more_nervous3[is.na(share8$cah020_)] <- NA



## depressed ----------------


# dichotom
share8$more_depressed <- rec(share8$camh802_,
                                  rec = "1=1;else=0;NA=0",
                                  var.label = "felt more depressed since outbreak",
                                  val.labels = c("no", "yes"))
# set back original NA
share8$more_depressed[is.na(share8$camh002_)] <- NA

# ordinal
share8$more_depressed3 <- rec(share8$camh802_,
                                   rec = "1=2;2,3=1;NA=0",
                                   as.num = FALSE,
                                   var.label = "felt more depressed since outbreak",
                                   val.labels = c("never depressed", "no", "yes"))
share8$more_depressed3[is.na(share8$camh002_)] <- NA

# dichotom, depressed last month only
share8$more_depressed2 <- share8$more_depressed
share8$more_depressed2[share8$camh002_ == 5] <- NA



## sleeptrouble -----------------


# dichotom
share8$more_sleeptrouble <- rec(share8$camh807_,
                                     rec = "1=1;else=0;NA=0",
                                     var.label = "had more trouble sleeping since outbreak",
                                     val.labels = c("no", "yes"))
# set back original NA
share8$more_sleeptrouble[is.na(share8$camh007_)] <- NA

# ordinal
share8$more_sleeptrouble3 <- rec(share8$camh807_,
                                      rec = "1=2;2,3=1;NA=0",
                                      as.num = FALSE,
                                      var.label = "had more trouble sleeping since outbreak",
                                      val.labels = c("never sleeptroubles", "no", "yes"))
share8$more_sleeptrouble3[is.na(share8$camh007_)] <- NA



## lonely -----------------


# dichotom
share8$more_lonely <- rec(share8$camh837_,
                               rec = "1=1;else=0;NA=0",
                               var.label = "feel more lonely since outbreak",
                               val.labels = c("no", "yes"))
# set back original NA
share8$more_lonely[is.na(share8$camh037_)] <- NA

# ordinal
share8$more_lonely3 <- rec(share8$camh837_,
                                rec = "1=2;2,3=1;NA=0",
                                as.num = FALSE,
                                var.label = "feel more lonely since outbreak",
                                val.labels = c("never lonely", "no", "yes"))
# set back original NA
share8$more_lonely3[is.na(share8$camh037_)] <- NA




# social contact frequencies --------------------

## copy original variables

share8$contact_children <- share8$cas003_1
share8$contact_parents <- share8$cas003_2
share8$contact_relatives <- share8$cas003_3
share8$contact_friends <- share8$cas003_4



## family isolation

share8$isolation_family <- NA
share8$isolation_family[share8$contact_children %in% 1:3 | share8$contact_relatives %in% 1:3 |  share8$contact_parents %in% 1:3] <- 0
share8$isolation_family[share8$contact_children > 3 & share8$contact_relatives > 3 & share8$contact_parents > 3] <- 1
share8$isolation_family <- factor(share8$isolation_family, labels = c("no", "yes"))

share8$isolation_non_family <- NA
share8$isolation_non_family[share8$contact_friends %in% 1:3] <- 0
share8$isolation_non_family[share8$contact_friends > 3] <- 1
share8$isolation_non_family <- factor(share8$isolation_non_family, labels = c("no", "yes"))

share8$isolation_any <- NA
share8$isolation_any[share8$isolation_family == "no" | share8$isolation_non_family == "no"] <- 0
share8$isolation_any[share8$isolation_family == "yes" & share8$isolation_non_family == "yes"] <- 1
share8$isolation_any <- factor(share8$isolation_any, labels = c("no", "yes"))



share8$isolation_family2 <- NA
share8$isolation_family2[share8$contact_children %in% 1:3 | share8$contact_relatives %in% 1:3 |  share8$contact_parents %in% 1:3] <- 0
share8$isolation_family2[share8$contact_children > 3 & share8$contact_relatives %in% 4:5 & share8$contact_parents %in% 4:5] <- 1
share8$isolation_family2 <- factor(share8$isolation_family2, labels = c("no", "yes"))

share8$isolation_non_family2 <- NA
share8$isolation_non_family2[share8$contact_friends %in% 1:3] <- 0
share8$isolation_non_family2[share8$contact_friends %in% 4:5] <- 1
share8$isolation_non_family2 <- factor(share8$isolation_non_family2, labels = c("no", "yes"))

share8$isolation_any2 <- NA
share8$isolation_any2[share8$isolation_family2 == "no" | share8$isolation_non_family2 == "no"] <- 0
share8$isolation_any2[share8$isolation_family2 == "yes" & share8$isolation_non_family2 == "yes"] <- 1
share8$isolation_any2 <- factor(share8$isolation_any2, labels = c("no", "yes"))


# social isolation

share8$social_isolation <- share8$isolation_any
share8$social_isolation[share8$partnerinhh == 1] <- "no"
share8$social_isolation[is.na(share8$social_isolation) & share8$living_alone == "yes"] <- "yes"



## most frequent contact

share8 <- set_na(share8, cas003_1:cas003_4, na = 99)
share8$contact_frequency <- apply(
  select(share8, cas003_1:cas003_4), 1,
  function(i) {
    if (all(is.na(i))) {
      NA
    } else {
      min(i, na.rm = TRUE)
    }
  })

set_label(share8$contact_frequency) <- "Most frequent contacts with family or friends since outbreak"
share8$contact_frequency <- set_labels(share8$contact_frequency,
                                            labels = get_labels(share8$cas003_1))

share8$contact_frequency_rev <- rec(share8$contact_frequency, rec = "rev")


## total contacts

total_cons <- share8 %>%
  select(cas003_1:cas003_4) %>%
  set_na(na = 99) %>%
  rec(rec = "rev", append = FALSE)

share8$total_contacts <- sjmisc::row_sums(total_cons, n = 2)[["rowsums"]]
set_label(share8$total_contacts) <- "Total contact frequency with family or friends since outbreak"


share8$mean_contacts <- sjmisc::row_means(total_cons, n = 2)[["rowmeans"]]
set_label(share8$mean_contacts) <- "Average contact frequency with family and friends since outbreak"




# living alone --------------------

share8$living_alone <- ifelse(share8$hhsize > 1, 0, 1)
share8$living_alone <- factor(share8$living_alone, labels = c("no", "yes"))





# make ends meet --------------------

share8$make_ends_meet <- NULL
share8$make_ends_meet2 <- ifelse(share8$co007_ < 3, 0, 1)
share8$make_ends_meet2 <- factor(share8$make_ends_meet2, labels = c("difficult", "easy"))





# weights ---------------

share8 <- datawizard::rescale_weights(share8, "country", "crossin_weights")




# Filterfrage wegen marital status?

# Recode age ------------------

share8$wave_year <- rec(share8$wave, rec = "1=2004;2=2006;4=2010;5=2013;6=2015;7=2017;8=2020", as.num = TRUE)

share8 <- share8 %>%
  group_by(wave) %>%
  mutate(age_wave = wave_year - yrbirth) %>%
  ungroup()

share8$age_2020 <- 2020 - share8$yrbirth
share8$age_groups10 <- group_var(share8$age_wave, size = 10)
share8$age_groups10 <- set_labels(share8$age_groups10, labels = group_labels(share8$age_wave, size = 10))

# exception: merge small groups
share8$age_groups10[share8$age_groups10 > 10] <- 10

share8$age_groups5 <- group_var(share8$age_wave, size = 5)
share8$age_groups5 <- set_labels(share8$age_groups5, labels = group_labels(share8$age_wave, size = 5))

# exception: merge small groups
share8$age_groups5[share8$age_groups5 > 22] <- 22

# split at 70
share8$age_dicho <- rec(share8$age_wave,
                       rec = "min:69=0;70:max=1;else=copy",
                       var.label = "Age, dichotomized",
                       val.labels = c("under 70", "70 and older"),
                       as.num = FALSE)





# Recode education ------------------

share8$isced1997_r <- set_na(share8$isced1997_r, na = 95)
share8$education8 <- rec(
  share8$isced1997_r,
  rec = "0:2=0;3:4=1;5:6=2;else=NA",
  as.num = FALSE,
  var.label = "Educational level (ISCED)",
  val.labels = c("low (lower/upper secondary)",
                 "mid (post-secondary)",
                 "high (tertiary)"))

# fill missing with information from last wave

share8$education8[is.na(share8$education8)] <- share8$education[is.na(share8$education8)]
share8$edu_label <- to_label(share8$education8)
share8$education <- NULL



# renaming --------------------


share8 <- rename_variables(
  share8,
  reduced_contacts_d = "meet_people",
  reduced_public_d = "being_outside",
  edu_label = "Education",
  contact_frequency_rev = "Contact_Frq",
  ch001_ = "n_of_child",
  co007_ = "hh_make_ends_meet",
  dn002_ = "birth_month",
  dn004_ = "born_in_country",
  dn007_ = "citizen_of_country",
  dn042_ = "sex",
  ep005_ = "job_situation",
  hc029_ = "hc_nursing_home",
  hc031_ = "hc_weeks_in_nursing_home",
  ph003_ = "general_health",
  ph005_ = "limited_adl"
)

levels(share8$Education) <- c("low", "mid", "high")



# Label gender ------------------------------

share8$gender <- set_labels(share8$gender, labels = c("1" = "male", "2" = "female"))




# Recode migration background ------------------

share8$country_born <- share8$dn005c
share8$marital <- share8$dn014_
share8$got_help <- share8$sp020_
share8$came_to_country <- share8$dn006_





# other recodings --------------------------

share8$iadl_d <- rec(
  share8$iadl,
  rec = "0=0;1:max=1;else=copy",
  as.num = FALSE,
  val.labels = c("no IADL limitations", "1+ IADL limitations"),
  var.label = "IADL limitations"
)

share8$sphus_d <- rec(
  share8$sphus,
  rec = "1:2=0;3:5=1;else=copy",
  as.num = FALSE,
  val.labels = c("(very) good health", "poorer health"),
  var.label = "Self-perceived health"
)

share8$limited_adl_d <- rec(
  share8$limited_adl,
  rec = "1,2=1;3=0;else=copy",
  as.num = FALSE,
  val.labels = c("not limited", "(severly) limited"),
  var.label = "Limited ADL"
)






# clean ---------------

share8 <- drop_labels(share8)
share8[duplicated(colnames(share8))] <- NULL
