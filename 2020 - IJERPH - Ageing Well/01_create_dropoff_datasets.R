library(dplyr)
library(sjlabelled)
library(sjmisc)

deas_1996 <- read_spss("../SPSS-Daten/SUF_DEAS1996_3-0_en_SPSS.sav") %>%
  drop_labels() %>%
  remove_empty_cols() %>%
  dplyr::filter(dostatus_96 == 1)

deas_2002 <- read_spss("../SPSS-Daten/SUF_DEAS2002_3-0_en_SPSS.sav") %>%
  drop_labels() %>%
  remove_empty_cols() %>%
  dplyr::filter(dostatus_02 == 1)

deas_2008 <- read_spss("../SPSS-Daten/SUF_DEAS2008_3-0_en_SPSS.sav") %>%
  drop_labels() %>%
  remove_empty_cols() %>%
  dplyr::filter(dostatus_08 == 1)

deas_2011 <- read_spss("../SPSS-Daten/SUF_DEAS2011_2-0_en_SPSS.sav") %>%
  drop_labels() %>%
  remove_empty_cols() %>%
  dplyr::filter(dostatus_11 == 1)

deas_2014 <- read_spss("../SPSS-Daten/SUF_DEAS2014_1-0_en_SPSS.sav") %>%
  drop_labels() %>%
  remove_empty_cols() %>%
  dplyr::filter(dostatus_14 == 1)

deas_2017 <- read_spss("../SPSS-Daten/SUF_DEAS2017_1-0_en_SPSS.sav") %>%
  drop_labels() %>%
  remove_empty_cols() %>%
  dplyr::filter(dostatus_17 == 1)

deas_meta <- read_spss("../SPSS-Daten/SUF_DEAS_Meta_04-2018_en_SPSS.sav") %>% drop_labels()

deas_1996$year <- 1996
deas_2002$year <- 2002
deas_2008$year <- 2008
deas_2011$year <- 2011
deas_2014$year <- 2014
deas_2017$year <- 2017
