suppressMessages({
  library(tidyverse)
  library(strengejacke)
  library(easystats)
})

ego_us <- read_spss("Daten/02_USA_AM_20180118.sav")

ego_us <- ego_us %>%
  drop_labels() %>%
  as_label(VigB, VigDTM, VigMW, country, q100, kontakt_nah, obese) %>%
  center(q101, bmi, append = T) %>%
  rec(s7, rec = "2=0;1=1;else=copy", as.num = F, val.labels = c("nein", "ja"), append = T)

ego_us$selv1_l <- set_labels(
  ego_us$selv1,
  labels = c("RA, weiblich", "RA, männlich", "RK, weiblich", "HM, männlich",
             "RA, weiblich (mig)", "RA, männlich (mig)", "RK, weiblich (mig)", "HM, männlich (mig)")
)

ego_us$selv1_l <- as_label(ego_us$selv1_l)
ego_us$sex <- to_value(ego_us$q100, start.at = 0)
ego_us$ueber <- to_value(ego_us$obese, start.at = 0)
ego_us$kontakt_n <- to_value(ego_us$kontakt_nah, start.at = 0)

ego_us$obese_exp <- case_when(
  ego_us$s3 %in% c(3, 4) ~ 1,
  ego_us$s4 == 1 ~ 1,
  is.na(ego_us$s3) & is.na(ego_us$s4) ~ as.double(NA),
  TRUE ~ 0
)

set_label(ego_us$obese_exp) <- "Overweight experience"
ego_us$obese_exp <- set_labels(ego_us$obese_exp, labels = c("no", "yes"))
ego_us$experience <- as_label(ego_us$obese_exp)
