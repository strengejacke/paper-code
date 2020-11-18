suppressMessages({
  library(tidyverse)
  library(strengejacke)
  library(easystats)
})

ego_de <- read_spss("Daten/01_DEU_AM_20180207.sav")

ego_de <- ego_de %>%
  drop_labels() %>%
  as_label(VigB, VigDTM, VigMW, country, q100, kontakt_nah, obese) %>%
  center(q101, bmi, append = T) %>%
  rec(s7, rec = "2=0;1=1;else=copy", as.num = F, val.labels = c("nein", "ja"), append = T)

ego_de$selv1_l <- set_labels(
  ego_de$selv1,
  labels = c("RA, weiblich", "RA, männlich", "RK, weiblich", "HM, männlich",
             "RA, weiblich (mig)", "RA, männlich (mig)", "RK, weiblich (mig)", "HM, männlich (mig)")
)

ego_de$selv1_l <- as_label(ego_de$selv1_l)
ego_de$sex <- to_value(ego_de$q100, start.at = 0)
ego_de$ueber <- to_value(ego_de$obese, start.at = 0)
ego_de$kontakt_n <- to_value(ego_de$kontakt_nah, start.at = 0)

ego_de$obese_exp <- case_when(
  ego_de$s3 %in% c(3, 4) ~ 1,
  ego_de$s4 == 1 ~ 1,
  is.na(ego_de$s3) & is.na(ego_de$s4) ~ as.double(NA),
  TRUE ~ 0
)

set_label(ego_de$obese_exp) <- "Overweight experience"
ego_de$obese_exp <- set_labels(ego_de$obese_exp, labels = c("no", "yes"))
ego_de$experience <- as_label(ego_de$obese_exp)
