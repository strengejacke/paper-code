# ---------------------------------------------
# fehlende label setzen
# ---------------------------------------------
eisced_value_labels <- c("low (lower/upper secondary)", "mid (post-secondary)", "high (tertiary)")
set_label(ess$education) <- "Educational level"
set_labels(ess$education) <- eisced_value_labels

set_label(ess$education_d) <- "Educational level (dichotomized)"
set_labels(ess$education_d) <- c("low education", "high education")

set_label(ess$hhinc_d) <- "Household Netincome, dicho"
set_labels(ess$hhinc_d) <- c("low", "high")

set_label(ess$hhinc_3) <- "Household Netincome, tertiles"
set_labels(ess$hhinc_3) <- c("lower tertile", "middle tertile", "upper tertile")

set_labels(ess$hhinc_med) <- c("low", "high")
set_label(ess$hhinc_med) <- "Household's total net income, median-split"

set_label(ess$aquinc) <- "Household net equivalent income"

set_label(ess$aquinc_d) <- "Household net equivalent income, dicho"
set_labels(ess$hhinc_d) <- c("low", "high")

set_label(ess$aquinc_med) <- "Household's net equivalent income, median-split"
set_labels(ess$aquinc_med) <- c("low", "high")


# -----------------------------------------------
# ISEI 3 Kategorien
# -----------------------------------------------
ess$isei_3 <- split_var(ess$isei,
                        groupcount = 3,
                        var.label = "ISEI (3 categories)",
                        val.labels = c("lower tertile",
                                       "middle tertile",
                                       "upper tertile"))

# -----------------------------------------------
# Gedrehte Skalen, damit Regression besser lesbar
# -----------------------------------------------
ess$education_r <- rec(ess$education, "rev", to.factor = TRUE)
ess$isei_3_r <- rec(ess$isei_3, "rev", to.factor = TRUE)
ess$hhinc_3_r <- rec(ess$hhinc_3, "rev", to.factor = TRUE)


# -----------------------------------------------
# Hampered in daily activities, dicho
# -----------------------------------------------
ess$hlthhmp_d <- rec(ess$hlthhmp, "1:2=1;3=0;else=NA", to.factor = TRUE, val.labels = c("No", "Yes"))


# -----------------------------------------------
# Einkommen, missing als gültige Kategorie
# -----------------------------------------------
ess$hhinc_med3 <- rec(ess$hhinc_med, "1=0;2=2;NA=1", to.factor = TRUE, val.labels = c("low", "missing", "high"))
ess$aquinc_med3 <- rec(ess$aquinc_med, "1=0;2=2;NA=1", to.factor = TRUE, val.labels = c("low", "missing", "high"))


# -----------------------------------------------
# Alter zentrieren und standardisieren
# -----------------------------------------------
ess$agea_c <- center(ess$agea)
ess$agea_s <- std(ess$agea)
set_label(ess$agea_c) <- "Age of respondent, centered"
set_label(ess$agea_s) <- "Age of respondent, standardized"


# -----------------------------------------------
# Response Rates über alle 7 Wellen
# -----------------------------------------------
ess$resrate <- rec(ess$cntry, recodes = "BE=58.5; CH=48.7; DE=43.6; DK=56.0; ES=63.9;
                   FI=66.6; FR=47.5; GB=52.8; HU=61.4; IE=61.3; NL=59.4; NO=60.6;
                   PL=71.3; PT=67.9; SE=59.5; SI=62.8",
                   to.factor = FALSE, var.label = "Response Rate Mean (2002-2014)")

ess$rrmin <- rec(ess$cntry, recodes = "BE=53.4; CH=33.5; DE=30.5; DK=49.1; ES=53.2;
                 FI=59.5; FR=43.1; GB=43.6; HU=49.2; IE=51.6; NL=49.8; NO=53.9; PL=65.8; PT=43.0;
                 SE=50.1; SI=52.3",
                 to.factor = FALSE, var.label = "Response Rate Min (2002-2014)")

ess$rrmax <- rec(ess$cntry, recodes = "BE=61.2; CH=53.3; DE=55.7; DK=67.7; ES=70.3;
                 FI=73.2; FR=52.1; GB=56.3; HU=69.9; IE=67.8; NL=67.9; NO=66.2; PL=74.9; PT=77.1;
                 SE=69.5; SI=70.5",
                 to.factor = FALSE, var.label = "Response Rate Min (2002-2014)")

# ess$resrate <- round(ess$resrate)
# ess$rrmin <- round(ess$rrmin)
# ess$rrmax <- round(ess$rrmax)


# -----------------------------------------------
# Education dichotomisieren
# -----------------------------------------------
ess$education_d <- rec(ess$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                       var.label = "Educational level (ISCED)")

ess$education_d[which(ess$eisced == 0 & ess$edulvlb %in% c(1, 2, 3))] <- 0
ess$education_d[which(ess$eisced == 0 & ess$edulvlb %in% c(4, 5))] <- 1
set_labels(ess$education_d) <- c("lower education", "higher education")


# -----------------------------------------------
# Variablen kategorial
# -----------------------------------------------
ess$gndr_n <- to_value(ess$gndr)
ess$wave <- to_factor(ess$essround)
ess$gndr <- to_factor(ess$gndr)
ess$aquinc_med <- to_factor(ess$aquinc_med)


# -----------------------------------------------
# Alter dichotomisieren, für Reviewer-Kommentare
# -----------------------------------------------
ess$age_dicho2 <- dicho(
  ess$agea, 74,
  to.factor = TRUE,
  var.label = "Age, dichotomized",
  val.labels = c("60 to 74", "75 and older")
)
ess$age_dicho <- dicho(
  ess$agea,
  69,
  to.factor = TRUE,
  var.label = "Age, dichotomized",
  val.labels = c("60 to 69", "70 and older")
)
