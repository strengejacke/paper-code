library(parameters)
library(sjlabelled)
library(performance)
library(datawizard)
library(mokken)

pc <- principal_components(remove_all_labels(qualidem_pp), rotation = "varimax")
pc

check_itemscale(pc)

item_reliability(QD1_pflegebez_pp)

# Bouman AIE, Ettema TP, Wetzels RB, et al. Evaluation of Qualidem: a
# dementia-specific quality of life instrument for persons with dementia in
# residential settings; scalability and reliability of subscales in four Dutch
# field surveys. International Journal of Geriatric Psychiatry 2011;26:711–22.
# doi:10.1002/gps.2585

sum(apply(qualidem_pp, c(1, 2), is.na)) / (nrow(qualidem_pp) * ncol(qualidem_pp))

sum(apply(qualidem_pp, c(1, 2), is.na))
(nrow(qualidem_pp) * ncol(qualidem_pp))

set.seed(1207)
QD1_pflegebez_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD1_pflegebez_pp))))
QD2_posaf_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD2_posaf_pp))))
QD3_negaf_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD3_negaf_pp))))
QD4_ruhelos_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD4_ruhelos_pp))))
QD5_selbst_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD5_selbst_pp))))
QD6_bez_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD6_bez_pp))))
QD7_iso_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD7_iso_pp))))
QD8_home_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD8_home_pp))))
QD9_sinn_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD9_sinn_pp))))

coefH(QD1_pflegebez_2)[c("Hi", "H")]
coefH(QD2_posaf_2)[c("Hi", "H")]
coefH(QD3_negaf_2)[c("Hi", "H")]
coefH(QD4_ruhelos_2)[c("Hi", "H")]
coefH(QD5_selbst_2)[c("Hi", "H")]
coefH(QD6_bez_2)[c("Hi", "H")]
coefH(QD7_iso_2)[c("Hi", "H")]
coefH(QD8_home_2)[c("Hi", "H")]
coefH(QD9_sinn_2)[c("Hi", "H")]

check.reliability(QD1_pflegebez_2)["MS"][[1]] %>% round(2)
check.reliability(QD2_posaf_2)["MS"][[1]] %>% round(2)
check.reliability(QD3_negaf_2)["MS"][[1]] %>% round(2)
check.reliability(QD4_ruhelos_2)["MS"][[1]] %>% round(2)
check.reliability(QD5_selbst_2)["MS"][[1]] %>% round(2)
check.reliability(QD6_bez_2)["MS"][[1]] %>% round(2)
check.reliability(QD7_iso_2)["MS"][[1]] %>% round(2)
check.reliability(QD8_home_2)["MS"][[1]] %>% round(2)
check.reliability(QD9_sinn_2)["MS"][[1]] %>% round(2)

summary(check.monotonicity(QD1_pflegebez_2))
summary(check.monotonicity(QD2_posaf_2))
summary(check.monotonicity(QD3_negaf_2))
summary(check.monotonicity(QD4_ruhelos_2))
summary(check.monotonicity(QD5_selbst_2))
summary(check.monotonicity(QD6_bez_2))
summary(check.monotonicity(QD7_iso_2))
summary(check.monotonicity(QD8_home_2))
summary(check.monotonicity(QD9_sinn_2))


cronbachs_alpha(QD1_pflegebez_2) %>% round(2)
cronbachs_alpha(QD2_posaf_2) %>% round(2)
cronbachs_alpha(QD3_negaf_2) %>% round(2)
cronbachs_alpha(QD4_ruhelos_2) %>% round(2)
cronbachs_alpha(QD5_selbst_2) %>% round(2)
cronbachs_alpha(QD6_bez_2) %>% round(2)
cronbachs_alpha(QD7_iso_2) %>% round(2)
cronbachs_alpha(QD8_home_2) %>% round(2)
cronbachs_alpha(QD9_sinn_2) %>% round(2)


item_intercor(QD1_pflegebez_2) %>% round(2)
item_intercor(QD2_posaf_2) %>% round(2)
item_intercor(QD3_negaf_2) %>% round(2)
item_intercor(QD4_ruhelos_2) %>% round(2)
item_intercor(QD5_selbst_2) %>% round(2)
item_intercor(QD6_bez_2) %>% round(2)
item_intercor(QD7_iso_2) %>% round(2)
item_intercor(QD8_home_2) %>% round(2)
item_intercor(QD9_sinn_2) %>% round(2)
