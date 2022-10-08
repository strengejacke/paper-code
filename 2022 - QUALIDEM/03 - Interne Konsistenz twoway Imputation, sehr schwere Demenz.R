library(parameters)
library(sjlabelled)
library(performance)
library(datawizard)
library(mokken)

pc <- principal_components(remove_all_labels(qualidem_s_pp), rotation = "varimax")
pc

check_itemscale(pc)

item_reliability(QD1_pflegebez_pp)

# Bouman AIE, Ettema TP, Wetzels RB, et al. Evaluation of Qualidem: a
# dementia-specific quality of life instrument for persons with dementia in
# residential settings; scalability and reliability of subscales in four Dutch
# field surveys. International Journal of Geriatric Psychiatry 2011;26:711–22.
# doi:10.1002/gps.2585

sum(apply(qualidem_s_pp, c(1, 2), is.na)) / (nrow(qualidem_s_pp) * ncol(qualidem_s_pp))

sum(apply(qualidem_s_pp, c(1, 2), is.na))
(nrow(qualidem_s_pp) * ncol(qualidem_s_pp))

set.seed(1207)
QD1s_pflegebez_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD1s_pflegebez_pp))))
QD2s_posaf_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD2s_posaf_pp))))
QD3s_negaf_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD3s_negaf_pp))))
QD4s_ruhelos_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD4s_ruhelos_pp))))
QD6s_bez_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD6s_bez_pp))))
QD7s_iso_2 <- as.data.frame(twoway(as.data.frame(remove_empty_rows(QD7s_iso_pp))))

coefH(QD1s_pflegebez_2)[c("Hi", "H")]
coefH(QD2s_posaf_2)[c("Hi", "H")]
coefH(QD3s_negaf_2)[c("Hi", "H")]
coefH(QD4s_ruhelos_2)[c("Hi", "H")]
coefH(QD6s_bez_2)[c("Hi", "H")]
coefH(QD7s_iso_2)[c("Hi", "H")]

check.reliability(QD1s_pflegebez_2)["MS"][[1]] %>% round(2)
check.reliability(QD2s_posaf_2)["MS"][[1]] %>% round(2)
check.reliability(QD3s_negaf_2)["MS"][[1]] %>% round(2)
check.reliability(QD4s_ruhelos_2)["MS"][[1]] %>% round(2)
check.reliability(QD6s_bez_2)["MS"][[1]] %>% round(2)
check.reliability(QD7s_iso_2)["MS"][[1]] %>% round(2)

summary(check.monotonicity(QD1s_pflegebez_2))
summary(check.monotonicity(QD2s_posaf_2))
summary(check.monotonicity(QD3s_negaf_2))
summary(check.monotonicity(QD4s_ruhelos_2))
summary(check.monotonicity(QD6s_bez_2))
summary(check.monotonicity(QD7s_iso_2))

cronbachs_alpha(QD1s_pflegebez_2) %>% round(2)
cronbachs_alpha(QD2s_posaf_2) %>% round(2)
cronbachs_alpha(QD3s_negaf_2) %>% round(2)
cronbachs_alpha(QD4s_ruhelos_2) %>% round(2)
cronbachs_alpha(QD6s_bez_2) %>% round(2)
cronbachs_alpha(QD7s_iso_2) %>% round(2)
