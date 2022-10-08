library(parameters)
library(sjlabelled)
library(performance)
library(datawizard)
library(mokken)

pc <- principal_components(remove_all_labels(qualidem_pp))
pc

check_itemscale(pc)

item_reliability(QD1_pflegebez_pp)

# Bouman AIE, Ettema TP, Wetzels RB, et al. Evaluation of Qualidem: a
# dementia-specific quality of life instrument for persons with dementia in
# residential settings; scalability and reliability of subscales in four Dutch
# field surveys. International Journal of Geriatric Psychiatry 2011;26:711–22.
# doi:10.1002/gps.2585

coefH(as.data.frame(na.omit(QD1_pflegebez_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD2_posaf_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD3_negaf_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD4_ruhelos_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD5_selbst_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD6_bez_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD7_iso_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD8_home_pp)))[c("Hi", "H")]
coefH(as.data.frame(na.omit(QD9_sinn_pp)))[c("Hi", "H")]

check.reliability(as.data.frame(na.omit(QD1_pflegebez_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD2_posaf_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD3_negaf_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD4_ruhelos_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD5_selbst_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD6_bez_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD7_iso_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD8_home_pp)))["MS"][[1]] %>% round(2)
check.reliability(as.data.frame(na.omit(QD9_sinn_pp)))["MS"][[1]] %>% round(2)



sum(apply(qualidem_pp, c(1, 2), is.na)) / (nrow(qualidem_pp) * ncol(qualidem_pp))

nrow(as.data.frame(na.omit(QD1_pflegebez_pp)))
nrow(as.data.frame(na.omit(QD2_posaf_pp)))
nrow(as.data.frame(na.omit(QD3_negaf_pp)))
nrow(as.data.frame(na.omit(QD4_ruhelos_pp)))
nrow(as.data.frame(na.omit(QD5_selbst_pp)))
nrow(as.data.frame(na.omit(QD6_bez_pp)))
nrow(as.data.frame(na.omit(QD7_iso_pp)))
nrow(as.data.frame(na.omit(QD8_home_pp)))
nrow(as.data.frame(na.omit(QD9_sinn_pp)))
