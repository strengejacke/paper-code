# hier die skalenkonsistenz mit original imputierten Daten

library(easystats)
library(strengejacke)
library(tidyverse)
library(mokken)

load("~/IMS-Server/Projekte/DAVID/Datenauswertung/david_studientauglich.RData")

dementia <- which(david$mmse_score >= 10)
severe_dementia <- which(david$mmse_score < 10)

d <- david[dementia, ]
ds <- david[severe_dementia, ]


QD1_pflegebez <- d %>% dplyr::select(QAbw, QAerger, QKonfl, QSchuld, QWertsch, QAnnHilf, QRout)
QD1s_pflegebez <- ds %>% dplyr::select(QAerger, QKonfl, QWertsch)

# Zweite Subskala: Positiver Affekt
QD2_posaf <- d %>% dplyr::select(QFroeh, QZufri, QGenuss, QLaun, QLach, QPosStimm)
QD2s_posaf <- ds %>% dplyr::select(QZufri, QGenuss, QLaun, QPosStimm)

# Dritte Subskala: Negativer Affekt
QD3_negaf <- d %>% dplyr::select(QAngst, QTraur, QWeint)
QD3s_negaf <- ds %>% dplyr::select(QAngst, QWeint)

# Vierte Subskala: Ruheloses Verhalten
QD4_ruhelos <- d %>% dplyr::select(QBew, QRuhlos, QAngesp)
QD4s_ruhelos <- ds %>% dplyr::select(QBew, QRuhlos, QAngesp)

# Fünfte Subskala: Positives Selbstbild
QD5_selbst <- d %>% dplyr::select(QMehrHil, QKoenn, QWertlos)

# Sechste Subskala: Soziale Beziehungen, Kurzversion für schwer Demente
QD6_bez <- d %>% dplyr::select(QKont, QPosKon, QSorgt, QSchott, QFreund, QUnwohl)
QD6s_bez <- ds %>% dplyr::select(QKont, QPosKon, QSchott)

# Siebte Subskala: Soziale Isolation
QD7_iso <- d %>% dplyr::select(QAbwPat, QNegKon, QRuft)
QD7s_iso <- ds %>% dplyr::select(QAbwPat, QNegKon, QRuft)

# Achte Subskala: Sich zu Hause fühlen
QD8_home <- d %>% dplyr::select(QLangw, QSchloss, QHeimisch, QVerlass)

# Neunte Subskala: Etwas zu tun haben
QD9_sinn <- d %>% dplyr::select(QBeschaeft, QGernArb)

# leichte bis schwere demenz

coefH(QD1_pflegebez)[c("Hi", "H")]
coefH(QD2_posaf)[c("Hi", "H")]
coefH(QD3_negaf)[c("Hi", "H")]
coefH(QD4_ruhelos)[c("Hi", "H")]
coefH(QD5_selbst)[c("Hi", "H")]
coefH(QD6_bez)[c("Hi", "H")]
coefH(QD7_iso)[c("Hi", "H")]
coefH(QD8_home)[c("Hi", "H")]
coefH(QD9_sinn)[c("Hi", "H")]

check.reliability(QD1_pflegebez)["MS"][[1]] %>% round(2)
check.reliability(QD2_posaf)["MS"][[1]] %>% round(2)
check.reliability(QD3_negaf)["MS"][[1]] %>% round(2)
check.reliability(QD4_ruhelos)["MS"][[1]] %>% round(2)
check.reliability(QD5_selbst)["MS"][[1]] %>% round(2)
check.reliability(QD6_bez)["MS"][[1]] %>% round(2)
check.reliability(QD7_iso)["MS"][[1]] %>% round(2)
check.reliability(QD8_home)["MS"][[1]] %>% round(2)
check.reliability(QD9_sinn)["MS"][[1]] %>% round(2)

cronbachs_alpha(QD1_pflegebez) %>% round(2)
cronbachs_alpha(QD2_posaf) %>% round(2)
cronbachs_alpha(QD3_negaf) %>% round(2)
cronbachs_alpha(QD4_ruhelos) %>% round(2)
cronbachs_alpha(QD5_selbst) %>% round(2)
cronbachs_alpha(QD6_bez) %>% round(2)
cronbachs_alpha(QD7_iso) %>% round(2)
cronbachs_alpha(QD8_home) %>% round(2)
cronbachs_alpha(QD9_sinn) %>% round(2)


# sehr schwere demenz

coefH(QD1s_pflegebez)[c("Hi", "H")]
coefH(QD2s_posaf)[c("Hi", "H")]
coefH(QD3s_negaf)[c("Hi", "H")]
coefH(QD4s_ruhelos)[c("Hi", "H")]
coefH(QD6s_bez)[c("Hi", "H")]
coefH(QD7s_iso)[c("Hi", "H")]

check.reliability(QD1s_pflegebez)["MS"][[1]] %>% round(2)
check.reliability(QD2s_posaf)["MS"][[1]] %>% round(2)
check.reliability(QD3s_negaf)["MS"][[1]] %>% round(2)
check.reliability(QD4s_ruhelos)["MS"][[1]] %>% round(2)
check.reliability(QD6s_bez)["MS"][[1]] %>% round(2)
check.reliability(QD7s_iso)["MS"][[1]] %>% round(2)

cronbachs_alpha(QD1s_pflegebez) %>% round(2)
cronbachs_alpha(QD2s_posaf) %>% round(2)
cronbachs_alpha(QD3s_negaf) %>% round(2)
cronbachs_alpha(QD4s_ruhelos) %>% round(2)
cronbachs_alpha(QD6s_bez) %>% round(2)
cronbachs_alpha(QD7s_iso) %>% round(2)

