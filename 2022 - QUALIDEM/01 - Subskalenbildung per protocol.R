library(dplyr)
library(datawizard)

load("david_final.RData")

dementia <- which(david$mmse_score >= 10)
severe_dementia <- which(david$mmse_score < 10)

d_pp <- david[dementia, ]
ds_pp <- david[severe_dementia, ]

#Erste Subskala: Pflegebeziehung
QD1_pflegebez_pp <- d_pp %>% dplyr::select(QAbw, QAerger, QKonfl, QSchuld, QWertsch, QAnnHilf, QRout)
QD1s_pflegebez_pp <- ds_pp %>% dplyr::select(QAerger, QKonfl, QWertsch)

# Zweite Subskala: Positiver Affekt
QD2_posaf_pp <- d_pp %>% dplyr::select(QFroeh, QZufri, QGenuss, QLaun, QLach, QPosStimm)
QD2s_posaf_pp <- ds_pp %>% dplyr::select(QZufri, QGenuss, QLaun, QPosStimm)

# Dritte Subskala: Negativer Affekt
QD3_negaf_pp <- d_pp %>% dplyr::select(QAngst, QTraur, QWeint)
QD3s_negaf_pp <- ds_pp %>% dplyr::select(QAngst, QWeint)

# Vierte Subskala: Ruheloses Verhalten
QD4_ruhelos_pp <- d_pp %>% dplyr::select(QBew, QRuhlos, QAngesp)
QD4s_ruhelos_pp <- ds_pp %>% dplyr::select(QBew, QRuhlos, QAngesp)

# Fünfte Subskala: Positives Selbstbild
QD5_selbst_pp <- d_pp %>% dplyr::select(QMehrHil, QKoenn, QWertlos)

# Sechste Subskala: Soziale Beziehungen, Kurzversion für schwer Demente
QD6_bez_pp <- d_pp %>% dplyr::select(QKont, QPosKon, QSorgt, QSchott, QFreund, QUnwohl)
QD6s_bez_pp <- ds_pp %>% dplyr::select(QKont, QPosKon, QSchott)

# Siebte Subskala: Soziale Isolation
QD7_iso_pp <- d_pp %>% dplyr::select(QAbwPat, QNegKon, QRuft)
QD7s_iso_pp <- ds_pp %>% dplyr::select(QAbwPat, QNegKon, QRuft)

# Achte Subskala: Sich zu Hause fühlen
QD8_home_pp <- d_pp %>% dplyr::select(QLangw, QSchloss, QHeimisch, QVerlass)

# Neunte Subskala: Etwas zu tun haben
QD9_sinn_pp <- d_pp %>% dplyr::select(QBeschaeft, QGernArb)

qualidem_pp <- bind_cols(
  QD1_pflegebez_pp,
  QD2_posaf_pp,
  QD3_negaf_pp,
  QD4_ruhelos_pp,
  QD5_selbst_pp,
  QD6_bez_pp,
  QD7_iso_pp,
  QD8_home_pp,
  QD9_sinn_pp
)

prefix <- c(
  rep("QD1_", ncol(QD1_pflegebez_pp)),
  rep("QD2_", ncol(QD2_posaf_pp)),
  rep("QD3_", ncol(QD3_negaf_pp)),
  rep("QD4_", ncol(QD4_ruhelos_pp)),
  rep("QD5_", ncol(QD5_selbst_pp)),
  rep("QD6_", ncol(QD6_bez_pp)),
  rep("QD7_", ncol(QD7_iso_pp)),
  rep("QD8_", ncol(QD8_home_pp)),
  rep("QD9_", ncol(QD9_sinn_pp))
)
colnames(qualidem_pp) <- sprintf("%s%s", prefix, colnames(qualidem_pp))

qualidem_s_pp <- bind_cols(
  QD1s_pflegebez_pp,
  QD2s_posaf_pp,
  QD3s_negaf_pp,
  QD4s_ruhelos_pp,
  QD6s_bez_pp,
  QD7s_iso_pp,
)

prefix <- c(
  rep("QD1s_", ncol(QD1s_pflegebez_pp)),
  rep("QD2s_", ncol(QD2s_posaf_pp)),
  rep("QD3s_", ncol(QD3s_negaf_pp)),
  rep("QD4s_", ncol(QD4s_ruhelos_pp)),
  rep("QD6s_", ncol(QD6s_bez_pp)),
  rep("QD7s_", ncol(QD7s_iso_pp))
)
colnames(qualidem_s_pp) <- sprintf("%s%s", prefix, colnames(qualidem_s_pp))


gruppen_pp <- c(
  rep(1, ncol(QD1_pflegebez_pp)),
  rep(2, ncol(QD2_posaf_pp)),
  rep(3, ncol(QD3_negaf_pp)),
  rep(4, ncol(QD4_ruhelos_pp)),
  rep(5, ncol(QD5_selbst_pp)),
  rep(6, ncol(QD6_bez_pp)),
  rep(7, ncol(QD7_iso_pp)),
  rep(8, ncol(QD8_home_pp))
  # rep(9, ncol(QD9_sinn_pp))
)


gruppen_s_pp <- c(
  rep(1, ncol(QD1s_pflegebez_pp)),
  rep(2, ncol(QD2s_posaf_pp)),
  rep(3, ncol(QD3s_negaf_pp)),
  rep(4, ncol(QD4s_ruhelos_pp)),
  rep(6, ncol(QD6s_bez_pp)),
  rep(7, ncol(QD7s_iso_pp))
)
