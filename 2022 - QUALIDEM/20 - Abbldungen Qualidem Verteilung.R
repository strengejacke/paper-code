library(tidyr)
library(ggplot2)
library(datawizard)

load("david_recodes.RData")

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




qualidem_9 <- data.frame(
  QD1 = rowSums(QD1_pflegebez),
  QD2 = rowSums(QD2_posaf),
  QD3 = rowSums(QD3_negaf),
  QD4 = rowSums(QD4_ruhelos),
  QD5 = rowSums(QD5_selbst),
  QD6 = rowSums(QD6_bez),
  QD7 = rowSums(QD7_iso),
  QD8 = rowSums(QD8_home),
  QD9 = rowSums(QD9_sinn)
)

p <- ggplot(
  pivot_longer(normalize(qualidem_9), seq_len(ncol(qualidem_9)), names_to = "Subscale", values_to = "Score"),
  aes(x = Subscale, y = Score, group = Subscale)
) +
  geom_boxplot(fill = "grey90", width = .5) +
  ggeffects::theme_ggeffects(base_size = 14) +
  scale_x_discrete(labels = c("care relationship", "positive affect", "negative affect",
                              "restless behaviour", "positive self-image", "social relations",
                              "social isolation", "feeling at home", "have something to do")) +
  theme(
    axis.text.x = element_text(angle = 90, color = "grey10", size = 14),
    axis.line.x = element_line(colour = "grey30"),
    axis.line.y = element_line(colour = "grey30")
  ) +
  labs(x = NULL, y = "QUALIDEM Score (normalized)")

ggsave("qualidem_9.tiff", p, width = 10, height = 6, compression = "lzw", dpi = 600)





qualidem_6 <- data.frame(
  QD1 = rowSums(QD1s_pflegebez),
  QD2 = rowSums(QD2s_posaf),
  QD3 = rowSums(QD3s_negaf),
  QD4 = rowSums(QD4s_ruhelos),
  QD6 = rowSums(QD6s_bez),
  QD7 = rowSums(QD7s_iso)
)

p <- ggplot(
  pivot_longer(normalize(qualidem_6), seq_len(ncol(qualidem_6)), names_to = "Subscale", values_to = "Score"),
  aes(x = Subscale, y = Score, group = Subscale)
) +
  geom_boxplot(fill = "grey90", width = .5) +
  ggeffects::theme_ggeffects(base_size = 14) +
  scale_x_discrete(labels = c("care relationship", "positive affect", "negative affect",
                              "restless behaviour", "social relations", "social isolation")) +
  theme(
    axis.text.x = element_text(angle = 90, color = "grey10", size = 14),
    axis.line.x = element_line(colour = "grey30"),
    axis.line.y = element_line(colour = "grey30")
  ) +
  labs(x = NULL, y = "QUALIDEM Score (normalized)")

ggsave("qualidem_6.tiff", p, width = 6.8, height = 6, compression = "lzw", dpi = 600)
