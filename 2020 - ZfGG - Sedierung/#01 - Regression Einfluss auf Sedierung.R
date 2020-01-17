suppressMessages(suppressWarnings({
  library(ggplot2)      # Abbildungen
  library(sjPlot)       # Abbildungen / Tabellen
  library(ggeffects)    # Abbildungen Estimated Marginal Means
  library(see)          # Abbildungen manipulieren
  library(performance)  # Modelldiagnostik
  library(emmeans)      # Multiples Testen
}))


# Daten laden ----

load("david.RData")


# Deskriptiv: Variiert Outcome 1 in Abhängigkeit der Hauptdiagnose? ----

ggplot2::theme_set(
  theme_lucid(
    axis.title.size = 24,
    axis.text.size = 20,
    legend.text.size = 20,
    legend.position = "bottom",
    axis.title.space = 32
  )
)

sjp.xtab(
  david$Hauptdiagnose,
  david$bm_allg_sedierung,
  show.values = F,
  show.total = F,
  ylim = c(0, .4),
  grid.breaks = .05,
  legend.labels = c("Nein", "Ja"),
  geom.colors = c("#27ae60", "#c0392b")
) + label_angle(90) +
  labs(fill = NULL, x = NULL)

ggsave("Figure S1.tiff", width = 6, height = 4, units = "in", dpi = 300, compression = "lzw", scale = 2)


# Modell 1: Sedierung ohne Interaktion ----

mf1 <-
  formula(
    bm_allg_sedierung ~ sex + alter_z + dementia_mmse + cci_z +
      barthtot_a_z + Dauer_z + pas_score_z + kkh_r
  )


# Modell 3: Sedierung mit Interaktion ----

mf3 <-
  formula(
    bm_allg_sedierung ~ sex + alter_z + dementia_mmse + cci_z +
      barthtot_a_z + Dauer_z + pas_score_z * kkh_r
  )

m1 <- glm(mf1, family = binomial(), data = david)
m3 <- glm(mf3, family = binomial(), data = david)


# Modelldiagnostik Modelle 1 und 2 ----

check_collinearity(m1)
binned_residuals(m1)
performance_pcp(m1)

check_collinearity(m3)
binned_residuals(m3)
performance_pcp(m3)


# Odds Ratio und Marginal Effects ----

tab_model(m1, m3, prefix.labels = "v")

ggemmeans(m1, "pas_score_z") %>% plot()
ggemmeans(m1, "kkh_r") %>% plot()

ggemmeans(m3, c("pas_score_z", "kkh_r")) %>%
  plot() +
  labs(colour = NULL, y = NULL, title = NULL) +
  scale_x_continuous(breaks = c(-1, .25, 1.5, 2.75, 4), labels = c("niedrig", "4", "8", "12", "hoch")) +
  theme_lucid(
    axis.title.size = 24,
    axis.text.size = 20,
    legend.text.size = 20,
    legend.position = "bottom",
    axis.title.space = 32
  ) +
  theme(
    legend.spacing.x = unit(.5, "cm")
  )

ggsave("Figure 1.tiff", width = 6, height = 4, units = "in", dpi = 300, compression = "lzw", scale = 2)

emmeans(m3, c("pas_score_z", "kkh_r"), at = list(pas_score_z = c(-1, 4))) %>%
  contrast(method = "pairwise")
