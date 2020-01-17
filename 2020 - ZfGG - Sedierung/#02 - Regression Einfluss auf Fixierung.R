suppressMessages(suppressWarnings({
  library(ggplot2)      # Abbildungen
  library(sjPlot)       # Abbildungen / Tabellen
  library(ggeffects)    # Abbildungen Estimated Marginal Means
  library(see)          # Abbildungen manipulieren
  library(performance)  # Modelldiagnostik
  library(emmeans)      # Multiples Testen
}))


# Daten laden  ----

load("david.RData")


# Variiert Outcome 2 in Abhängigkeit der Hauptdiagnose? ----

ggplot2::theme_set(theme_sjplot2())
sjp.xtab(
  david$Hauptdiagnose,
  david$Fixierung_d,
  show.values = F,
  show.total = F,
  ylim = c(0, .4),
  grid.breaks = .05
) + label_angle(90)


# Modell 2: Fixierung ohne Interaktion ----

mf2 <-
  formula(
    Fixierung_d ~ sex + alter_z + dementia_mmse + cci_z +
      barthtot_a_z + Dauer_z + pas_score_z + kkh_r
  )


# Modell 4: Fixierung mit Interaktion ----

mf4 <-
  formula(
    Fixierung_d ~ sex + alter_z + dementia_mmse + cci_z +
      barthtot_a_z * kkh_r + Dauer_z * kkh_r + pas_score_z * kkh_r
  )

m2 <- glm(mf2, family = binomial(), data = david)
m4 <- glm(mf4, family = binomial(), data = david)


# Modelldiagnostik Modelle 3 und 4 ----

check_collinearity(m2)
binned_residuals(m2)
performance_pcp(m2)

check_collinearity(m4)
binned_residuals(m4)
performance_pcp(m4)


# Odds Ratio und Marginal Effects ----

tab_model(m2, m4, prefix.labels = "v")

axis_title_size <- 22
axis_text_size <- 18
legend_text_size <- 18
axis_title_space <- 28


p1 <- ggemmeans(m4, c("Dauer_z", "kkh_r")) %>%
  plot() +
  labs(colour = NULL, y = NULL, title = NULL, x = "Aufenthaltsdauer") +
  scale_x_continuous(
    breaks = c(-1.3, -.25, .8, 1.85, 2.9),
    labels = c("1 Tag", "7 Tage", "14 Tage", "21 Tage", "28 Tage")
  ) +
  theme_lucid(
    axis.title.size = axis_title_size,
    axis.text.size = axis_text_size,
    legend.text.size = legend_text_size,
    legend.position = "bottom",
    axis.title.space = axis_title_space
  ) +
  theme(
    legend.spacing.x = unit(.5, "cm")
  )

ggsave("Figure 2a.tiff", plot = p1, width = 6, height = 4, units = "in", dpi = 300, compression = "lzw", scale = 2)


p2 <- ggemmeans(m4, c("barthtot_a_z", "kkh_r")) %>%
  plot() +
  labs(colour = NULL, y = NULL, title = NULL, x = "Barthel-Index") +
  scale_x_continuous(
    breaks = c(-1.1, -.44, .22, .88, 1.54, 2.2),
    labels = c("niedrig", "20", "40", "60", "80", "hoch")
  ) +
  theme_lucid(
    axis.title.size = axis_title_size,
    axis.text.size = axis_text_size,
    legend.text.size = legend_text_size,
    legend.position = "bottom",
    axis.title.space = axis_title_space
  ) +
  theme(
    legend.spacing.x = unit(.5, "cm")
  )

ggsave("Figure 2b.tiff", plot = p2, width = 6, height = 4, units = "in", dpi = 300, compression = "lzw", scale = 2)

p3 <- ggemmeans(m4, c("pas_score_z", "kkh_r")) %>%
  plot() +
  labs(colour = NULL, y = NULL, title = NULL) +
  scale_x_continuous(breaks = c(-1, .25, 1.5, 2.75, 4), labels = c("niedrig", "4", "8", "12", "hoch")) +
  theme_lucid(
    axis.title.size = axis_title_size,
    axis.text.size = axis_text_size,
    legend.text.size = legend_text_size,
    legend.position = "bottom",
    axis.title.space = axis_title_space
  ) +
  theme(
    legend.spacing.x = unit(.5, "cm")
  )

ggsave("Figure 2c.tiff", plot = p3, width = 6, height = 4, units = "in", dpi = 300, compression = "lzw", scale = 2)


emmeans(m4, c("pas_score_z", "kkh_r"), at = list(pas_score_z = c(-1, 0, 4))) %>%
  contrast(method = "pairwise")

emmeans(m4, c("barthtot_a_z", "kkh_r"), at = list(barthtot_a_z = c(-1, 0, 2))) %>%
  contrast(method = "pairwise")

emmeans(m4, c("Dauer_z", "kkh_r"), at = list(Dauer_z = c(-1, 0, 1))) %>%
  contrast(method = "pairwise")
