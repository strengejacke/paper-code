# Deskriptive Tabelle

# Abhängige: Depression (PHQ-9)
library(easystats)
library(ggeffects)
library(glmmTMB)
library(ggplot2)

load("nako.RData")

nako <- nako |>
  data_rename(
    pattern = c(
      "a_sn_sni", "d_sn7", "d_sn8", "a_ses_househ", "a_ses_famst",
      "basis_age", "basis_sex", "a_ses_incpos",
      "a_ses_isced97_cat", "mig_status3", "a_emo_phq9_sum",
      "a_emo_phq9_cut10", "a_ses_bedarfgw",
      "a_ses_beruf", "a_ses_ewstat", "aussiedler_2"
    ),
    replacement = c(
      "sni", "inf_supp", "emo_supp", "hh_size", "fam_status",
      "age", "gender", "income_pos", "education", "migstatus",
      "phq9_score", "phq9_dicho", "ses_weight", "jobtype", "employment",
      "resettler"
    )
  )

nako$education <- assign_labels(
  nako$education,
  values = c("low education", "intermediate education", "high education")
)
nako$education <- labels_to_levels(nako$education)
nako$education <- relevel(nako$education, ref = "high education")
nako$age3_r <- relevel(nako$age3, ref = "60+")
nako$age3_r <- nako$age3
levels(nako$resettler) <- c("non-migrant", "1st generation migrant", "1st generation naturalized", "2nd generation", "German resettler")

# SNI umdrehen
nako$sni_r <- reverse_scale(nako$sni_dicho)
data_tabulate(nako, "sni_r", by = "sni_dicho")
data_tabulate(nako, c("sni_r", "sni_dicho"))

data_tabulate(nako, select = c("phq9_dicho", "sni_r", "age3", "intersec3a"))

# gucken, wie sich Variablen verteilen, wenn missings entfernt werden
d <- na.omit(data_select(nako, select = c("phq9_dicho", "sni", "age3", "intersec3a")))
data_tabulate(d)

# gucken, wie sich Variablen verteilen, wenn missings entfernt werden
d <- na.omit(data_select(nako, select = c("phq9_dicho", "age3", "intersec3a")))
data_tabulate(d)

# Intersektionalität ala Fisk et al. https://doi.org/10.1016/j.ssmph.2018.03.005
# AV: PHQ-9 (depressive Symptomatik nein/ja)

# "quantify to what degree the different dimensions used to construct the
# intersectional strata contributed to the between stratum variance seen in
# the previous model." (i.e. to model 1)

m1 <- glmmTMB(phq9_dicho ~ age3_r + (1 | resettler:gender:education), data = nako, family = binomial())
model_parameters(m1, exponentiate = TRUE)
icc(m1)

# identisch mit
# m1 <- glmmTMB(phq9_dicho ~ age3_r + (1 | intersec3a), data = nako, family = binomial())

m2a <- glmmTMB(phq9_dicho ~ age3_r + resettler + (1 | resettler:gender:education), data = nako, family = binomial())
m2b <- glmmTMB(phq9_dicho ~ age3_r + gender + (1 | resettler:gender:education), data = nako, family = binomial())
m2c <- glmmTMB(phq9_dicho ~ age3_r + education + (1 | resettler:gender:education), data = nako, family = binomial())

# m2a <- glmmTMB(phq9_dicho ~ age3_r + resettler + (1 | intersec3a), data = nako, family = binomial())
# m2b <- glmmTMB(phq9_dicho ~ age3_r + gender + (1 | intersec3a), data = nako, family = binomial())
# m2c <- glmmTMB(phq9_dicho ~ age3_r + education + (1 | intersec3a), data = nako, family = binomial())

m3 <- glmmTMB(phq9_dicho ~ age3_r + resettler + gender + education + (1 | resettler:gender:education), data = nako, family = binomial())
# m3 <- glmmTMB(phq9_dicho ~ age3_r + resettler + gender + education + (1 | intersec3a), data = nako, family = binomial())

# Odds Ratios deuten darauf hin, dass "gender" und "resettler" ähnliche
# Zusmamenhänge zu PHQ-9 (Depression) aufweisen, "(low) education" aber
# den stärksten Effekt aufweist

## TODO: Referenzkategorien anpassen, sodass alle in eine Richtung zeigen
compare_parameters(m1, m2a, m2b, m2c, m3, exponentiate = TRUE) |> print_html()

icc(m1)
icc(m2a)
icc(m2b)
icc(m2c)
icc(m3)

# icc(m1, ci = 0.95, ci_method = "boot", iterations = 50)
# icc(m2a, ci = 0.95, ci_method = "boot", iterations = 50)
# icc(m2b, ci = 0.95, ci_method = "boot", iterations = 50)
# icc(m2c, ci = 0.95, ci_method = "boot", iterations = 50)
# icc(m3, ci = 0.95, ci_method = "boot", iterations = 50)


# PVC - Proportional Change in the between-stratum Variance
v1 <- get_variance(m1)
v2a <- get_variance(m2a)
v2b <- get_variance(m2b)
v2c <- get_variance(m2c)
v3 <- get_variance(m3)

# PCV Modell 1 zu Modell 2a
(v1$var.random - v2a$var.random) / v1$var.random

# PCV Modell 1 zu Modell 2b
(v1$var.random - v2b$var.random) / v1$var.random

# PCV Modell 1 zu Modell 2c
(v1$var.random - v2c$var.random) / v1$var.random

# PCV Modell 1 zu Modell full
(v1$var.random - v3$var.random) / v1$var.random

# Oben zeigt sich auch, dass "Bildung" das Intersektionalitätsmerkmal ist,
# das die meisten Ungleichheiten erklärt, da hier der PCV am größten ist


# Varianz zwischen den Intersektionalitätsgruppen

# Variante 1
m1p <- glmmTMB(phq9_dicho ~ age3_r + (1 | resettler:gender:education), data = nako, family = binomial())
pr <- predict_response(
  m1p,
  c("resettler", "gender", "education"),
  type = "random",
  interval = "confidence"
)
pr$facet <- factor(pr$facet, levels = c("low", "medium", "high"))

p1 <- plot(pr) +
  facet_wrap(~facet, axis = "all_x") +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(color = "black")
  )
p1
ggsave("varianz_strata1.png", plot = p1, width = 11, height = 5, scale = 0.85, dpi = 600)


# Variante 2 - im Artikel!
m1p <- glmmTMB(phq9_dicho ~ age3_r + (1 | intersec3a), data = nako, family = binomial())

# Finale predictions für das Modell
pr <- predict_response(
  m1p,
  "intersec3a",
  type = "random",
  interval = "confidence"
)
# pr <- predict_response(m1p, "intersec3a", margin = "empirical")

pr$x_char <- as.character(pr$x)
pr$group <- as.factor(recode_into(
  startsWith(x_char, "low education") ~ "low",
  startsWith(x_char, "intermediate education") ~ "intermediate",
  default = "high",
  data = pr
))
pr$group <- factor(pr$group, levels = c("low", "intermediate", "high"))

# Label säubern
axis_labels <- gsub(
  "(high education|intermediate education|low education), (.*)",
  "\\2",
  levels(pr$x)
)
axis_labels <- Hmisc::capitalize(axis_labels)

p2 <- pr |>
  data_arrange("predicted") |>
  ggplot(aes(
    x = forcats::fct_reorder(x, predicted, .desc = TRUE),
    y = predicted,
    ymin = conf.low,
    ymax = conf.high,
    color = group
  )) +
  geom_pointrange(fatten = 2) +
  coord_flip() +
  scale_y_log10(
    limits = c(0.045, 0.4),
    labels = scales::percent,
    breaks = c(0.05, 0.075, 0.1, 0.15, 0.2, 0.3, 0.4)
  ) +
  scale_x_discrete(labels = axis_labels[order(pr$predicted, decreasing = TRUE)]) +
  labs(x = NULL, y = NULL, color = "Education") +
  theme_ggeffects() +
  see::scale_color_material() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(color = "black")
  )

p2
ggsave("varianz_strata2_neu.png", plot = p2, width = 11, height = 7.5, scale = 0.7, dpi = 600)


# Pairwise Comparisons

m1p <- glmmTMB(phq9_dicho ~ age3_r + (1 | resettler:gender:education), data = nako, family = binomial())
pr_compare1 <- predict_response(
  m1p,
  c("resettler", "gender", "education"),
  type = "random",
  interval = "confidence"
)
pairwise_comp <- test_predictions(pr_compare1, by = "education")

# high education

out <- pairwise_comp |>
  format(combine_levels = TRUE) |>
  data_filter(education == "high education" & (`Pair 1` == "German resettler-female" | `Pair 2` == "German resettler-female")) |>
  data_select(exclude = "education")
sw <- which(out$`Pair 1` == "German resettler-female")
if (length(sw) > 0) {
  out$`Pair 1`[sw] <- out$`Pair 2`[sw]
  out$Contrast[sw] <- paste0("-", out$Contrast[sw])
}
out |>
  data_select(exclude = "Pair 2") |>
  data_arrange("-Contrast") |>
  print_html()

# intermediate education

out <- pairwise_comp |>
  format(combine_levels = TRUE) |>
  data_filter(education == "intermediate education" & (`Pair 1` == "1st generation migrant-female" | `Pair 2` == "1st generation migrant-female")) |>
  data_select(exclude = "education")
sw <- which(out$`Pair 1` == "1st generation migrant-female")
if (length(sw) > 0) {
  out$`Pair 1`[sw] <- out$`Pair 2`[sw]
  out$Contrast[sw] <- paste0("-", out$Contrast[sw])
}
out |>
  data_select(exclude = "Pair 2") |>
  data_arrange("-Contrast") |>
  print_html()

# low education

out <- pairwise_comp |>
  format(combine_levels = TRUE) |>
  data_filter(education == "low education" & (`Pair 1` == "2nd generation-female" | `Pair 2` == "2nd generation-female")) |>
  data_select(exclude = "education")
sw <- which(out$`Pair 1` == "2nd generation-female")
if (length(sw) > 0) {
  out$`Pair 1`[sw] <- out$`Pair 2`[sw]
  out$Contrast[sw] <- paste0("-", out$Contrast[sw])
}
out |>
  data_select(exclude = "Pair 2") |>
  data_arrange("-Contrast") |>
  print_html()
