library(easystats)
library(ggplot2)
library(patchwork)

options(modelbased_join_dots = FALSE)

d <- data_read("osfio_data.rds", convert_factors = FALSE)


# Recodings ----------------------------------------------------------
# --------------------------------------------------------------------

d$mean_uke3_trust2 <- row_means(
  d,
  select = c("uke2_1_neu", "uke2_2_neu", "uke2_3_neu"),
  min_valid = 1
)
d$mean_uke3_collectiver2 <- row_means(
  d,
  select = c("recoded_uke2_4_neu", "uke2_5_neu", "recoded_uke2_6_neu", "uke2_7_neu"),
  min_valid = 1
)

d$age_group3 <- recode_values(d$age_group, recode = list(`1` = c(1, 2), `2` = c(3, 4), `3` = c(5, 6)))

d <- to_factor(d, c("Parties", "q1", "educ_rec", "age_group", "age_group3"))
d <- convert_to_na(d, na = c(777, 998, 999), verbose = FALSE)

levels(d$age_group3) <- c("18-39", "40-59", "60+")
levels(d$Parties) <- c("SPD", "CDU", "Greens", "AfD", "FDP", "Left")


# Table 1 ------------------------------------------------------------
# --------------------------------------------------------------------

report_sample(
  data_select(
    d,
    c(
      "mean_uke3_trust2", "mean_uke3_collectiver2", "recoded_uke2_8_neu",
      "uke2_9_neu", "q1", "age_group3", "educ_rec", "Parties", "weight"
    )
  ),
  by = "Parties",
  weights = "weight"
) |> print_html()

report_sample(
  data_select(
    d,
    c(
      "uke2_1_neu", "uke2_2_neu", "uke2_3_neu", "recoded_uke2_4_neu",
      "uke2_5_neu", "recoded_uke2_6_neu", "uke2_7_neu", "recoded_uke2_8_neu",
      "uke2_9_neu", "Parties", "weight"
    )
  ),
  by = "Parties",
  weights = "weight"
) |> print_html()

means_by_group(
  d,
  c("uke2_1_neu", "uke2_2_neu", "uke2_3_neu", "recoded_uke2_4_neu",
    "uke2_5_neu", "recoded_uke2_6_neu", "uke2_7_neu", "recoded_uke2_8_neu",
    "uke2_9_neu"
  ),
  by = "Parties",
  weights = "weight"
)

# Regression models --------------------------------------------
# --------------------------------------------------------------

m1 <- lm(mean_uke3_trust2 ~ Parties + q1 + educ_rec + age_group, data = d, weights = weight)
m2 <- lm(mean_uke3_collectiver2 ~ Parties + q1 + educ_rec + age_group, data = d, weights = weight)
m3 <- lm(recoded_uke2_8_neu ~ Parties + q1 + educ_rec + age_group, data = d, weights = weight)
m4 <- lm(uke2_9_neu ~ Parties + q1 + educ_rec + age_group, data = d, weights = weight)

# check_model(m1)
# model_parameters(m)


# Estimated Marginal Means--------------------------------------------
# --------------------------------------------------------------------

emm1 <- estimate_means(m1, "Parties", vcov = "HC3")
emm1$Parties <- factor(emm1$Parties, levels = emm1$Parties[order(emm1$Mean)])

emm2 <- estimate_means(m2, "Parties", vcov = "HC3")
emm2$Parties <- factor(emm2$Parties, levels = emm2$Parties[order(emm2$Mean)])

emm3 <- estimate_means(m3, "Parties", vcov = "HC3")
emm3$Parties <- factor(emm3$Parties, levels = emm3$Parties[order(emm3$Mean)])

emm4 <- estimate_means(m4, "Parties", vcov = "HC3")
emm4$Parties <- factor(emm4$Parties, levels = emm4$Parties[order(emm4$Mean)])


# Figure 1 -----------------------------------------------------------
# --------------------------------------------------------------------

party_colors <- okabeito_colors()[c(2, 1, 6, 7, 8, 3)]
names(party_colors) <- levels(emm1$Parties)

p1 <- ggplot(emm1, aes(x = Mean, y = Parties, xmin = CI_low, xmax = CI_high, color = Parties)) +
  geom_point(size = 2.5) +
  geom_errorbarh(height = 0, linewidth = 0.9) +
  theme_modern(show.ticks = TRUE, legend.position = "none") +
  labs(x = NULL, y = NULL, title = "Trust in vaccination") +
  scale_color_manual(values = party_colors) +
  xlim(c(1.3, 3.5))

p2 <- ggplot(emm2, aes(x = Mean, y = Parties, xmin = CI_low, xmax = CI_high, color = Parties)) +
  geom_point(size = 2.5) +
  geom_errorbarh(height = 0, linewidth = 0.9) +
  theme_modern(show.ticks = TRUE, legend.position = "none") +
  labs(x = NULL, y = NULL, title = "Collective responsibility") +
  scale_color_manual(values = party_colors) +
  xlim(c(1.3, 3.5))

p3 <- ggplot(emm3, aes(x = Mean, y = Parties, xmin = CI_low, xmax = CI_high, color = Parties)) +
  geom_point(size = 2.5) +
  geom_errorbarh(height = 0, linewidth = 0.9) +
  theme_modern(show.ticks = TRUE, legend.position = "none") +
  labs(x = NULL, y = NULL, title = "Trust in medical institutions") +
  scale_color_manual(values = party_colors) +
  xlim(c(1.3, 3.5))

p4 <- ggplot(emm4, aes(x = Mean, y = Parties, xmin = CI_low, xmax = CI_high, color = Parties)) +
  geom_point(size = 2.5) +
  geom_errorbarh(height = 0, linewidth = 0.9) +
  theme_modern(show.ticks = TRUE, legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Trust in government") +
  scale_color_manual(values = party_colors) +
  xlim(c(1.3, 3.5))

p <- (p1 + p2) / (p3 + p4)
ggsave(
  "figure1.tiff",
  plot = p,
  scale = 0.7,
  width = 12,
  height = 12,
  dpi = 600,
  compression = "lzw"
)

# numbers for figure 1
print_html(emm1)
print_html(emm2)
print_html(emm3)
print_html(emm4)


# Tables 2-5 -----------------------------------------------------------
# ----------------------------------------------------------------------

estimate_contrasts(m1, "Parties", vcov = "HC3") |> print_html(digits = 3)
estimate_contrasts(m2, "Parties", vcov = "HC3") |> print_html(digits = 3)
estimate_contrasts(m3, "Parties", vcov = "HC3") |> print_html(digits = 3)
estimate_contrasts(m4, "Parties", vcov = "HC3") |> print_html(digits = 3)
