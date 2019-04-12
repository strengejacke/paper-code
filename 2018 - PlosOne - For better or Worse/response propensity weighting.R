library(tidyverse)
library(strengejacke)
library(lme4)

# Mimic base data set w/o adjusted propensity weight

efc <- efc_final


# Copy dependent variables from follow-up dataset into main dataset ----

efc_tmp <- efc %>%
  select(cs_inresidential, cs_diffcarer, ID) %>%
  mutate(response = 1)

# efc_full$ID <- efc_full$eu_id
efc$c166reli_f <- to_factor(efc$c166reli)


# Merge base and follow-up data ----

efc_new <- efc_full %>%
  left_join(efc_tmp, by = "ID") %>%
  sjmisc::replace_na(response, value = 0) %>%
  std(c160age, pos_v_4, neg_c_7, c12hour) %>%
  mutate(c166reli_f = to_factor(c166reli))


# Response Propensity Weighting Modell ----

rpm <- glmer(
  response ~ c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r +
    behave_r + c12hour_z + neg_c_7_z + pos_v_4_z + e15_r + (1 | g2ctry),
  data = efc_new,
  family = binomial("logit"))


# Calculate Response Propensity-Score ----

propensity_score <- predict(rpm, type = "response")
efc_new$propensity_score <- NA
efc_new$propensity_score[as.numeric(names(propensity_score))] <- propensity_score

efc_new$response <- set_label(efc_new$response, label = "Response")
efc_new$response <- set_labels(efc_new$response, labels = c("Non-Respondents", "Respondents"))


# Calculate inverse probability weights and adj. inv. prob. weights ----

efc_new <- efc_new %>%
  mutate(
    inv_weight = (1 / propensity_score * response) + (1 / (1 - propensity_score) * (1 - response)),
    # alternative calculation, same results
    inv_weight2 = ifelse(response == 1, 1 / propensity_score, 1 / (1 - propensity_score)),
    response_label = as_label(response),
    adj_inv_weight = inv_weight / mean(inv_weight, na.rm = TRUE)
  )


# new dataset, only follow-up data ----

efc2 <- efc_new %>%
  filter(response == 1) %>%
  to_factor(e15_r, c172code, c161sex, behave_r) %>%
  mutate(adj_inv_weight = inv_weight / mean(inv_weight, na.rm = TRUE))  %>%
  scale_weights(g2ctry, inv_weight)


# final dataset for analyis ----

efc_final <- efc2 %>%
  select(
    c12hour, more_s, tot_need, e22h_r, f1care, f3reaa3, f3reasb, f22home_r,
    f3reaa1, f3reaa2, f3reaa4, c172code, demdiag, g2ctry, e15_r, c161sex,
    behave_r, c160age, e42dep, neg_c_7, pos_v_4, c12hour_d, more_s_r, tot_need_r,
    cs_inresidential, demdiag_r, e17age, e16sex, c160age_z, neg_c_7_z, pos_v_4_z,
    c12hour_z, cs_diffcarer, ID, c166reli, propensity_score, inv_weight,
    adj_inv_weight
  ) %>%
  bind_cols(select(efc, e42dep_d, neg_c_7_d, pos_v_4_d, e15_num)) %>%
  mutate(c166reli_f = to_factor(c166reli))


# Compare Weights of Respondents and Dropouts ----

pdf("S2 Fig.pdf", width = 9, height = 6)

ggplot(efc_new, aes(x = adj_inv_weight, fill = response_label)) +
  geom_density(alpha = 0.5, colour = "grey50") +
  geom_rug() +
  scale_x_log10(breaks = c(1, 5, 10, 20, 40)) +
  ggtitle("Distribution of adjusted inverse probability weights") +
  labs(y = NULL, fill = NULL, x = "Weight factor", subtitle = word_wrap("Comparison of the adjusted inverse probability weights for respondents (green) and non-respondents (red). Patterns indicate that our follow-up sample is rather a 'representative' subset of the baseline data.", wrap = 80, linesep = "\n")) +
  theme_sjplot2()

dev.off()


# Compare weighted and unweighted models ----

fit1 <- glmer(cs_diffcarer ~
                c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
                c12hour_z + more_s_r + neg_c_7_z + pos_v_4_z + e15_r + (1 | g2ctry),
              data = efc,
              family = binomial("logit"))

fit1w <- glmer(cs_diffcarer ~
                c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
                c12hour_z + more_s_r + neg_c_7_z + pos_v_4_z + e15_r + (1 | g2ctry),
              data = efc2,
              weights = adj_inv_weight,
              family = binomial("logit"))

tab_model(fit1, fit1w)



fit2 <- glmer(cs_inresidential ~
                c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
                c12hour_z + more_s_r + neg_c_7_z + pos_v_4_z + e15_r + (1 | g2ctry),
              data = efc,
              family = binomial("logit"))

fit2w <- glmer(cs_inresidential ~
                c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
                c12hour_z + more_s_r + neg_c_7_z + pos_v_4_z + e15_r + (1 | g2ctry),
              data = efc2,
              weights = adj_inv_weight,
              family = binomial("logit"))

tab_model(fit2, fit2w)



fit3 <- glmer(cs_diffcarer ~
                c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
                more_s_r + neg_c_7_z + pos_v_4_z + e15_r * c12hour_z + (1 | g2ctry),
              data = efc,
              family = binomial("logit"))

fit3w <- glmer(cs_diffcarer ~
                c161sex + c160age_z + c172code + c166reli_f + e42dep + demdiag_r + behave_r +
                more_s_r + neg_c_7_z + pos_v_4_z + e15_r * c12hour_z + (1 | g2ctry),
               data = efc2,
               weights = adj_inv_weight,
               family = binomial("logit"))

tab_model(fit3, fit3w)

