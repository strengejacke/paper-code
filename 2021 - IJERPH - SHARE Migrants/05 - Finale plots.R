library(dplyr)
library(strengejacke)
library(easystats)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(survey)
library(patchwork)



load("share.RData")

share <- share %>%
  to_label(eurodcat, age_dicho, living, gender, make_ends_meet, education,
           migrant, migrant_western, country, chron_cond, gali, iadl_d,
           sphus_d, limited_adl_d, eurodcat)

d <- share %>%
  dplyr::filter(!is.na(crossin_weights)) %>%
  select(eurodcat, wave, migrant, age_between, gender, living, education, hhsize,
         age_within, mergeid, crossin_weights, country, sphus, chron_n, chron_cond,
         gali, iadl, limited_adl, limited_adl_d, iadl_d, sphus_d, age_dicho, eurodcat)

d$Age <- d$age_between

design <- svydesign(
  ids = ~ mergeid,
  weights = ~ crossin_weights,
  data = d,
  strata = ~ country,
  nest = TRUE
)






# Modelle rechnen --------------------------


f1 <- as.formula("gali ~ wave * migrant * Age * gender + education + hhsize + age_within")
m1 <- svyglm(f1, family = quasibinomial(), design = design)

f5 <- as.formula("sphus_d ~ wave * migrant * Age * gender + education + hhsize + age_within")
m5 <- svyglm(f5, family = quasibinomial(), design = design)

f6 <- as.formula("eurodcat ~ wave * migrant * Age * gender + education + hhsize + age_within")
m6 <- svyglm(f6, family = quasibinomial(), design = design)







# Plot-Funktion --------------------------------


abbildung <- function(model,
                      predict_data,
                      filter_facet,
                      filter_panel,
                      title = NULL,
                      prozent_label = TRUE,
                      jahr_labels = TRUE,
                      legend = FALSE,
                      ylim = c(0, 1),
                      y_dodge = -0.05) {

  emm <- emtrends(model,
                  c("migrant", "Age", "gender"),
                  var = "wave",
                  at = list(Age = c(-1, 1)),
                  adjust = "none")

  p_for_trends <- model_parameters(emm) %>%
    rec(Age, rec = c("-1=lower;1=higher"), suffix = "") |>
    filter(p < 0.05) |>
    select(-Coefficient, -SE, -CI_low, -CI_high, -z, - CI, - df_error) |>
    as.data.frame() |>
    sjmisc::rename_columns(migrant = "group", Age = "facet", gender = "panel") |>
    mutate(x = 6.0)

  predict_data <- merge(predict_data, p_for_trends, all = TRUE)
  predict_data$p <- insight::format_p(predict_data$p)

  round_percent <- function(ylim) {
    sprintf("%g%%", round(100 * seq(ylim[1], ylim[2], .05)))
  }

  plot_data <- predict_data[predict_data$facet == filter_facet & predict_data$panel == filter_panel, ]
  p <- ggplot(plot_data, aes(x = x, y = predicted, ymin = conf.low,
                             ymax = conf.high, fill = group, colour = group)) +
    geom_ribbon(alpha = .2, colour = NA) +
    geom_line() +
    geom_text(aes(label = p), nudge_y = y_dodge) +
    see::scale_color_flat() +
    see::scale_fill_flat() +
    see::theme_modern()

  if (legend) {
    p <- p +
      guides(color = "none") +
      theme(legend.position = "bottom") +
      labs(y = NULL, x = NULL, title = title, fill = "Migrant Background")
  } else {
    p <- p +
      guides(fill = "none", color = "none") +
      labs(y = NULL, x = NULL, title = title)
  }

  if (prozent_label) {
    p <- p + scale_y_continuous(labels = round_percent(ylim), breaks = seq(ylim[1], ylim[2], .05), limits = ylim)
  } else {
    p <- p + scale_y_continuous(labels = NULL, limits = ylim)
  }

  if (jahr_labels) {
    p <- p + scale_x_continuous(labels = c("2004", "2006", "", "2010", "2013", "2015", "2017"), breaks = 1:7)
  } else {
    p <- p + scale_x_continuous(labels = NULL)
  }

  p
}







# Abbildung 1: GALI --------------------------

pr1 <- ggemmeans(m1, c("wave [1:7 by=.5]", "migrant", "Age [-1,1]", "gender"))
pr1$facet <- factor(pr1$facet, labels = c("lower", "higher"))
pr1_range <- c(.35, .6)

p1 <- abbildung(m1,
                pr1,
                "lower",
                "male",
                title = "a) Lower aged male",
                jahr_labels = FALSE,
                ylim = pr1_range)

p2 <- abbildung(m1,
                pr1,
                "lower",
                "female",
                prozent_label = FALSE,
                jahr_labels = FALSE,
                title = "c) Lower aged female",
                ylim = pr1_range)

p3 <- abbildung(m1,
                pr1,
                "higher",
                "male",
                title = "b) Higher aged male",
                ylim = pr1_range)

p4 <- abbildung(m1,
                pr1,
                "higher",
                "female",
                prozent_label = FALSE,
                title = "d) Higher aged female",
                ylim = pr1_range,
                legend = TRUE)


p1 + p2 + p3 + p4
ggsave("gali.tiff", compress = "lzw", units = "cm", width = 13, height = 8,
       scale = 2, dpi = 600)







# Abbildung 2: Depression --------------------------

pr2 <- ggemmeans(m6, c("wave [1:7 by=.5]", "migrant", "Age [-1,1]", "gender"))
pr2$facet <- factor(pr2$facet, labels = c("lower", "higher"))
pr2_range <- c(.15, .45)

p1 <- abbildung(m6,
                pr2,
                "lower",
                "male",
                title = "a) Lower aged male",
                jahr_labels = FALSE,
                ylim = pr2_range)

p2 <- abbildung(m6,
                pr2,
                "lower",
                "female",
                prozent_label = FALSE,
                jahr_labels = FALSE,
                title = "c) Lower aged female",
                ylim = pr2_range)

p3 <- abbildung(m6,
                pr2,
                "higher",
                "male",
                title = "b) Higher aged male",
                ylim = pr2_range)

p4 <- abbildung(m6,
                pr2,
                "higher",
                "female",
                prozent_label = FALSE,
                title = "d) Higher aged female",
                ylim = pr2_range,
                legend = TRUE)


p1 + p2 + p3 + p4
ggsave("depression.tiff", compress = "lzw", units = "cm", width = 13, height = 8,
       scale = 2, dpi = 600)





# Abbildung 3: SPH --------------------------

pr3 <- ggemmeans(m5, c("wave [1:7 by=.5]", "migrant", "Age [-1,1]", "gender"))
pr3$facet <- factor(pr3$facet, labels = c("lower", "higher"))
pr3_range <- c(.65, .9)

p1 <- abbildung(m5,
                pr3,
                "lower",
                "male",
                title = "a) Lower aged male",
                jahr_labels = FALSE,
                ylim = pr3_range,
                y_dodge = -0.03)

p2 <- abbildung(m5,
                pr3,
                "lower",
                "female",
                prozent_label = FALSE,
                jahr_labels = FALSE,
                title = "c) Lower aged female",
                ylim = pr3_range,
                y_dodge = -0.03)

p3 <- abbildung(m5,
                pr3,
                "higher",
                "male",
                title = "b) Higher aged male",
                ylim = pr3_range,
                y_dodge = -0.03)

p4 <- abbildung(m5,
                pr3,
                "higher",
                "female",
                prozent_label = FALSE,
                title = "d) Higher aged female",
                ylim = pr3_range,
                legend = TRUE,
                y_dodge = -0.03)


p1 + p2 + p3 + p4
ggsave("sph.tiff", compress = "lzw", units = "cm", width = 13, height = 8,
       scale = 2, dpi = 600)
