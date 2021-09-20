library(dplyr)
library(strengejacke)
library(easystats)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(survey)
library(tidyr)
library(purrr)
library(metafor)
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


# create subsets of data, split by wave and country

sub_data <- d %>%
  group_by(wave, country) %>%
  nest()






# Functions --------------------------

plot_data <- function(form) {

  ## run logistic regression on each subset, and predict the probability of
  ## outcome for each country at each wave. these data points are used for the
  ## meta-analysis

  response <- deparse(form[[2]])

  data_list <- lapply(1:nrow(sub_data), function(i) {
    dat <- sub_data$data[[i]]
    if (!all_na(dat$migrant) && !all_na(dat[[response]]) && nlevels(droplevels(dat$migrant)) > 1) {
      m <- glm(form, family = binomial(), data = dat)
      pr <- ggemmeans(m, "migrant")
      pr$wave <- sub_data$wave[i]
      pr$country <- sub_data$country[i]
      pr
    } else {
      NA
    }
  })


  ## bind all predictions

  pred <- do.call(rbind, data_list)
  pred <- pred[complete.cases(pred), ]


  ## run linear models on predicted probabilities, to calculate the trend

  models <- pred %>%
    group_by(x, country) %>%
    nest() %>%
    mutate(models = map(data, ~ lm(predicted ~ wave, data = .))) %>%
    spread_coef(models, se = TRUE)


  ## run meta analysis on slopes (predicted trends)

  ma <- rma(
    yi = wave,
    mods = ~ x - 1,
    sei = wave.se,
    slab = country,
    measure = "GEN",
    method = "REML",
    data = models
  )


  ## some preparation for plotting

  mp <- model_parameters(ma)
  mp$Subgroup <- ""
  mp$Subgroup[grepl("\\.1$", mp$Parameter)] <- "non-migrant"
  mp$Subgroup[grepl("\\.2$", mp$Parameter)] <- "migrant"
  mp$Subgroup[grepl("xnon-migrant", mp$Parameter)] <- "non-migrant"
  mp$Subgroup[grepl("xmigrant", mp$Parameter)] <- "migrant"
  mp$Parameter[grepl("^(xmigrant|xnon-migrant)", mp$Parameter)] <- "Overall"
  mp$Parameter <- gsub("(.*)(\\.1|\\.2)$", "\\1", mp$Parameter)
  mp$Parameter <- factor(mp$Parameter, levels = rev(unique(mp$Parameter)))


  ## return plot data

  mp[c("Parameter", "Coefficient", "CI_low", "CI_high", "Subgroup")]
}






forest_plot <- function(plotdata, title, dodge = .7, show_legend = TRUE, show_labels = TRUE) {
  p <- ggplot(plotdata, aes(x = Coefficient, xmin = CI_low, xmax = CI_high,
                       y = Parameter, colour = Subgroup)) +
    geom_vline(xintercept = 0, colour = "#aaaaaa") +
    geom_errorbar(position = position_dodge(dodge), width = 0, size = .75) +
    geom_point(position = position_dodge(dodge), size = 2) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_manual(values = c("migrant" = "#e74c3c", "non-migrant" = "#2980b9")) +
    theme_lucid() +
    labs(y = NULL, colour = NULL,
         x = NULL, title = title) +
    theme(axis.text = element_text(colour = "#333333"))

  if (!show_labels) {
    p <- p + scale_y_discrete(labels = NULL) +
      theme(axis.line.y = element_blank())
  }

  if (show_legend) {
    p <- p + theme(legend.position = "bottom")
  } else {
    p <- p + guides(colour = "none")
  }

  p
}






# Trend gali ----------------


## model specification

f <- as.formula("gali ~ migrant + Age + gender + education + hhsize + age_within")
plotdata1 <- plot_data(f)


## plot

plot_gali <- forest_plot(plotdata1, title = "GALI",
                         show_legend = FALSE, show_labels = TRUE)




# Trend depression ----------------


## model specification

f <- as.formula("eurodcat ~ migrant + Age + gender + education + hhsize + age_within")
plotdata2 <- plot_data(f)


## plot

plot_depressed <- forest_plot(plotdata2, title = "Euro-D",
                              show_legend = TRUE, show_labels = FALSE)





# Trend sph ----------------


## model specification

f <- as.formula("sphus_d ~ migrant + Age + gender + education + hhsize + age_within")
plotdata3 <- plot_data(f)


## plot

plot_sph <- forest_plot(plotdata3, title = "SPH",
                        show_legend = FALSE, show_labels = FALSE)





# Final plot ----------------------

final_plot <- plot_gali + plot_depressed + plot_sph

ggsave("trendplots.tiff", plot = final_plot, compress = "lzw", units = "cm",
       width = 15, height = 10, scale = 1.5, dpi = 600)
