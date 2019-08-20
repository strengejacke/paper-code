library(tidyverse)
library(ggridges)
library(sjmisc)
library(sjlabelled)
library(sjstats)
library(sjPlot)
library(insight)
library(bayestestR)
library(brms)

# Data available at https://doi.org/10.5281/zenodo.1479676

# load data ----

load("Dataset.RData")

# divide age by 10

d$age10 <- d$age / 10


# Labels for final model ----

labs <-
  c(
    stay_c = "Length of Stay",
    age = "Age",
    age10 = "Age",
    mmse2 = "Moderate Dementia",
    mmse3 = "Severe Dementia",
    sex2 = "Female Sex",
    barthel_code = "Barthel-Index",
    groupintervention = "Special Care Ward",
    physres1 = "Physical Restraints",
    pas_c = "PAS-Score",
    cci_c = "Charlson's Comorbidity Index",
    chemicalres1 = "Psychotropic Drug Use", # oder as-needed
    b_stay_c = "Length of Stay",
    b_age = "Age",
    b_age10 = "Age",
    b_mmse2 = "Moderate Dementia",
    b_mmse3 = "Severe Dementia",
    b_sex2 = "Female Sex",
    b_barthel_code = "Barthel-Index",
    b_groupintervention = "Special Care Ward",
    b_physres1 = "Physical Restraints",
    b_pas_c = "PAS-Score",
    b_cci_c = "Charlson's Comorbidity Index",
    b_chemicalres1 = "Psychotropic Drug Use" # oder as-needed
  )

# prior-definition in brms ----

# scale is 2.5 * sd(y) / sd(x)

bprior <-
  prior(normal(0, 6), class = "b", coef = "stay_c") +
  prior(normal(.1554, 40), class = "b", coef = "age10") +
  prior(normal(0, 42), class = "b", coef = "mmse2") +
  prior(normal(-.444, 42), class = "b", coef = "mmse3") +
  prior(normal(-3.219, 42), class = "b", coef = "sex2") +
  prior(normal(0, 29), class = "b", coef = "barthel_code") +
  prior(normal(-5, 42), class = "b", coef = "physres1") +
  prior(normal(0, 42), class = "b", coef = "groupintervention") +
  prior(normal(0, 13), class = "b", coef = "pas_c") +
  prior(normal(.2925, 42), class = "b", coef = "chemicalres1") +
  prior(normal(0, 26.77), class = "b", coef = "cci_c")

# see:
# Beerens HC, Sutcliffe C, Renom-Guiteras A, et al. Quality of Life and
# Quality of Care for People With Dementia Receiving Long Term Institutional
# Care or Professional Home Care: The European RightTimePlaceCare Study.
# Journal of the American Medical Directors Association. 2014;15(1):54-61.
# doi:10.1016/j.jamda.2013.09.010
#
# QoL-Scale ranges from 13-52 (40 points). Effects from those study are
# multiplied by 2.5 (rescaling 40-points to 100-point-scale of QUALIDEM)

# see:
# Barca, M. L., Engedal, K., Laks, J., & Selbæk, G. (2011). Quality of Life
# among Elderly Patients with Dementia in Institutions. Dementia and Geriatric
# Cognitive Disorders, 31(6), 435–442. https://doi.org/10.1159/000328969

# QoL-scale ranges from 11-55 (45 points). Effects from those study are
# multiplied by 2.2 (rescaling 45-points to 100-point-scale of QUALIDEM)


# model formula ----

mf <-
  formula(
    QoL ~ stay_c + age10 + mmse + sex + cci_c +
      barthel_code + physres + group + pas_c +
      chemicalres + (1 | maindiag)
  )


# brms-model ----

set.seed(1207)

m2a <- brm(
  formula = mf,
  data = d,
  prior = bprior,
  sample_prior = TRUE
)


# Figure 3 ----

theme_set(theme_sjplot2(base_size = 14, base_family = "serif"))

p <- plot_model(
  m2a,
  title = "",
  axis.labels = labs,
  sort.est = T,
  colors = c("grey30"),
  axis.title = "Change in QUALIDEM-Score",
  wrap.title = 100,
  wrap.labels = 20,
  width = .2,
  grid.breaks = 2,
  size.inner = .1
) +
  ylab("Change in QUALIDEM Total Score") +
  theme_sjplot2(base_size = 14, base_family = "serif")

p_pdf <- p +
  theme_sjplot2(base_size = 28, base_family = "serif") +
  theme(
    panel.grid.major = element_line(size = .1),
    panel.grid.minor = element_line(size = .05),
    axis.line.x      = element_line(size = .15),
    axis.line.y      = element_line(size = .15)
  )

ggsave(filename = "Fig3.tiff", plot = p, width = 170, height = 120, units = "mm", dpi = 300, compression = "lzw")
ggsave(filename = "Fig3.pdf", scale = 2, plot = p_pdf, width = 170, height = 120, units = "mm", dpi = 300)


# Appendix S1: Test for practical equivalence ----

rope(m2a, rope = c(-6, 6))
rope(m2a, rope = c(-7.5, 7.5))

equivalence_test(m2a, parameters = "^(?!prior)")
equivalence_test(m2a, parameters = "^(?!prior)") %>% plot()


# Appendix S1, Table Regression Coefficients ----

tab_df(tidy_stan(m2a, prob = c(.5, .89), digits = 1))


# Appendix S1, Prior Adjustement ----

insight::get_priors(m2a)


# Appendix S1, Figure distribution Posterior Samples ----

tmp <- m2a %>%
  as_tibble() %>%
  select(2:12) %>%
  gather(key = "predictor", value = "estimate") %>%
  to_factor(predictor)

tmp$predictor <- lvls_reorder(tmp$predictor, c(9, 4, 8, 3, 7, 11, 10, 6, 1, 2, 5))

p2 <- ggplot(tmp, aes(x = estimate, y = predictor)) +
  geom_vline(xintercept = 0, colour = "grey70", size = .8) +
  geom_density_ridges2(rel_min_height = 0.001, scale = 2, alpha = .5) +
  scale_x_continuous(breaks = seq(-8, 8, 2)) +
  scale_y_discrete(labels = labs) +
  labs(x = "Change in QUALIDEM-Score", y = NULL) +
  theme_sjplot2(base_size = 14, base_family = "serif")

ggsave(filename = "FigS1.tiff", plot = p2, width = 190, height = 120, units = "mm", dpi = 300)


# Appendix S1, test for practical equivalence ----

## Short version

equivalence_test(m2a, parameters = "^(?!prior)")
equivalence_test(m2a, parameters = "^(?!prior)") %>% plot()


## More beautiful tweaked version

tmp.hdi <- hdi(m2a, prob = .95) %>%
  slice(c(-c(1, 13:23)))

tmp2 <- m2a %>%
  as_tibble() %>%
  select(2:12) %>%
  map2_df(tmp.hdi$CI_low, function(x, y) {
    x[x < y] <- NA
    x
  }) %>%
  map2_df(tmp.hdi$CI_high, function(x, y) {
    x[x > y] <- NA
    x
  }) %>%
  gather(key = "predictor", value = "estimate") %>%
  to_factor(predictor)

tmp2$predictor <- lvls_reorder(tmp2$predictor, c(9, 4, 8, 3, 7, 11, 10, 6, 1, 2, 5))

tmp2$grp <- dplyr::case_when(
  tmp2$predictor %in% c("b_stay_c", "b_cci_c") ~ "reject",
  tmp2$predictor %in% c("b_age10", "b_mmse2", "b_mmse3", "b_sex2", "b_barthel_code") ~ "undecided",
  TRUE ~ "accept"
)

p3 <- ggplot(tmp2, aes(x = estimate, y = predictor, fill = grp)) +
  # rope based on "equi_test(model)".
  annotate("rect", xmin = -1.7, xmax = 1.7, ymin = 0, ymax = Inf, fill = sjplot_pal(palette = "us")[1], alpha = 0.15) +
  geom_vline(xintercept = 0, colour = "grey70", size = .8) +
  geom_density_ridges2(rel_min_height = 0.01, scale = 2, alpha = .4) +
  scale_x_continuous(breaks = seq(-8, 8, 2)) +
  scale_y_discrete(labels = labs) +
  scale_fill_manual(values = sjplot_pal()[c(3, 1, 7)]) +
  labs(x = "Change in QUALIDEM-Score", y = NULL, fill = "Acceptance of Parameters") +
  theme_sjplot2(base_size = 14, base_family = "serif") +
  theme(
    legend.title = element_text(size = 13),
    legend.position = "bottom",
    axis.line.x = element_line(colour = "grey50"),
    axis.line.y = element_line(colour = "grey50")
  )

ggsave(filename = "FigS2.tiff", plot = p3, width = 190, height = 120, units = "mm", dpi = 300)


# Appendix S1, Posterior-Prior-Check ----

## Short version

plot_model(m2a, type = "diag", axis.lim = c(-20, 20))

## More beautiful tweaked version

pr_samp <- prior_samples(m2a) %>%
  select(starts_with("b_")) %>%
  gather(key = "Term", value = "Estimate") %>%
  mutate(Sample = "prior")

ps_samp <- posterior_samples(m2a) %>%
  select(starts_with("b_"), -b_Intercept) %>%
  gather(key = "Term", value = "Estimate") %>%
  mutate(Sample = "posterior")

m_pp_data <- bind_rows(pr_samp, ps_samp) %>% to_factor(Term)
m_pp_data$Term <- lvls_reorder(m_pp_data$Term, rev(c(8, 3, 10, 4, 7, 5, 11, 6, 1, 2, 9)))

p4 <- ggplot(m_pp_data, aes(x = Estimate, fill = Sample)) +
  geom_density(alpha = .4) +
  scale_x_continuous(limits = c(-20, 20)) +
  facet_wrap(
    ~ Term,
    scales = "free",
    labeller = labeller(Term = labs),
    nrow = 4
  ) +
  labs(x = NULL, y = NULL) +
  bayesplot::theme_default(base_size = 13) +
  theme(
    axis.line.x      = element_line(colour = "grey50"),
    axis.line.y      = element_line(colour = "grey50"),
    axis.text        = element_text(colour = "grey10"),
    axis.title       = element_text(colour = "black"),
    # strip.background = element_rect(colour = "grey50", fill = "grey90"),
    # strip.text       = element_text(colour = "grey20"),
    legend.title     = element_text(colour = "grey10"),
    legend.text      = element_text(colour = "grey20"),
    legend.position  = c(.5, .15),
    legend.justification = c(-2, 1)
  ) +
  scale_fill_manual(values = sjplot_pal("breakfast club")[c(1, 3)])

ggsave(filename = "FigS3.tiff", plot = p4, width = 190, height = 214, units = "mm", dpi = 300)


# Appendix S1, Traceplot ----

p <- rstan::traceplot(m2a$fit, pars = colnames(as.data.frame(m2a))[1:12], inc_warmup = F)
p$data$parameter <- as.character(p$data$parameter)
tmp <- p$data %>%
  filter(parameter != "b_Intercept")

for (i in 1:length(labs)) {
  if (names(labs)[i] %in% tmp$parameter) {
    r <- which(tmp$parameter == names(labs)[i])
    tmp$parameter[r] <- labs[i]
  }
}

p5 <- ggplot(tmp, aes(x = iteration, y = value, colour = chain)) +
  geom_line() +
  facet_wrap(~parameter, scales = "free_y", ncol = 3) +
  scale_color_manual(values = sjplot_pal("us", n = 4)) +
  labs(x = NULL, y = NULL) +
  bayesplot::theme_default(base_size = 13) +
  theme(
    axis.line.x      = element_line(colour = "grey50"),
    axis.line.y      = element_line(colour = "grey50"),
    axis.text        = element_text(colour = "grey10"),
    axis.title       = element_text(colour = "black"),
    # strip.background = element_rect(colour = "grey50", fill = "grey90"),
    # strip.text       = element_text(colour = "grey20"),
    legend.title     = element_text(colour = "grey10"),
    legend.text      = element_text(colour = "grey20"),
    legend.position  = c(.5, .15),
    legend.justification = c(-4.2, 0.7)
  )

ggsave(filename = "FigS4.tiff", compress = "lzw", plot = p5, width = 190, height = 214, units = "mm", dpi = 300)
