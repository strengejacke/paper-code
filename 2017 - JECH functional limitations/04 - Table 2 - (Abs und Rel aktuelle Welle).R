# Für Männer und Frauen ---------

# gruppierter Datensatz, gruppieren nach Welle und Land, danach
# für jede Welle in jedem Land eine logistische Regression
source("04x - rd_rr_by_wave_and_gender.R")

welle <- 7
# 1 für Männer, 2 für Frauen
geschlecht <- 1

# separat abs and rel. inequ über all ländern -----
fit <- glm(hlthhmp_d ~ aquinc_med + agea,
           family = binomial(link = "logit"),
           data = filter(ess, wave == welle, gndr_n == geschlecht))

mf <- model.frame(fit)
low <- predict(fit, newdata = data.frame(aquinc_med = "1", agea = mean(mf$agea)), type = "response")
hi <- predict(fit, newdata = data.frame(aquinc_med = "2", agea = mean(mf$agea)), type = "response")

abs_inequ_all <- 100 * (low - hi)
rel_inequ_all <- low / hi

results <- rd_rr_by_wave_and_gender(welle, geschlecht)

d1 <- data.frame(results$t1, results$t2[, -1], pval = results$t3[, -1])
d1$pval <- as.character(d1$pval)

d2 <- data.frame(country = "All", abs_inequ = sprintf("%.2f", abs_inequ_all), rel_inequ = sprintf("%.2f", rel_inequ_all),
                 pval = sprintf("%.3f", summary(fit)$coefficients[2, 4]))

d2$country <- as.character(d2$country)
d2$abs_inequ <- as.character(d2$abs_inequ)
d2$rel_inequ <- as.character(d2$rel_inequ)
d2$pval <- as.character(d2$pval)

sjPlot::tab_df(bind_rows(d1, d2))
