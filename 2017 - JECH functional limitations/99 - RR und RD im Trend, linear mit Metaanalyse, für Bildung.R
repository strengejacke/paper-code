library(dplyr)
library(sjstats)
library(sjmisc)

# gruppierter Datensatz, gruppieren nach Welle und Land, danach
# für jede Welle in jedem Land eine logistische Regression
ess_nested_non_mi <- ess %>%
  group_by(wave, country) %>%
  tidyr::nest() %>%
  ungroup()


# Modellberechnung muss manuell durchgeführt werden, da es Länder und Zeitpunkte
# gibt, in denen alle Fälle aufgrund von Missings wegfallen
res <- list()
for (i in seq_len(nrow(ess_nested_non_mi))) {
  x <- ess_nested_non_mi$data[[i]]
  dummy <- na.omit(data.frame(hlthhmp_d = x$hlthhmp_d, education_d = x$education_d,
                              agea = x$agea, gndr_n = x$gndr_n))
  if (nrow(dummy) > 2) {
    fit <- glm(hlthhmp_d ~ education_d + agea + gndr_n,
               family = binomial(link = "logit"),
               data = x)
  } else {
    fit <- NA
  }
  res[[length(res) + 1]] <- fit
}

# Fertig gerechnete Modelle als Variable speichern
ess_nested_non_mi$model.glm <- res


# absolute inequalities für jedes land und jede welle berechnen -----
ess_nested_non_mi$abs_inequ <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(education_d = "0", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  hi <- predict(x, newdata = data.frame(education_d = "1", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  return(100 * (low - hi))
}))

# relative inequalities für jedes land und jede welle berechnen -----
ess_nested_non_mi$rel_inequ <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(education_d = "0", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  hi <- predict(x, newdata = data.frame(education_d = "1", agea = mean(mf$agea), gndr_n = mean(mf$gndr_n)), type = "response")
  return(low / hi)
}))


# absolute inequalities für jedes land und jede welle berechnen, nur männer -----
ess_nested_non_mi$abs_inequ_m <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(education_d = "0", agea = mean(mf$agea), gndr_n = 1), type = "response")
  hi <- predict(x, newdata = data.frame(education_d = "1", agea = mean(mf$agea), gndr_n = 1), type = "response")
  return(100 * (low - hi))
}))

# relative inequalities für jedes land und jede welle berechnen, nur männer -----
ess_nested_non_mi$rel_inequ_m <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(education_d = "0", agea = mean(mf$agea), gndr_n = 1), type = "response")
  hi <- predict(x, newdata = data.frame(education_d = "1", agea = mean(mf$agea), gndr_n = 1), type = "response")
  return(low / hi)
}))


# absolute inequalities für jedes land und jede welle berechnen, nur frauen -----
ess_nested_non_mi$abs_inequ_w <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(education_d = "0", agea = mean(mf$agea), gndr_n = 2), type = "response")
  hi <- predict(x, newdata = data.frame(education_d = "1", agea = mean(mf$agea), gndr_n = 2), type = "response")
  return(100 * (low - hi))
}))

# relative inequalities für jedes land und jede welle berechnen, nur frauen -----
ess_nested_non_mi$rel_inequ_w <- unlist(lapply(ess_nested_non_mi$model.glm, function(x) {
  if (is.na(x)) return(NA)
  mf <- model.frame(x)
  low <- predict(x, newdata = data.frame(education_d = "0", agea = mean(mf$agea), gndr_n = 2), type = "response")
  hi <- predict(x, newdata = data.frame(education_d = "1", agea = mean(mf$agea), gndr_n = 2), type = "response")
  return(low / hi)
}))

# prävalenz functional limitations für jedes land und jede welle berechnen -----
ess_nested_non_mi$fun_lim <-
  100 * unlist(lapply(ess_nested_non_mi$data, function(x) {
    prop.table(table(x$hlthhmp_d))[2]
  }))

# prävalenz functional limitations für jedes land und jede welle berechnen -----
ess_nested_non_mi$fun_lim_m <-
  100 * unlist(lapply(ess_nested_non_mi$data, function(x) {
    prop.table(table(x$hlthhmp_d[x$gndr_n == 1]))[2]
  }))

# prävalenz functional limitations für jedes land und jede welle berechnen -----
ess_nested_non_mi$fun_lim_w <-
  100 * unlist(lapply(ess_nested_non_mi$data, function(x) {
    prop.table(table(x$hlthhmp_d[x$gndr_n == 2]))[2]
  }))

# numerice zeitvariable
ess_nested_non_mi$time <- to_value(ess_nested_non_mi$wave)

# hilfsfunktion, berechnet den linearen trend aus prävalenzraten
# (abs. inequ., rel. inequ. und func. limit.)
fit_models <- function(formula) {
  mdl <- list()
  for (i in unique(ess_nested_non_mi$country)) {
    tmp <- filter(ess_nested_non_mi, country == i)
    fit <- lm(formula, data = tmp)
    mdl[[length(mdl) + 1]] <- fit
  }
  return(mdl)
}

# extrahiert aus den lineare modellen für trend die notwendigen
# daten für die meta-analyse
create_data <- function(formula) {
  dummy <- as.data.frame(lapply(fit_models(formula), function(x) {
    c(coef(x)[2], confint(x)[2, ], se = summary(x)$coefficients[2, 2], p.val = summary(x)$coefficients[2, 4])
  }))
  colnames(dummy) <- unique(ess_nested_non_mi$country)
  dummy <- as.data.frame(t(dummy))
  tibble::rownames_to_column(dummy)
}

# daten für meta-analyse erstellen
absolute_inequ <- create_data(as.formula(abs_inequ ~ time))
relative_inequ <- create_data(as.formula(rel_inequ ~ time))

absolute_inequ_m <- create_data(as.formula(abs_inequ_m ~ time))
relative_inequ_m <- create_data(as.formula(rel_inequ_m ~ time))

absolute_inequ_w <- create_data(as.formula(abs_inequ_w ~ time))
relative_inequ_w <- create_data(as.formula(rel_inequ_w ~ time))

func_limit <- create_data(as.formula(fun_lim ~ time))
func_limit_m <- create_data(as.formula(fun_lim_m ~ time))
func_limit_w <- create_data(as.formula(fun_lim_w ~ time))

library(metafor)

ma_abs <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = absolute_inequ)
ma_rel <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = relative_inequ)

ma_abs_m <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = absolute_inequ_m)
ma_rel_m <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = relative_inequ_m)

ma_abs_w <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = absolute_inequ_w)
ma_rel_w <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = relative_inequ_w)

ma_fun_lim <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = func_limit)
ma_fun_lim_m <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = func_limit_m)
ma_fun_lim_w <- rma(yi = time, sei = se, slab = rowname, measure = "GEN", method = "REML", data = func_limit_w)

land_reihe <- c(1, 4, 6, 7, 3, 9, 10, 11, 12, 13, 14, 16, 5, 15, 2, 8)

pdf("Meta-education.pdf")

forest(ma_fun_lim, digits = 2, xlab = "Trends in Functional Limitations", order = land_reihe, mlab = "average")
forest(ma_fun_lim_m, digits = 2, xlab = "Trends in Functional Limitations (male)", order = land_reihe, mlab = "average")
forest(ma_fun_lim_w, digits = 2, xlab = "Trends in Functional Limitations (female)", order = land_reihe, mlab = "average")

forest(ma_abs, digits = 2, xlab = "Absolute Inequalities", order = land_reihe, mlab = "average")
forest(ma_rel, digits = 3, xlab = "Relative Inequalities", order = land_reihe, mlab = "average")

forest(ma_abs_m, digits = 2, xlab = "Absolute Inequalities (male)", order = land_reihe, mlab = "average")
forest(ma_rel_m, digits = 3, xlab = "Relative Inequalities (male)", order = land_reihe, mlab = "average")

forest(ma_abs_w, digits = 2, xlab = "Absolute Inequalities (female)", order = land_reihe, mlab = "average")
forest(ma_rel_w, digits = 3, xlab = "Relative Inequalities (female)", order = land_reihe, mlab = "average")

dev.off()


png(width = 1600, height = 1200, pointsize = 8, res = 300)

# Abbildung Trends in func. Limit. -----
par(font = 1, mar = c(4, 4, 1, 2))
forest(ma_fun_lim_m, digits = 2, xlab = "Trends in Functional Limitations (male)", order = land_reihe, mlab = "average")
par(font = 2)
text(x = grconvertX(-.025, "npc"), grconvertY(.96, "npc"), labels = "Country")
text(x = grconvertX(.975, "npc"), grconvertY(.96, "npc"), labels = "B (CI)")

par(font = 1, mar = c(4, 4, 1, 2))
forest(ma_fun_lim_w, digits = 2, xlab = "Trends in Functional Limitations (female)", order = land_reihe, mlab = "average")
par(font = 2)
text(x = grconvertX(-.025, "npc"), grconvertY(.96, "npc"), labels = "Country")
text(x = grconvertX(.975, "npc"), grconvertY(.96, "npc"), labels = "B (CI)")


# Abbildung abs. inequ. bei Männern -----
par(font = 1, mar = c(4, 4, 1, 2))
forest(ma_abs_m, digits = 2, xlab = "Absolute Inequalities (male)", order = land_reihe, mlab = "average")
par(font = 2)
text(x = grconvertX(-.025, "npc"), grconvertY(.96, "npc"), labels = "Country")
text(x = grconvertX(.975, "npc"), grconvertY(.96, "npc"), labels = "B (CI)")

par(font = 1, mar = c(4, 4, 1, 2))
forest(ma_abs_w, digits = 2, xlab = "Absolute Inequalities (female)", order = land_reihe, mlab = "average")
par(font = 2)
text(x = grconvertX(-.025, "npc"), grconvertY(.96, "npc"), labels = "Country")
text(x = grconvertX(.975, "npc"), grconvertY(.96, "npc"), labels = "B (CI)")


# Abbildung abs. inequ. bei Frauen -----
par(font = 1, mar = c(4, 4, 1, 2))
forest(ma_rel_m, digits = 3, xlab = "Relative Inequalities (male)", order = land_reihe, mlab = "average")
par(font = 2)
text(x = grconvertX(-.025, "npc"), grconvertY(.96, "npc"), labels = "Country")
text(x = grconvertX(.975, "npc"), grconvertY(.96, "npc"), labels = "B (CI)")

par(font = 1, mar = c(4, 4, 1, 2))
forest(ma_rel_w, digits = 3, xlab = "Relative Inequalities (female)", order = land_reihe, mlab = "average")
par(font = 2)
text(x = grconvertX(-.025, "npc"), grconvertY(.96, "npc"), labels = "Country")
text(x = grconvertX(.975, "npc"), grconvertY(.96, "npc"), labels = "B (CI)")

dev.off()
