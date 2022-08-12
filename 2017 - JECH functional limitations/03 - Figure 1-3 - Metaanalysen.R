source("01g - ESS nested dataset.R")

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

pdf("Meta.pdf")

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
tiff(width = 1600, height = 1200, pointsize = 8, res = 300)

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
