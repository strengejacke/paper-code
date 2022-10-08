library(sjPlot)
library(performance)

l1 <- lapply(qualidem_pp, table, useNA = "always")
l2 <- lapply(qualidem_pp, function(i) prop.table(table(i, useNA = "always")))
res <- mapply(function(i, j) sprintf("%i (%.1f%%)", i, 100 * j), l1, l2)

d1 <- as.data.frame(t(res), stringsAsFactors = FALSE)
d2 <- data.frame(
  meansd = sprintf("%.1f (%.1f)", colMeans(qualidem_pp, na.rm = TRUE), sapply(qualidem_pp, sd, na.rm = TRUE)),
  floor = sprintf("%.2f", sapply(qualidem_pp, function(i) sum(i == 0, na.rm = TRUE) / nrow(qualidem_pp))),
  ceiling = sprintf("%.2f", sapply(qualidem_pp, function(i) sum(i == 3, na.rm = TRUE) / nrow(qualidem_pp))),
  difficulty = sprintf("%.2f", item_difficulty(qualidem_pp)$difficulty)
)

tab_df(cbind(d1, d2))
tab_stackfrq(qualidem_pp, show.n = TRUE, show.total = TRUE, show.na = TRUE)



l1 <- lapply(qualidem_s_pp, table, useNA = "always")
l2 <- lapply(qualidem_s_pp, function(i) prop.table(table(i, useNA = "always")))
res <- mapply(function(i, j) sprintf("%i (%.1f%%)", i, 100 * j), l1, l2)

d1 <- as.data.frame(t(res), stringsAsFactors = F)
d2 <- data.frame(
  meansd = sprintf("%.1f (%.1f)", colMeans(qualidem_s_pp, na.rm = T), sapply(qualidem_s_pp, sd, na.rm = TRUE)),
  difficulty = sprintf("%.2f", item_difficulty(qualidem_s_pp)$difficulty)
)

tab_df(cbind(d1, d2))
tab_stackfrq(qualidem_s_pp, show.n = T, show.total = T, show.na = T)



colMeans(datawizard::normalize(qualidem_pp))
max(qualidem_pp$QD1_QSchuld, na.rm = TRUE)


sapply(qualidem_pp, function(i) sum(i == 0, na.rm = TRUE))
