# neue ISCED-3er ----

ego$education <- rec(
  ego$ISCED,
  rec = "0:2=0;3:4=1;5:6=2;else=NA",
  as.num = FALSE,
  var.label = "Educational level (ISCED)",
  val.labels = c("low (lower secondary)", "mid (upper/post-secondary)", "high (tertiary)")
)


# Bildung nach USUMA ----

ego$bildung_usuma <- NA
# rekodierung DE
ego$bildung_usuma[ego$c9 == 1] <- 0
ego$bildung_usuma[ego$c9 %in% c(2, 3, 4)] <- 1
ego$bildung_usuma[ego$c9 %in% c(5, 6)] <- 2
ego$bildung_usuma[ego$c9 %in% c(7, 8, 9)] <- 3
# rekodierung US
ego$bildung_usuma[ego$c91 == 1] <- 0
ego$bildung_usuma[ego$c91 == 2] <- 1
ego$bildung_usuma[ego$c91 %in% c(3, 4)] <- 2
ego$bildung_usuma[ego$c91 %in% c(5, 6)] <- 3

ego$bildung_usuma <- set_labels(ego$bildung_usuma, labels = c("Schüler", "Niedrig", "Mittel", "Hoch"))

# Tabelle 1: deskriptives, official statistics ----

ego %>%
  group_by(country) %>%
  frq(q100, q101_gr, weights = gewicht, out = "b")

table(ego$q101, useNA = "always")
table(ego$q101_qr, useNA = "always")


ego %>%
  group_by(country) %>%
  frq(c9_c, education, MIG, weights = gewicht)

xtab_statistics(ego, c9_c, country, weights = gewicht)
xtab_statistics(ego, MIG, country, weights = gewicht)

grpmean(ego, bmi, country, weights = gewicht)
wtd_ttest(bmi ~ country + gewicht, ego)

# score 1 ----

g_a <- grpmean(ego, ui, country, weights = gewicht)
g_b <- grpmean(ego, uj, country, weights = gewicht)
g_c <- grpmean(ego, ul, country, weights = gewicht)
g_d <- grpmean(ego, score1, country, weights = gewicht)

de <- rbind(g_a[1, c(2, 4)], g_b[1, c(2, 4)], g_c[1, c(2, 4)], g_d[1, c(2, 4)])
us <- rbind(g_a[2, c(2, 4)], g_b[2, c(2, 4)], g_c[2, c(2, 4)], g_d[2, c(2, 4)])

tab_df(cbind(us, de))

# score 2 ----

g_a <- grpmean(ego, ua, country, weights = gewicht)
g_b <- grpmean(ego, ub, country, weights = gewicht)
g_c <- grpmean(ego, ug, country, weights = gewicht)
g_d <- grpmean(ego, score2, country, weights = gewicht)

de <- rbind(g_a[1, c(2, 4)], g_b[1, c(2, 4)], g_c[1, c(2, 4)], g_d[1, c(2, 4)])
us <- rbind(g_a[2, c(2, 4)], g_b[2, c(2, 4)], g_c[2, c(2, 4)], g_d[2, c(2, 4)])

tab_df(cbind(us, de))


# score 3 ----

g_a <- grpmean(ego, ud, country, weights = gewicht)
g_b <- grpmean(ego, uf, country, weights = gewicht)
g_c <- grpmean(ego, uh, country, weights = gewicht)
g_d <- grpmean(ego, score3, country, weights = gewicht)

de <- rbind(g_a[1, c(2, 4)], g_b[1, c(2, 4)], g_c[1, c(2, 4)], g_d[1, c(2, 4)])
us <- rbind(g_a[2, c(2, 4)], g_b[2, c(2, 4)], g_c[2, c(2, 4)], g_d[2, c(2, 4)])

tab_df(cbind(us, de))


# score 4 ----

g_a <- grpmean(ego, uc, country, weights = gewicht)
g_b <- grpmean(ego, ue, country, weights = gewicht)
g_c <- grpmean(ego, uk, country, weights = gewicht)
g_d <- grpmean(ego, score4, country, weights = gewicht)

de <- rbind(g_a[1, c(2, 4)], g_b[1, c(2, 4)], g_c[1, c(2, 4)], g_d[1, c(2, 4)])
us <- rbind(g_a[2, c(2, 4)], g_b[2, c(2, 4)], g_c[2, c(2, 4)], g_d[2, c(2, 4)])

tab_df(cbind(us, de))
