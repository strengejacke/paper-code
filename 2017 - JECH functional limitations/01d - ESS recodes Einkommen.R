# -----------------------------------------------
# Haushaltseinkommen, Dicho
# -----------------------------------------------
dicho.label <- c("low", "high")
dicho.recode_1 <- "1:6=0;7:12=1;else=NA"
dicho.recode_2 <- "1:5=0;6:10=1;else=NA"


# -----------------------------------------------
# Haushaltsäquivalenzeinkommen, dicho, für Wellen 1-3
# -----------------------------------------------
ess7$aquinc <- ess7$hinctnta / ((ess7$hhmmb - 1) * .5 + 1)
ess6$aquinc <- ess6$hinctnta / ((ess6$hhmmb - 1) * .5 + 1)
ess5$aquinc <- ess5$hinctnta / ((ess5$hhmmb - 1) * .5 + 1)
ess4$aquinc <- ess4$hinctnta / ((ess4$hhmmb - 1) * .5 + 1)
ess3$aquinc <- ess3$hinctnta / ((ess3$hhmmb - 1) * .5 + 1)
ess2$aquinc <- ess2$hinctnta / ((ess2$hhmmb - 1) * .5 + 1)
ess1$aquinc <- ess1$hinctnta / ((ess1$hhmmb - 1) * .5 + 1)


# -----------------------------------------------
# Haushaltseinkommen, dicho, für Wellen 1-3
# -----------------------------------------------
ess1$hhinc_d <- rec(ess1$hinctnta, dicho.recode_1, to.factor = TRUE, val.labels = dicho.label)
ess2$hhinc_d <- rec(ess2$hinctnta, dicho.recode_1, to.factor = TRUE, val.labels = dicho.label)
ess3$hhinc_d <- rec(ess3$hinctnta, dicho.recode_1, to.factor = TRUE, val.labels = dicho.label)
# -----------------------------------------------
# Haushaltäquivalenzseinkommen, dicho, für Wellen 1-3
# -----------------------------------------------
ess1$aquinc_d <- rec(ess1$aquinc, dicho.recode_1, to.factor = TRUE, val.labels = dicho.label)
ess2$aquinc_d <- rec(ess2$aquinc, dicho.recode_1, to.factor = TRUE, val.labels = dicho.label)
ess3$aquinc_d <- rec(ess3$aquinc, dicho.recode_1, to.factor = TRUE, val.labels = dicho.label)


# -----------------------------------------------
# Haushaltseinkommen, dicho, für Wellen 4-7
# -----------------------------------------------
ess4$hhinc_d <- rec(ess4$hinctnta, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)
ess5$hhinc_d <- rec(ess5$hinctnta, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)
ess6$hhinc_d <- rec(ess6$hinctnta, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)
ess7$hhinc_d <- rec(ess7$hinctnta, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)
# -----------------------------------------------
# Haushaltäquivalenzseinkommen, dicho, für Wellen 4-7
# -----------------------------------------------
ess4$aquinc_d <- rec(ess4$aquinc, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)
ess5$aquinc_d <- rec(ess5$aquinc, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)
ess6$aquinc_d <- rec(ess6$aquinc, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)
ess7$aquinc_d <- rec(ess7$aquinc, dicho.recode_2, to.factor = TRUE, val.labels = dicho.label)



# -----------------------------------------------
# Haushaltseinkommen, Terzile
# -----------------------------------------------
terz.label <- c("lower tertile", "middle tertile", "upper tertile")
terz.recode_1 <- "1:4=1;5:8=2;9:12=3;else=NA"
terz.recode_2 <- "1:4=1;5:7=2;8:10=3;else=NA"

# -----------------------------------------------
# Haushaltseinkommen, Terzile, für Wellen 1-3
# -----------------------------------------------
ess1$hhinc_3 <- rec(ess1$hinctnta, terz.recode_1, to.factor = TRUE, val.labels = terz.label)
ess2$hhinc_3 <- rec(ess2$hinctnta, terz.recode_1, to.factor = TRUE, val.labels = terz.label)
ess3$hhinc_3 <- rec(ess3$hinctnta, terz.recode_1, to.factor = TRUE, val.labels = terz.label)


# -----------------------------------------------
# Haushaltseinkommen, Terzile, für Wellen 4-7
# -----------------------------------------------
ess4$hhinc_3 <- rec(ess4$hinctnta, terz.recode_2, to.factor = TRUE, val.labels = terz.label)
ess5$hhinc_3 <- rec(ess5$hinctnta, terz.recode_2, to.factor = TRUE, val.labels = terz.label)
ess6$hhinc_3 <- rec(ess6$hinctnta, terz.recode_2, to.factor = TRUE, val.labels = terz.label)
ess7$hhinc_3 <- rec(ess7$hinctnta, terz.recode_2, to.factor = TRUE, val.labels = terz.label)



# -----------------------------------------------
# Haushaltseinkommen, Median-Split der Einkommensgruppen, länderspezifisch
# -----------------------------------------------
ess.all <- list(ess1, ess2, ess3, ess4, ess5, ess6, ess7)
hhinc_med.all <- list()
for (j in seq_len(length(ess.all))) {
  ess.single <- ess.all[[j]]
  x <- as.character(na.omit(unique(ess.single$cntry)))
  hhinc_med <- vector("double", length = nrow(ess.single))
  for (i in seq_len(length(x))) {
    # Länderfälle auswählen
    indices <- which(ess.single$cntry == x[i])
    # zwei Split-versuche. Median ist nicht immer optimal bei 10 oder
    # 12 Gruppen, daher die Gruppe auswählen, die eher einer 50/50
    # Verteilung entspricht
    v1 <- split_var(ess.single$hinctnta[indices], 2, inclusive = FALSE)
    v2 <- split_var(ess.single$hinctnta[indices], 2, inclusive = TRUE)
    # Differenzen der beiden Verteilungen. Die, die näher an der
    # 50/50 Verteilung ist, hat den kleineren Wert (Differenz)
    d1 <- as.vector(abs(diff(table(v1))))
    d2 <- as.vector(abs(diff(table(v2))))
    # dann den besseren Wert wählen
    if (d1 > d2)
      hhinc_med[indices] <- v2
    else
      hhinc_med[indices] <- v1
  }
  hhinc_med.all[[length(hhinc_med.all) + 1]] <- hhinc_med
}

ess1$hhinc_med <- hhinc_med.all[[1]]
ess2$hhinc_med <- hhinc_med.all[[2]]
ess3$hhinc_med <- hhinc_med.all[[3]]
ess4$hhinc_med <- hhinc_med.all[[4]]
ess5$hhinc_med <- hhinc_med.all[[5]]
ess6$hhinc_med <- hhinc_med.all[[6]]
ess7$hhinc_med <- hhinc_med.all[[7]]



# -----------------------------------------------
# Haushaltsäquivalenzeinkommen, Median-Split der Einkommensgruppen, länderspezifisch
# -----------------------------------------------
ess.all <- list(ess1, ess2, ess3, ess4, ess5, ess6, ess7)
aquinc_med.all <- list()
for (j in seq_len(length(ess.all))) {
  ess.single <- ess.all[[j]]
  x <- as.character(na.omit(unique(ess.single$cntry)))
  aquinc_med <- vector("double", length = nrow(ess.single))
  for (i in seq_len(length(x))) {
    # Länderfälle auswählen
    indices <- which(ess.single$cntry == x[i])
    # zwei Split-versuche. Median ist nicht immer optimal bei 10 oder
    # 12 Gruppen, daher die Gruppe auswählen, die eher einer 50/50
    # Verteilung entspricht
    v1 <- split_var(ess.single$aquinc[indices], 2, inclusive = FALSE)
    v2 <- split_var(ess.single$aquinc[indices], 2, inclusive = TRUE)
    # Differenzen der beiden Verteilungen. Die, die näher an der
    # 50/50 Verteilung ist, hat den kleineren Wert (Differenz)
    d1 <- as.vector(abs(diff(table(v1))))
    d2 <- as.vector(abs(diff(table(v2))))
    # dann den besseren Wert wählen
    if (d1 > d2)
      aquinc_med[indices] <- v2
    else
      aquinc_med[indices] <- v1
  }
  aquinc_med.all[[length(aquinc_med.all) + 1]] <- aquinc_med
}

ess1$aquinc_med <- aquinc_med.all[[1]]
ess2$aquinc_med <- aquinc_med.all[[2]]
ess3$aquinc_med <- aquinc_med.all[[3]]
ess4$aquinc_med <- aquinc_med.all[[4]]
ess5$aquinc_med <- aquinc_med.all[[5]]
ess6$aquinc_med <- aquinc_med.all[[6]]
ess7$aquinc_med <- aquinc_med.all[[7]]
