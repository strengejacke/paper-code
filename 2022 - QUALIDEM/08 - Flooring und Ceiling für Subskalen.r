library(easystats)
library(dplyr)

load("david_recodes.RData")
david$schwer_dement_d <- as.factor(as.numeric(david$schwer_dement2))
levels(david$schwer_dement_d) <- c("leichte/mittlere Demenz", "schwere Demenz")

d <- data_filter(david, schwer_dement_d == "leichte/mittlere Demenz")
ds <- data_filter(david, schwer_dement_d == "schwere Demenz")


# flooring/ceiling QUALIDEM mild to severe dementia ----------------

qd_scores <- list(
  d$QD1_pflegebez,
  d$QD2_posaf,
  d$QD3_negaf,
  d$QD4_ruhelos,
  d$QD5_selbst,
  d$QD6_bez,
  d$QD7_iso,
  d$QD8_home,
  d$QD9_sinn,
  d$QD_total
)
qd_range <- c(21, 18, 9, 9, 9, 18, 9, 12, 6, 111)
names(qd_scores) <- sprintf("QD%i_score", 1:10)

out <- lapply(1:10, function(i) {
  s <- (100 / qd_range[i]) * qd_scores[[i]] # rescale to 0-100
  data.frame(
    flooring = round(100 * sum(s < 10, na.rm = TRUE) / sum(!is.na(s)), 1),
    ceiling = round(100 * sum(s > 90, na.rm = TRUE) / sum(!is.na(s)), 1)
  )
})

df_floorceiling1 <- do.call(rbind, out)
df_floorceiling1



# flooring/ceiling QUALIDEM very severe dementia ----------------

qd_scores <- list(
  ds$QD1s_pflegebez,
  ds$QD2s_posaf,
  ds$QD3s_negaf,
  ds$QD4s_ruhelos,
  NULL,
  ds$QD6s_bez,
  ds$QD7s_iso,
  NULL,
  NULL,
  ds$QDs_total
)
qd_range <- c(9, 12, 6, 9, NA, 9, 9, NA, NA, 54)
names(qd_scores) <- sprintf("QD%i_score", 1:10)

out <- lapply(1:10, function(i) {
  s <- (100 / qd_range[i]) * qd_scores[[i]] # rescale to 0-100
  data.frame(
    flooring = round(100 * sum(s < 10, na.rm = TRUE) / sum(!is.na(s)), 1),
    ceiling = round(100 * sum(s > 90, na.rm = TRUE) / sum(!is.na(s)), 1)
  )
})

df_floorceiling2 <- do.call(rbind, out)
df_floorceiling2
