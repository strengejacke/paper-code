library(easystats)

# Setup - load data
fab_fam <- data_read("famber_fabel.rds")
fab_fam <- reverse_scale(fab_fam, select = 8:10)

# PCA - how many components suggested?
n_components(fab_fam) |> plot()


# Table 2: Factor structure of the FaBel questionnaire by factor loadings
# -----------------------------------------------------------------------

pca <- principal_components(fab_fam, n = 4, rotation = "varimax")
comp <- as.vector(closest_component(pca))

pca <- data_relocate(pca, "RC3", before = "RC4")
comp <- recode_values(comp, recode = list(`1` = 1, `2` = 3, `3` = 2, `4` = 4))

print_html(pca[order(comp), ])
pca
summary(pca)


# Table 3: Characteristics and reliability of the FaBel-20 subscales and total score
# ----------------------------------------------------------------------------------

# all 5 subscales
fab_fam18 <- data_read("famber_fabel18.rds")
comp18 <- c(comp, c(5, 5, 5))

# check correct assignment
cbind(colnames(fab_fam18), comp18)

# reverse scales
fab_fam18 <- reverse_scale(fab_fam18, select = 8:10)

# create subscales
scale1_family <- row_means(fab_fam18, select = which(comp18 == 1), min_valid = 3)
scale2_financial <- row_means(fab_fam18, select = which(comp18 == 2), min_valid = 2)
scale3_personal <- row_means(fab_fam18, select = which(comp18 == 3), min_valid = 2)
scale4_coping <- row_means(fab_fam18, select = which(comp18 == 4), min_valid = 2)
scale5_sibling <- row_means(fab_fam18, select = which(comp18 == 5), min_valid = 2)
total_score <- row_means(fab_fam18, min_valid = 10)

d <- data.frame(
  family = scale1_family,
  financial = scale2_financial,
  personal = scale3_personal,
  coping = scale4_coping,
  sibling = scale5_sibling,
  total = total_score
)

# missing values, mean and sd for subscales
data_codebook(d)
describe_distribution(d) |> print_html()

# Cronbach's Alpha for subscales
fab_fam18 |>
  data_select(select = which(comp18 == 1)) |>
  cronbachs_alpha()

fab_fam18 |>
  data_select(select = which(comp18 == 2)) |>
  cronbachs_alpha()

fab_fam18 |>
  data_select(select = which(comp18 == 3)) |>
  cronbachs_alpha()

fab_fam18 |>
  data_select(select = which(comp18 == 4)) |>
  cronbachs_alpha()

fab_fam18 |>
  data_select(select = which(comp18 == 5)) |>
  cronbachs_alpha()

# Total score
cronbachs_alpha(fab_fam18)



# flooring / ceiling effects

# score should range from 0 to 3
fab_fam18_0_3 <- slide(fab_fam18)

fabel_scores <- data.frame(
  scale1_family = row_sums(fab_fam18_0_3, select = which(comp18 == 1), min_valid = 3),
  scale2_financial = row_sums(fab_fam18_0_3, select = which(comp18 == 2), min_valid = 2),
  scale3_personal = row_sums(fab_fam18_0_3, select = which(comp18 == 3), min_valid = 2),
  scale4_coping = row_sums(fab_fam18_0_3, select = which(comp18 == 4), min_valid = 2),
  scale5_sibling = row_sums(fab_fam18_0_3, select = which(comp18 == 5), min_valid = 2),
  total_score = row_sums(fab_fam18_0_3, min_valid = 10)
)

# rescale all sub-scales from 0 to 100
fabel_scores <- rescale(fabel_scores)

# calculate flooring and ceiling
out <- lapply(fabel_scores, function(i) {
  data.frame(
    flooring = round(100 * sum(i < 10, na.rm = TRUE) / sum(!is.na(i)), 1),
    ceiling = round(100 * sum(i > 90, na.rm = TRUE) / sum(!is.na(i)), 1)
  )
})

df_floorceiling <- do.call(rbind, out)
df_floorceiling


# Internal check - alpha if deleted
# ---------------------------------

check <- check_itemscale(fab_fam, setNames(comp, colnames(fab_fam)))
print_html(check)

check <- check_itemscale(fab_fam18, setNames(comp18, colnames(fab_fam18)))
print_html(check)



# Table 4:  Intercorrelation of the subscales (Cronbach’s alpha in parenthesis)
# -----------------------------------------------------------------------------

correlation::correlation(d) |> summary()


# Table 5: Known-group validity of the FaBel subscales and total score
# --------------------------------------------------------------------

# known-group validity
fd <- data_read("fab_known_group.rds")

# predictors
preds <- c("Sex", "Education", "MaritalStatus", "Employment", "CareLevel", "Disability")
outcomes <- c(
  "scale1_family",
  "scale2_financial",
  "scale3_personal",
  "scale4_coping",
  "scale5_sibling",
  "total_score"
)

# known-group validity
# anova for categorical variables
# eta squared as effect size

result <- lapply(outcomes, function(dv) {
  sapply(preds, function(iv) {
    f <- as.formula(paste(dv, "~", iv))
    out <- parameters::model_parameters(
      anova(lm(f, data = fd)),
      es_type = "eta"
    )
    paste0(
      insight::format_value(sqrt(out$Eta2[1]), digits = 2),
      insight::format_p(out$p[1], stars_only = TRUE)
    )
  })
})

do.call(rbind, result) |>
  as.data.frame() |>
  data_transpose() |>
  export_table(column_names = outcomes, format = "html")


# known-group validity
# correlation for numerical variables
# pearson's r as effect size

preds <- c("AgeParent", "AgeChild")

result <- lapply(outcomes, function(dv) {
  sapply(preds, function(iv) {
    out <- correlation::correlation(fd, select = c(dv, iv))
    paste0(
      insight::format_value(out$r * -1, digits = 2, zap_small = TRUE),
      insight::format_p(out$p, stars_only = TRUE)
    )
  })
})

do.call(rbind, result) |>
  as.data.frame() |>
  data_transpose() |>
  export_table(column_names = outcomes, format = "html")
