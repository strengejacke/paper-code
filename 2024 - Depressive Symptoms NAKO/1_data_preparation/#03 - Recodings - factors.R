library(easystats)

# missings ----------------

nako$d_qol2 <- convert_to_na(nako$d_qol2, na = c(8, 9))



# atomic to factors ---------------------------------

convert_factors <- function(d, exclude = NULL) {
  message("Converting atomic to factors. Please wait...\n")
  d[] <- lapply(colnames(d), function(i) {
    x <- d[[i]]
    if (is.null(exclude) || !i %in% exclude) {
      labs <- attr(x, "labels", exact = TRUE)
      lab <- attr(x, "label", exact = TRUE)
      if ((is.atomic(x) || is.numeric(x) || is.integer(x)) && !is.null(labs) && length(labs) >= length(unique(stats::na.omit(x)))) {
        x <- as.factor(x)
        attr(x, "labels") <- labs
        if (!is.null(lab))
          attr(x, "label") <- lab
      }
    }
    x
  })

  d
}



nako <- convert_factors(
  nako,
  exclude = c("basis_ageclass", "a_ses_inc", "a_ses_isced97_level", "d_se_b1",
              "d_se_b1a", "d_se_bf1", "d_se_bf4", "d_se_bf7", "geb_region",
              "geb_region_vater", "geb_region_mutter", "d_se_n1a", "d_se_bf3a",
              "d_se_bf6a", "d_se_n5_1")
)

# speichern ------------------
# save(nako, nako_label, file = "Daten/nako.RData")
