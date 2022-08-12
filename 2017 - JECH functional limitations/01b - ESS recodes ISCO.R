# -----------------------------------------------
# Berufsstaus, ISEI
# -----------------------------------------------
source("isco88_to_isco08.R")
source("isco08_to_isei.R")

ess5$isco08 <- isco88_to_isco08(ess5$isco08)
ess4$isco08 <- isco88_to_isco08(ess4$isco08)
ess3$isco08 <- isco88_to_isco08(ess3$isco08)
ess2$isco08 <- isco88_to_isco08(ess2$isco08)
ess1$isco08 <- isco88_to_isco08(ess1$isco08)

set_label(ess5$isco08) <- "Occupation, ISCO08"
set_label(ess4$isco08) <- "Occupation, ISCO08"
set_label(ess3$isco08) <- "Occupation, ISCO08"
set_label(ess2$isco08) <- "Occupation, ISCO08"
set_label(ess1$isco08) <- "Occupation, ISCO08"


ess7$isei <- round(isco08_to_isei(ess7$isco08))
ess6$isei <- round(isco08_to_isei(ess6$isco08))
ess5$isei <- round(isco08_to_isei(ess5$isco08))
ess4$isei <- round(isco08_to_isei(ess4$isco08))
ess3$isei <- round(isco08_to_isei(ess3$isco08))
ess2$isei <- round(isco08_to_isei(ess2$isco08))
ess1$isei <- round(isco08_to_isei(ess1$isco08))


set_label(ess7$isei) <- "International Socio-Economic Index (ISEI)"
set_label(ess6$isei) <- "International Socio-Economic Index (ISEI)"
set_label(ess5$isei) <- "International Socio-Economic Index (ISEI)"
set_label(ess4$isei) <- "International Socio-Economic Index (ISEI)"
set_label(ess3$isei) <- "International Socio-Economic Index (ISEI)"
set_label(ess2$isei) <- "International Socio-Economic Index (ISEI)"
set_label(ess1$isei) <- "International Socio-Economic Index (ISEI)"


set_labels(ess7$isei) <- ""
set_labels(ess6$isei) <- ""
set_labels(ess5$isei) <- ""
set_labels(ess4$isei) <- ""
set_labels(ess3$isei) <- ""
set_labels(ess2$isei) <- ""
set_labels(ess1$isei) <- ""

# Ganzeboom, Harry B.G.; Treiman, Donald J. (2003). "Three Internationally Standardised Measures for
# Comparative Research on Occupational Status." Pp. 159-193 in Jürgen H.P. Hoffmeyer-Zlotnik & Christof
# Wolf (Eds.), Advances in Cross-National Comparison. A European Working Book for Demographic and Socio-Economic
# Variables. New York: Kluwer Academic Press.

# Harry B.G. Ganzeboom, Paul M. De Graaf, Donald J. Treiman, A standard international socio-economic
# index of occupational status, Social Science Research, Volume 21, Issue 1, March 1992,
# Pages 1-56, ISSN 0049-089X, http://dx.doi.org/10.1016/0049-089X(92)90017-B.
# (http://www.sciencedirect.com/science/article/pii/0049089X9290017B)
