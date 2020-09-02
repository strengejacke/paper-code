library(sjPlot)
library(parameters)


# I have avoided certain (busy) places  ----

m1 <- glm(
  avoidplaces ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m1, exponentiate = TRUE), select = "minimal")



# I have adapted my school or work situation ----

m2 <- glm(
  worksituation ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m2, exponentiate = TRUE), select = "minimal")



# I washed my hands more often and longer ----

m3 <- glm(
  handwashing ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m3, exponentiate = TRUE), select = "minimal")



# I have kept distance to other people (at least 1.5 meters) ----

m4 <- glm(
  keptdistance ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m4, exponentiate = TRUE), select = "minimal")



# I have quarantined myself, although I have no symptoms ----

m5 <- glm(
  quarantinenosymptoms ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m5, exponentiate = TRUE), select = "minimal")



# I used disinfectant ----

m6 <- glm(
  desinfectant ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m6, exponentiate = TRUE), select = "minimal")



# I have reduced personal meetings and contacts ----

m7 <- glm(
  reducesocial ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m7, exponentiate = TRUE), select = "minimal")



# I was wearing face masks ----

m8 <- glm(
  facemask ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m8, exponentiate = TRUE), select = "minimal")



# I have taken none of these measures ----

m9 <- glm(
  nomeasures ~ education + sex + alter + marstat + hh_groesse,
  data = d,
  weights = weight,
  family = binomial()
)

print(model_parameters(m9, exponentiate = TRUE), select = "minimal")




# complete summary tables ----
tab_model(m1, m2, m3)
tab_model(m4, m5, m6)
tab_model(m7, m8, m9)
