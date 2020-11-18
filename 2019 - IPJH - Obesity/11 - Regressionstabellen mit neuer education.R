# score 1: socio-cultural ----

model1 <- lm(
  score1 ~ I(q101/10) + q100 +  education + MIG + bmi + country + VigMW + VigDTM + VigB,
  data = ego,
  weights = gewicht
)

model2 <- lm(
  score1 ~ I(q101 / 10) + q100 +  education + MIG + bmi + country * VigMW + country * VigDTM + country * VigB,
  data = ego,
  weights = gewicht
)

tab_model(model1, model2, digits = 2)


# score 2: behavior ----

model3 <- lm(
  score2 ~ I(q101/10) + q100 +  education + MIG + bmi + country + VigMW + VigDTM + VigB,
  data = ego,
  weights = gewicht
)

model4 <- lm(
  score2 ~ I(q101 / 10) + q100 +  education + MIG + bmi + country * VigMW + country * VigDTM + country * VigB,
  data = ego,
  weights = gewicht
)

tab_model(model3, model4, digits = 2)


# score 3: somatic-psychological ----

model5 <- lm(
  score3 ~ I(q101/10) + q100 +  education + MIG + bmi + country + VigMW + VigDTM + VigB,
  data = ego,
  weights = gewicht
)

model6 <- lm(
  score3 ~ I(q101 / 10) + q100 +  education + MIG + bmi + country * VigMW + country * VigDTM + country * VigB,
  data = ego,
  weights = gewicht
)

tab_model(model5, model6, digits = 2)


# score 4: SES ----

model7 <- lm(
  score4 ~ I(q101/10) + q100 +  education + MIG + bmi + country + VigMW + VigDTM + VigB,
  data = ego,
  weights = gewicht
)

model8 <- lm(
  score4 ~ I(q101 / 10) + q100 +  education + MIG + bmi + country * VigMW + country * VigDTM + country * VigB,
  data = ego,
  weights = gewicht
)

tab_model(model7, model8, digits = 2)
