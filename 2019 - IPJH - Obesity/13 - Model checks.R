suppressMessages(suppressWarnings({
  library(tidyverse)
  library(strengejacke)
  library(easystats)
}))

load("Daten/ego.RData")

model1 <- lm(
  score1 ~ I(q101/10) + q100 +  education + MIG + bmi + country + VigMW + VigDTM + VigB,
  data = ego,
  weights = gewicht
)

p1 <- check_model(model1, panel = FALSE)
p2 <- check_normality(model1) %>% plot(type = "pp")

check_collinearity(model1)





pdf("check_model.pdf", width = 10, height = 12)
check_model(model1)
check_model(model2)
check_model(model3)
check_model(model4)
check_model(model5)
check_model(model6)
check_model(model7)
check_model(model8)
dev.off()


pdf("check_normality.pdf", width = 6, height = 6)
check_normality(model1) %>% plot(type = "qq")
check_normality(model1) %>% plot(type = "pp")
check_normality(model2) %>% plot(type = "qq")
check_normality(model2) %>% plot(type = "pp")
check_normality(model3) %>% plot(type = "qq")
check_normality(model3) %>% plot(type = "pp")
check_normality(model4) %>% plot(type = "qq")
check_normality(model4) %>% plot(type = "pp")
check_normality(model5) %>% plot(type = "qq")
check_normality(model5) %>% plot(type = "pp")
check_normality(model6) %>% plot(type = "qq")
check_normality(model6) %>% plot(type = "pp")
check_normality(model7) %>% plot(type = "qq")
check_normality(model7) %>% plot(type = "pp")
check_normality(model8) %>% plot(type = "qq")
check_normality(model8) %>% plot(type = "pp")
dev.off()

plot_frq(ego, score1:score4)
