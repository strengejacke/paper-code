library(sjmisc)
library(sjlabelled)
library(tidyverse)


# -----------------------------------------------
# Daten laden
# -----------------------------------------------
load("R-Datensätze/varnames.RData")
ess7 <- read_spss("../ESS_Data/ESS7e02.sav")
ess7 <- ess7 %>% select(which(colnames(ess7) %in% varnames_reduced))
ess6 <- read_spss("../ESS_Data/ESS6e02_2.sav")
ess6 <- ess6 %>% select(which(colnames(ess6) %in% varnames_reduced))
ess5 <- read_spss("../ESS_Data/ESS5e03_2.sav")
ess5 <- ess5 %>% select(which(colnames(ess5) %in% varnames_reduced))
ess4 <- read_spss("../ESS_Data/ESS4e04_3.sav")
ess4 <- ess4 %>% select(which(colnames(ess4) %in% varnames_reduced))
ess3 <- read_spss("../ESS_Data/ESS3e03_5.sav")
ess3 <- ess3 %>% select(which(colnames(ess3) %in% varnames_reduced))
ess2 <- read_spss("../ESS_Data/ESS2e03_4.sav")
ess2 <- ess2 %>% select(which(colnames(ess2) %in% varnames_reduced))
ess1 <- read_spss("../ESS_Data/ESS1e06_4.sav")
ess1 <- ess1 %>% select(which(colnames(ess1) %in% varnames_reduced))

ess7$country <- to_label(ess7$cntry)
ess6$country <- to_label(ess6$cntry)
ess5$country <- to_label(ess5$cntry)
ess4$country <- to_label(ess4$cntry)
ess3$country <- to_label(ess3$cntry)
ess2$country <- to_label(ess2$cntry)
ess1$country <- to_label(ess1$cntry)


# Variablennamen anpassen
ess5 <- ess5 %>% rename(icpdwrk = icmnact, isco08 = iscoco)
ess4 <- ess4 %>% rename(isco08 = iscoco, edulvlb = edulvla)
ess3 <- ess3 %>% rename(isco08 = iscoco, edulvlb = edulvla, hinctnta = hinctnt)
ess2 <- ess2 %>% rename(isco08 = iscoco, edulvlb = edulvla, hinctnta = hinctnt)
ess1 <- ess1 %>% rename(isco08 = iscoco, edulvlb = edulvla, hinctnta = hinctnt)


# Nur Alter60+
ess7 <- ess7 %>% filter(agea >= 60)
ess6 <- ess6 %>% filter(agea >= 60)
ess5 <- ess5 %>% filter(agea >= 60)
ess4 <- ess4 %>% filter(agea >= 60)
ess3 <- ess3 %>% filter(agea >= 60)
ess2 <- ess2 %>% filter(agea >= 60)
ess1 <- ess1 %>% filter(agea >= 60)


# Unbenutzte label entfernen
ess7 <- drop_labels(ess7)
ess6 <- drop_labels(ess6)
ess5 <- drop_labels(ess5)
ess4 <- drop_labels(ess4)
ess3 <- drop_labels(ess3)
ess2 <- drop_labels(ess2)
ess1 <- drop_labels(ess1)


# rekodierungen
source("01b - ESS recodes ISCO.R")
source("01c - ESS recodes ISCED.R")
source("01d - ESS recodes Einkommen.R")


# gesamtdatensatz erstellen
ess <- merge_df(ess1, ess2, ess3, ess4, ess5, ess6, ess7)

# anzahl an ländern, die mind. x wellen mitgemacht haben
min_wave <- 7
# Länderkürzel von Ländern, die in allen Wellen mitgemacht haben
alle_laender <- apply(table(ess$cntry, ess$essround), 1, function(x) sum(x != 0) >= min_wave)
alle_laender <- names(alle_laender)[alle_laender]
# Nur die Länder, die überall mitgemacht haben
ess <- ess %>% filter(cntry %in% alle_laender)
# unbenutzte level/label löschen
ess$country <- droplevels(ess$country)
ess$cntry <- drop_labels(ess$cntry)

source("01e - ESS recodes verschiedene.R")
source("01f - Einkommensrange, in EURO.R")
