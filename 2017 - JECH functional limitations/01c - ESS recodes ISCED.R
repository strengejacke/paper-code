eisced_value_labels <- c("low (lower/upper secondary)", "mid (post-secondary)", "high (tertiary)")

ess7$education <- rec(ess7$eisced, recodes = "1:2=0;3:5=1;6:7=2;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)", val.labels = eisced_value_labels)

ess6$education <- rec(ess6$eisced, recodes = "1:2=0;3:5=1;6:7=2;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)", val.labels = eisced_value_labels)

ess5$education <- rec(ess5$eisced, recodes = "1:2=0;3:5=1;6:7=2;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)", val.labels = eisced_value_labels)

ess4$education <- rec(ess4$eisced, recodes = "1:2=0;3:5=1;6:7=2;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)", val.labels = eisced_value_labels)

ess3$education <- rec(ess3$eisced, recodes = "1:2=0;3:5=1;6:7=2;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)", val.labels = eisced_value_labels)

ess2$education <- rec(ess2$eisced, recodes = "1:2=0;3:5=1;6:7=2;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)", val.labels = eisced_value_labels)

ess1$education <- rec(ess1$eisced, recodes = "1:2=0;3:5=1;6:7=2;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)", val.labels = eisced_value_labels)

ess4$education[which(ess4$eisced == 0 & ess4$edulvlb %in% c(1, 2))] <- 0
ess4$education[which(ess4$eisced == 0 & ess4$edulvlb %in% c(3, 4))] <- 1
ess4$education[which(ess4$eisced == 0 & ess4$edulvlb == 5)] <- 2

ess3$education[which(ess3$eisced == 0 & ess3$edulvlb %in% c(1, 2))] <- 0
ess3$education[which(ess3$eisced == 0 & ess3$edulvlb %in% c(3, 4))] <- 1
ess3$education[which(ess3$eisced == 0 & ess3$edulvlb == 5)] <- 2

ess2$education[which(ess2$eisced == 0 & ess2$edulvlb %in% c(1, 2))] <- 0
ess2$education[which(ess2$eisced == 0 & ess2$edulvlb %in% c(3, 4))] <- 1
ess2$education[which(ess2$eisced == 0 & ess2$edulvlb == 5)] <- 2

ess1$education[which(ess1$eisced == 0 & ess1$edulvlb %in% c(1, 2))] <- 0
ess1$education[which(ess1$eisced == 0 & ess1$edulvlb %in% c(3, 4))] <- 1
ess1$education[which(ess1$eisced == 0 & ess1$edulvlb == 5)] <- 2



ess7$education_d <- rec(ess7$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)")

ess6$education_d <- rec(ess6$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)")

ess5$education_d <- rec(ess5$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)")

ess4$education_d <- rec(ess4$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)")

ess3$education_d <- rec(ess3$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)")

ess2$education_d <- rec(ess2$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)")

ess1$education_d <- rec(ess1$eisced, recodes = "1:4=0;5:7=1;else=NA", to.factor = TRUE,
                      var.label = "Educational level (ISCED)")

ess4$education_d[which(ess4$eisced == 0 & ess4$edulvlb %in% c(1, 2, 3))] <- 0
ess4$education_d[which(ess4$eisced == 0 & ess4$edulvlb %in% c(4, 5))] <- 1

ess3$education_d[which(ess3$eisced == 0 & ess3$edulvlb %in% c(1, 2, 3))] <- 0
ess3$education_d[which(ess3$eisced == 0 & ess3$edulvlb %in% c(4, 5))] <- 1

ess2$education_d[which(ess2$eisced == 0 & ess2$edulvlb %in% c(1, 2, 3))] <- 0
ess2$education_d[which(ess2$eisced == 0 & ess2$edulvlb %in% c(4, 5))] <- 1

ess1$education_d[which(ess1$eisced == 0 & ess1$edulvlb %in% c(1, 2, 3))] <- 0
ess1$education_d[which(ess1$eisced == 0 & ess1$edulvlb %in% c(4, 5))] <- 1
