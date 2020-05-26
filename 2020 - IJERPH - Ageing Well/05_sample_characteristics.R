library(report)
library(sjmisc)
library(sjPlot)

deas_gesamt_complete$ehramt_weit_f <- as.factor(deas_gesamt_complete$ehramt_weit)

x <- report::report_sample(
  sjmisc::to_label(deas_gesamt_complete, erw, migrat_r, isced, gender, ehramt_weit, partner),
  group_by = "welle",
  select = c("sf36", "optimismus100", "isced", "migrat_r", "gender", "alter_aktuell", "partner", "ehramt_weit_f", "aee_oecd", "lone6", "srh"),
  weights = "drop_weight",
  centrality = "mean"
)

sjPlot::tab_df(x)

