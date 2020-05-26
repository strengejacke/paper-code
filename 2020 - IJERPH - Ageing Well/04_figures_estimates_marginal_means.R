library(ggplot2)
library(ggeffects)


# Grafik Modelle 3 und 4 ---------------------------

create_plot <- function(emm, model = "model3", y_title = NULL, legend_title = NULL, title = NULL, small_font_size = 7.5, large_font_size = 8.5, width = 2.5, height = 3) {
  ggplot(emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, colour = group, fill = group)) +
    geom_ribbon(alpha = .2, colour = NA) +
    geom_line() +
    theme_modern(
      plot.title.space = 5,
      plot.title.size = large_font_size * 1.1,
      axis.title.space = 0
    ) +
    theme(
      axis.text = element_text(size = small_font_size),
      axis.title = element_text(size = large_font_size),
      legend.text = element_text(size = small_font_size),
      legend.title = element_text(size = large_font_size),
      title = element_text(size = large_font_size * 1.1)
    ) +
    scale_x_continuous(breaks = 1:4, labels = c("2008", "2011", "2014", "2017")) +
    scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8"), guide = "none") +
    scale_fill_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
    labs(y = NULL, x = NULL, color = NULL, title = title, fill = legend_title) +
    theme(legend.position = "bottom", legend.key.size = unit(.3, "cm"))

  ggsave(sprintf("%s_%s.tiff", model, title), scale = .85, device = "tiff", width = width, height = height, dpi = 600, compression = "lzw")
}




# Estimated Marginal Means Model 3 ------------------------------

m1 <- ggemmeans(model3, c("welle [1:4 by=.2]", "isced"))
m2 <- ggemmeans(model3, c("welle [1:4 by=.2]", "aee_oecd_between_z2"))
m3 <- ggemmeans(model3, c("welle [1:4 by=.2]", "lone6_between_z2"))

m1$Model = "ISCED"
levels(m1$group) <- c("low", "middle", "high")
m2$Model = "Income"
levels(m2$group) <- c("-1 SD", "Mean", "+1 SD")
m3$Model = "Loneliness"
levels(m3$group) <- c("-1 SD", "Mean", "+1 SD")

create_plot(m1, title = "(a) PF and Education")
create_plot(m2, title = "(b) PF and Income")
create_plot(m3, title = "(c) PF and Loneliness")



# Estimated Marginal Means Model 4 ------------------------------

m1 <- ggemmeans(model4, c("welle [1:4 by=.2]", "isced"))
m2 <- ggemmeans(model4, c("welle [1:4 by=.2]", "aee_oecd_between_z2"))
m3 <- ggemmeans(model4, c("welle [1:4 by=.2]", "lone6_between_z2"))

m1$Model = "ISCED"
levels(m1$group) <- c("low", "middle", "high")
m2$Model = "Income"
levels(m2$group) <- c("-1 SD", "Mean", "+1 SD")
m3$Model = "Loneliness"
levels(m3$group) <- c("-1 SD", "Mean", "+1 SD")

create_plot(m1, model = "model4", title = "(a) Optimism and Education")
create_plot(m2, model = "model4", title = "(b) Optimism and Income")
create_plot(m3, model = "model4", title = "(c) Optimism and Loneliness")
