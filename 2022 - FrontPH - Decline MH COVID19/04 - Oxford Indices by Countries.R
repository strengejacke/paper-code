d <- share8
x <- d[!duplicated(d$country), ]
x$country <- factor(x$country, levels = x$country[order(x$mean_str_index)])

pdf("Oxford_Index.pdf", width = 7, height = 7, version = 1.7,
    paper = "a4")

ggplot(x, aes(x = mean_str_index, y = country)) +
  geom_point() +
  labs(x = "Stringency Index", x = NULL, title = "Covid Stringency Index by Country")

x <- d[!duplicated(d$country), ]
x$country <- factor(x$country, levels = x$country[order(x$mean_health_index)])

ggplot(x, aes(x = mean_health_index, y = country)) +
  geom_point() +
  labs(x = "Containment and Health Index", x = NULL, title = "Covid Containment and Health Index by Country")

x <- d[!duplicated(d$country), ]
x$country <- factor(x$country, levels = x$country[order(x$mean_gov_resp_index)])

ggplot(x, aes(x = mean_gov_resp_index, y = country)) +
  geom_point() +
  labs(x = "Government Response Index", x = NULL, title = "Covid Government Response Index by Country")

dev.off()



qunt <- quantile(d$mean_str_index, c(.2, .4, .6, .8))
d$covid_regime <- NA
d$covid_regime[d$mean_str_index <= qunt[1]] <- "very low"
d$covid_regime[d$mean_str_index > qunt[1] & d$mean_str_index <= qunt[2]] <- "low"
d$covid_regime[d$mean_str_index > qunt[2] & d$mean_str_index <= qunt[3]] <- "middle"
d$covid_regime[d$mean_str_index > qunt[3] & d$mean_str_index <= qunt[4]] <- "high"
d$covid_regime[d$mean_str_index > qunt[4]] <- "very high"

