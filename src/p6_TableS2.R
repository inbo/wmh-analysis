### COMPARE MEAN ARRIVAL, DEPARTURE, STOP DAYS, TOT DURATION BETWEEN SW AND LC HARRIERS
## to do this we use mixed model accounting for random individual intercepts
m0 <- lmer(arr.aut.yday ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx1 <- as.data.frame(posthoc[[2]])
xx1$model <- "arr.aut"

m0 <- lmer(arr.spr.yday ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx2 <- as.data.frame(posthoc[[2]])
xx2$model <- "arr.spr"

m0 <- lmer(dep.aut.yday ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx3 <- as.data.frame(posthoc[[2]])
xx3$model <- "dep.aut"

m0 <- lmer(dep.spr.yday ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx4 <- as.data.frame(posthoc[[2]])
xx4$model <- "dep.spr"

m0 <- lmer(stop.days.aut ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx5 <- as.data.frame(posthoc[[2]])
xx5$model <- "stop.days.aut"

m0 <- lmer(stop.days.spr ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx6 <- as.data.frame(posthoc[[2]])
xx6$model <- "stop.days.spr"

m0 <- lmer(aut.dur ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx7 <- as.data.frame(posthoc[[2]])
xx7$model <- "dur.aut"

m0 <- lmer(spr.dur ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx8 <- as.data.frame(posthoc[[2]])
xx8$model <- "dur.spr"

m0 <- lmer(tailwind.aut ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx9 <- as.data.frame(posthoc[[2]])
xx9$model <- "tail.aut"

m0 <- lmer(tailwind.spr ~ pop + (1 | ID), data = full, REML = FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
xx10 <- as.data.frame(posthoc[[2]])
xx10$model <- "tail.spr"

xx <- rbind(xx1, xx2, xx3, xx4, xx5, xx6, xx7, xx8, xx9, xx10)
write.csv(xx, "TableS2_GroupDifferencesMixedAnova.csv")
