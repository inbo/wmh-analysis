### GENERATE FIG 2: dependencies of annual timing

p1 <- ggplot(data = full, aes(y = arr.aut.yday, x = dep.aut.yday, group = pop, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(250, 310) +
  xlab("Departure DOY") +
  ylab("Arrival DOY")

p3 <- ggplot(data = full, aes(y = arr.aut.yday, x = stop.days.aut, group = pop, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(250, 310) +
  xlab("Nr. stop-over days") +
  ylab("Arrival DOY")

p5 <- ggplot(data = full, aes(y = aut.dur, x = stop.days.aut, group = pop, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Duration migration") +
  xlab("Nr. stop-over days")

p7 <- ggplot(data = full, aes(y = stop.days.aut, x = dep.aut.yday, group = pop, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Nr. stopover days") +
  xlab("Departure DOY")

p2 <- ggplot(data = full, aes(y = arr.spr.yday, x = dep.spr.yday, group = pop, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(85, 125) +
  xlab("Departure DOY") +
  ylab("Arrival DOY")

p4 <- ggplot(data = full, aes(y = arr.spr.yday, x = stop.days.spr, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x, aes(group = pop)) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(85, 125) +
  xlab("Nr. stop-over days") +
  ylab("Arrival DOY")

p6 <- ggplot(data = full, aes(y = spr.dur, x = stop.days.spr, group = pop, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Duration migration") +
  xlab("Nr. stop-over days")

p8 <- ggplot(data = full, aes(y = stop.days.spr, x = dep.spr.yday, group = pop, col = pop, fill = pop)) +
  # 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Nr. stopover days") +
  xlab("Departure DOY")


p <- grid.arrange(p1, p3, p5, p7, p2, p4, p6, p8, ncol = 4)
ggsave(plot = p, here::here("reports", "figures", "Fig2_CorrelationsInTiming.tiff"), dpi = 300, width = 8, height = 4)


#### TABLE S5: MODEL RELATIONSHIPS OF ARRIVAL TIMING VS DEPARTURE AND STOP DAYS (REPEAT ONCE PER SEASON)

ss <- full

m0i <- lmer(arr.spr.yday ~ dep.spr.yday * stop.days.spr + pop + (1 | ID), data = ss, REML = FALSE)
m0h <- lmer(arr.spr.yday ~ dep.spr.yday * stop.days.spr + (1 | ID), data = ss, REML = FALSE)
m0g <- lmer(arr.spr.yday ~ dep.spr.yday + stop.days.spr + pop + (1 | ID), data = ss, REML = FALSE)
m0f <- lmer(arr.spr.yday ~ dep.spr.yday + stop.days.spr + (1 | ID), data = ss, REML = FALSE)
m0e <- lmer(arr.spr.yday ~ stop.days.spr + pop + (1 | ID), data = ss, REML = FALSE)
m0d <- lmer(arr.spr.yday ~ dep.spr.yday + pop + (1 | ID), data = ss, REML = FALSE)
m0c <- lmer(arr.spr.yday ~ dep.spr.yday + (1 | ID), data = ss, REML = FALSE)
m0b <- lmer(arr.spr.yday ~ stop.days.spr + (1 | ID), data = ss, REML = FALSE)
m0a <- lmer(arr.spr.yday ~ pop + (1 | ID), data = ss, REML = FALSE)
m0 <- lmer(arr.spr.yday ~ (1 | ID), data = ss, REML = FALSE)


(a2 <- anova(m0, m0a, m0b, m0c, m0d, m0e, m0f, m0g, m0h, m0i))

mods.full <- attributes(a2)$heading[3:12]
mods <- attributes(a2)$row.names

mods <- as.data.frame(cbind(mods.full, mods))

a2 <- as.data.frame(a2)

a <- cbind(a2, mods)

write_csv(a, here::here("reports", "tables", "TableS5_GLLM-Arrival-SPRING.csv"))


#### TABLE S6: MODEL RELATIONSHIP OF STOP DAYS WT DEPARTURE (REPEAT ONCE PER SEASON)

ss <- full

m0d <- lmer(stop.days.aut ~ dep.aut.yday * pop + (1 | ID), data = ss, REML = FALSE)
m0c <- lmer(stop.days.aut ~ dep.aut.yday + pop + (1 | ID), data = ss, REML = FALSE)
m0b <- lmer(stop.days.aut ~ pop + (1 | ID), data = ss, REML = FALSE)
m0a <- lmer(stop.days.aut ~ dep.aut.yday + (1 | ID), data = ss, REML = FALSE)
m0 <- lmer(stop.days.aut ~ (1 | ID), data = ss, REML = FALSE)

(a2 <- anova(m0, m0a, m0b, m0c, m0d))

mods.full <- attributes(a2)$heading[3:7]
mods <- attributes(a2)$row.names

mods <- as.data.frame(cbind(mods.full, mods))

a2 <- as.data.frame(a2)

a <- cbind(a2, mods)

write_csv(a, here::here("reports", "tables", "TableS6_GLLM-Stops-vs-Dep-AUTUMN.csv"))


####################################################################################
####################################################################################
####################################################################################
## OLD CODE: FIGURE 2 - PART 2 : STOPOVER VS XXX

# p1 <- ggplot(data=full,aes(y=stop.days.aut,x=dep.aut.yday, group=pop,col=pop,fill=pop)) +
# 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
# 		 geom_point()+ geom_smooth(method='glm',formula=y~x)+
# 		scale_fill_manual(values=c("LC"="blue","SW"="red"))+
# 		scale_colour_manual(values=c("LC"="blue","SW"="red"))+
# 		theme_bw() + theme(legend.position="none")+
# 		xlab("Departure DOY") + ylab("Nr. stop-over days")

# p3 <- ggplot(data=full,aes(y=stop.days.aut,x=tailwind.aut, group=pop,col=pop,fill=pop)) +
# 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
# 		 geom_point()+ geom_smooth(method='glm',formula=y~x)+
# 		scale_fill_manual(values=c("LC"="blue","SW"="red"))+
# 		scale_colour_manual(values=c("LC"="blue","SW"="red"))+
# 		theme_bw() + theme(legend.position="none")+
# 		xlab("Mean Tailwind") + ylab("Nr. stop-over days")

# p2 <- ggplot(data=full,aes(y=stop.days.spr,x=dep.spr.yday, group=pop,col=pop,fill=pop)) +
# 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
# 		 geom_point()+ geom_smooth(method='glm',formula=y~x)+
# 		scale_fill_manual(values=c("LC"="blue","SW"="red"))+
# 		scale_colour_manual(values=c("LC"="blue","SW"="red"))+
# 		theme_bw() + theme(legend.position="none")+
# 		xlab("Departure DOY") + ylab("Nr. stop-over days")

# p4 <- ggplot(data=full,aes(y=stop.days.spr,x=tailwind.spr, group=pop,col=pop,fill=pop)) +
# 		geom_smooth(method='glm',formula=y~x,aes(group=name),fill='grey80',linetype='dashed',size=.6)+
# 		 geom_point()+ geom_smooth(method='glm',formula=y~x)+
# 		scale_fill_manual(values=c("LC"="blue","SW"="red"))+
# 		scale_colour_manual(values=c("LC"="blue","SW"="red"))+
# 		theme_bw() + theme(legend.position="none")+
# 		xlab("Mean Tailwind") + ylab("Nr. stop-over days")

# p<-grid.arrange(p1,p2,p3,p4,ncol=2)
# ggsave(plot=p,'Fig2_StopDays-vs-dep-wind.tiff',dpi=300,width=6,height=5)

# ss <- full

# 	m0i <- lmer(stop.days.spr ~ dep.spr.yday * tailwind.spr + pop + (1|ID),data=ss,REML=FALSE)
# 	m0h <- lmer(stop.days.spr ~ dep.spr.yday * tailwind.spr + (1|ID),data=ss,REML=FALSE)
# 	m0g <- lmer(stop.days.spr ~ dep.spr.yday + tailwind.spr + pop + (1|ID),data=ss,REML=FALSE)
# 	m0f <- lmer(stop.days.spr ~ dep.spr.yday + tailwind.spr + (1|ID),data=ss,REML=FALSE)
# 	m0e <- lmer(stop.days.spr ~ tailwind.spr + pop + (1|ID),data=ss,REML=FALSE)
# 	m0d <- lmer(stop.days.spr ~ dep.spr.yday + pop + (1|ID),data=ss,REML=FALSE)
# 	m0c <- lmer(stop.days.spr ~ tailwind.spr + (1|ID),data=ss,REML=FALSE)
# 	m0b <- lmer(stop.days.spr ~ dep.spr.yday + (1|ID),data=ss,REML=FALSE)
# 	m0a <- lmer(stop.days.spr ~ pop + (1|ID),data=ss,REML=FALSE)
# 	m0 <- lmer(stop.days.spr ~ (1|ID),data=ss,REML=FALSE)
# 	(a2 <- anova(m0,m0a,m0b,m0c,m0d,m0e,m0f,m0g,m0h,m0i))
# 	mods.full <- attributes(a2)$heading[3:12]
# 	mods <- attributes(a2)$row.names
# 	mods <- as.data.frame(cbind(mods.full,mods))
# 	a2 <- as.data.frame(a2)
# 	a <- cbind(a2,mods)
# 	write.csv(a,'Fig2B-Table_GLLM-NrStopOverDays-SPRING.csv')
