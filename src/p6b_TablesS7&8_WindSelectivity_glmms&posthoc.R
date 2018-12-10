### FIG3: compare wind conditions on travel vs non-travel days in each region and season

dayset$travel <- as.factor(ifelse(dayset$travel == 1,'travel','rest'))
dayset$pop <- as.factor(dayset$pop)

ggplot() + geom_boxplot(data=dayset,aes(y=tail,x=paste(pop,travel),fill=pop,linetype=travel))+
	facet_grid(continent~season)+
		scale_fill_manual(values=c("LC"="blue","SW"="red"))+
	theme_bw()+ ylab('Daily Tailwind [ms-1]')+ xlab("group") +
		theme(legend.position = 'none',
	  	legend.direction= 'horizontal',
		panel.border	= element_rect(colour='white'),
		strip.background	= element_rect(fill='white',colour='NA'),
		legend.title 	= element_text(size=12,face='bold'),
		legend.text 	= element_text(size=10,face='bold'),
		strip.text		= element_text(size=14,face='bold'),
		axis.text		= element_text(size=10,face='bold'),
		axis.title		= element_text(size=12,face='bold'))

ggsave(here('reports','figures','Fig3_WindSelectivity.tiff'),dpi=300,width=5,height=4.5)


# the following procedure needs to be repeated for every season and continent
sx <- subset(dayset,dayset$season == "Spring" & dayset$continent == "Africa")

	m0a <- lmer(tail ~ travel + (1|name),data=sx,REML=FALSE)
	m0b <- lmer(tail ~ pop + (1|name),data=sx,REML=FALSE)
	m0c <- lmer(tail ~ pop + travel + (1|name),data=sx,REML=FALSE)
	m0d <- lmer(tail ~ pop * travel + (1|name),data=sx,REML=FALSE)
	
	a1 <- anova(m0a,m0b,m0c,m0d)

	mods.full <- attributes(a1)$heading[3:6]
	mods <- attributes(a1)$row.names

	mods <- as.data.frame(cbind(mods.full,mods))

	a1 <- as.data.frame(a1)

	a <- cbind(a1,mods)

	write_csv(a, here('reports','tables','WindSelectivity_ModSel_SpringAfrica.csv'))
#this produces table S7
	
library(emmeans)
posthoc <- emmeans(m0d, list(pairwise ~ pop:travel), adjust = "tukey")
xx <- as.data.frame(posthoc[[2]])
write_csv(xx, here('reports','tables','WindSelectivity_POSTHOC_SpringAFrica.csv'))
# this produces Table S8