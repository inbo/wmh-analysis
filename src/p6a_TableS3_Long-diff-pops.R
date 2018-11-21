### TABLE S3: DETERMINE OVERLAP OF MIGRATION CORRIDORS LC AND SW HARRIERS STATISTICALLY
## To do this we compare longitude in each latitudinal band of 5 degrees, using a mixed model
## accounting for randomly varying intercepts

library(emmeans)

### STEP 1: aggregate longitudes per latitudinal band and season
data$zone <- round(data$lat / 5) * 5

ss25 <- subset(data,data$zone == 25)
ss25.spr <- subset(ss25,ss25$season == "Spring")
ss25.aut <- subset(ss25,ss25$season == "Autumn")

ss35 <- subset(data,data$zone == 35)
ss35.spr <- subset(ss35,ss35$season == "Spring")
ss35.aut <- subset(ss35,ss35$season == "Autumn")

ss45 <- subset(data,data$zone == 45)
ss45.spr <- subset(ss45,ss45$season == "Spring")
ss45.aut <- subset(ss45,ss45$season == "Autumn")

ss55 <- subset(data,data$zone == 55)
ss55.spr <- subset(ss55,ss55$season == "Spring")
ss55.aut <- subset(ss55,ss55$season == "Autumn")

longs.25.spr <- aggregate(ss25.spr$long,by=list(ss25.spr$pop,ss25.spr$ID,ss25.spr$yr),FUN='na.mean')
colnames(longs.25.spr)[1:4] <- c("pop","ID","yr","long.25.spr")

longs.35.spr <- aggregate(ss35.spr$long,by=list(ss35.spr$pop,ss35.spr$ID,ss35.spr$yr),FUN='na.mean')
colnames(longs.35.spr)[1:4] <- c("pop","ID","yr","long.35.spr")

longs.45.spr <- aggregate(ss45.spr$long,by=list(ss45.spr$pop,ss45.spr$ID,ss45.spr$yr),FUN='na.mean')
colnames(longs.45.spr)[1:4] <- c("pop","ID","yr","long.45.spr")

longs.55.spr <- aggregate(ss55.spr$long,by=list(ss55.spr$pop,ss55.spr$ID,ss55.spr$yr),FUN='na.mean')
colnames(longs.55.spr)[1:4] <- c("pop","ID","yr","long.55.spr")

longs.25.aut <- aggregate(ss25.aut$long,by=list(ss25.aut$pop,ss25.aut$ID,ss25.aut$yr),FUN='na.mean')
colnames(longs.25.aut)[1:4] <- c("pop","ID","yr","long.25.aut")

longs.35.aut <- aggregate(ss35.aut$long,by=list(ss35.aut$pop,ss35.aut$ID,ss35.aut$yr),FUN='na.mean')
colnames(longs.35.aut)[1:4] <- c("pop","ID","yr","long.35.aut")

longs.45.aut <- aggregate(ss45.aut$long,by=list(ss45.aut$pop,ss45.aut$ID,ss45.aut$yr),FUN='na.mean')
colnames(longs.45.aut)[1:4] <- c("pop","ID","yr","long.45.aut")

longs.55.aut <- aggregate(ss55.aut$long,by=list(ss55.aut$pop,ss55.aut$ID,ss55.aut$yr),FUN='na.mean')
colnames(longs.55.aut)[1:4] <- c("pop","ID","yr","long.55.aut")

### STEP2: compile the above information into a useful df 
longfull <- merge(longs.25.spr,longs.35.spr,all.x=T)
longfull <- merge(longfull,longs.45.spr,all.x=T)
longfull <- merge(longfull,longs.55.spr,all.x=T)
longfull <- merge(longfull,longs.25.aut,all.x=T)
longfull <- merge(longfull,longs.35.aut,all.x=T)
longfull <- merge(longfull,longs.45.aut,all.x=T)
longfull <- merge(longfull,longs.55.aut,all.x=T)

### STEP 3: RUN MIXED MODELS FOR COMPARING LONGITUDES OF GROUPS
m0 <- lmer(long.25.spr ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.25.spr <- as.data.frame(posthoc[[2]])
long.25.spr$model <- "long.25.spr"

m0 <- lmer(long.35.spr ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.35.spr <- as.data.frame(posthoc[[2]])
long.35.spr$model <- "long.35.spr"

m0 <- lmer(long.45.spr ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.45.spr <- as.data.frame(posthoc[[2]])
long.45.spr$model <- "long.45.spr"

m0 <- lmer(long.55.spr ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.55.spr <- as.data.frame(posthoc[[2]])
long.55.spr$model <- "long.55.spr"

m0 <- lmer(long.25.aut ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.25.aut <- as.data.frame(posthoc[[2]])
long.25.aut$model <- "long.25.aut"

m0 <- lmer(long.35.aut ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.35.aut <- as.data.frame(posthoc[[2]])
long.35.aut$model <- "long.35.aut"

m0 <- lmer(long.45.aut ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.45.aut <- as.data.frame(posthoc[[2]])
long.45.aut$model <- "long.45.aut"

m0 <- lmer(long.55.aut ~ pop + (1|ID),data=longfull,REML=FALSE)
posthoc <- emmeans(m0, list(pairwise ~ pop), adjust = "tukey")
long.55.aut <- as.data.frame(posthoc[[2]])
long.55.aut$model <- "long.55.aut"


long.zones <- rbind(long.25.spr,long.35.spr,long.45.spr,long.55.spr,long.25.aut,long.35.aut,long.45.aut,long.55.aut)
write.csv(long.zones,'TableS3_PopDifferencesLongitudes_MixedAnova.csv')



