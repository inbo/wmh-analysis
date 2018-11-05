### I previously extracted all wind data at morning, noon and evening of each day (cf Klaassen et al 2010)
### Read wind data and calculate mean daily wind components
wind <- read.csv("data/weather.csv")
wind <- wind[,c("indday","tag2","u","v")]

wind.6 <- wind[which(wind$tag2 == 6),]
wind.12 <- wind[which(wind$tag2 == 12),]
wind.18 <- wind[which(wind$tag2 == 18),]

colnames(wind.6)[3:4] <- c("u.6","v.6")
colnames(wind.12)[3:4] <- c("u.12","v.12")
colnames(wind.18)[3:4] <- c("u.18","v.18")

winds <- merge(wind.6[,c("indday","u.6","v.6")],wind.12[,c("indday","u.12","v.12")],all.x=T)
winds <- merge(winds,wind.18[,c("indday","u.18","v.18")],all.x=T)

winds$u <- (winds$u.6 + 2*winds$u.12 + winds$u.18)/4
winds$v <- (winds$v.6 + 2*winds$v.12 + winds$v.18)/4

winds <- winds[,c("indday","u","v")]

### Calculate wind speed and direction relative to north
winds$w <- (winds$u^2 + winds$v^2)^(1/2)			# determine strength of wind in m/s #
winds$alfa <- atan2(winds$u/winds$w,winds$v/winds$w)*(180/pi)
winds$c_alfa <- circular(winds$alfa, units = 'degrees', template ='geographics')

### Merge wind data and daily performance statistics
dayset <- unique(migration[,c("indday","date","name","ID","season","type2","travel","d.range","d.obs","daily.dist","daily.dir","continent","pop")])
dayset <- merge(dayset,winds,all.x=T)

### Make sure directions are on scale of 0 - 360 degrees (rather than -180/180)
dayset$alfa2 <- ifelse(dayset$alfa < 0,dayset$alfa + 360, dayset$alfa)
dayset$daily.dir2 <- ifelse(dayset$daily.dir < 0,dayset$daily.dir + 360, dayset$daily.dir)

### Calculate regionally prefered direction as mean of daily travel dirs 
regdirs <- aggregate(dayset[which(dayset$travel == 1),]$daily.dir2,by=list(dayset[which(dayset$travel == 1),]$pop,dayset[which(dayset$travel == 1),]$season),FUN='median')
colnames(regdirs) <- c("pop","season","reg.dir")

dayset <- merge(dayset,regdirs,all.x=T)

### Calculate deviation between wind and regionally preferred travel direction 
dayset$angle <- dayset$alfa2 - dayset$reg.dir 
dayset$angle <- ifelse(dayset$angle < 0, dayset$angle + 360, dayset$angle)
dayset$angle <- ifelse(dayset$angle > 180, dayset$angle - 360, dayset$angle)

### Determine U and V wind relative to regionally preferred direction for level 925 hPa
dayset$cross <- as.numeric(dayset$w * sin(rad(dayset$angle))) # TRANSFORM ANGLE TO RADIANS IN ORDER TO APPLY SIN() AND COS()
dayset$tail <- as.numeric(dayset$w * cos(rad(dayset$angle)))

### Ignore point over sea (we compare flight behaviour over land between Eur and AFr)
dayset <- subset(dayset,dayset$continent %in% c("Europe","Africa"))

### OLD CODE: determine daily cumulative interruption time and cumulative travel time
#d.travel <- aggregate(data[which(data$behav2 %in% c("travel [< 25]","travel [>= 25]")),]$dur,by=list(data[which(data$behav2 %in% c("travel [< 25]","travel [>= 25]")),]$indday),FUN='sum')
#d.inter <- aggregate(data[which(data$behav2 %in% c("interruption [< 5]","interruption [< 1]")),]$dur,by=list(data[which(data$behav2 %in% c("interruption [< 5]","interruption [< 1]")),]$indday),FUN='sum')
#d.inter2 <- aggregate(data[which(data$behav2 %in% c("interruption [< 1]")),]$dur,by=list(data[which(data$behav2 %in% c("interruption [< 1]")),]$indday),FUN='sum')
#colnames(d.travel) <- c("indday","travel.hrs")
#colnames(d.inter) <- c("indday","inter.hrs")
#colnames(d.inter2) <- c("indday","inter2.hrs")
#d.travel$travel.hrs <- d.travel$travel.hrs / 3600
#d.inter$inter.hrs <- d.inter$inter.hrs / 3600
#d.inter2$inter2.hrs <- d.inter2$inter2.hrs / 3600
#dayset <- merge(dayset,d.travel,all.x=T)
#dayset <- merge(dayset,d.inter,all.x=T)
#dayset <- merge(dayset,d.inter2,all.x=T)

