#### EXTRACT ANNUAL DEPARTURE AND ARRIVAL DATES AND DURATIONS FROM BEHAVIOUR TABLE
################################################################################
behaviour$yr <- year(behaviour$date)
names.new <- unique(dayset[, c("name", "ID")])
behaviour <- merge(behaviour, names.new, all.x = T)
behaviour <- subset(behaviour, behaviour$name %in% names.new$name)

arr.spring <- behaviour[which(behaviour$type2 == "breeding" & behaviour$use.date == 1), c("ID", "yr", "date")]
colnames(arr.spring)[3] <- "arr.spring"

dep.spring <- behaviour[which(behaviour$type2 == "migration" & behaviour$season2 == "spring" & behaviour$use.date == 1), c("ID", "yr", "date")]
colnames(dep.spring)[3] <- "dep.spring"

arr.autumn <- behaviour[which(behaviour$type == "winter.stage1" & behaviour$use.date == 1), c("ID", "yr", "date")]
colnames(arr.autumn)[3] <- "arr.autumn"

dep.autumn <- behaviour[which(behaviour$type2 == "migration" & behaviour$season2 == "autumn" & behaviour$use.date == 1), c("ID", "yr", "date")]
colnames(dep.autumn)[1:3] <- c("ID", "yr", "dep.autumn")
# NOTE! original annotation file discriminated pre-migratory departures in column "type"
# but when we made "type" we considered these as "migration". as a result there are multiple
# departure dates for some trips. We can take the first available data to ensure we have
# same start points as used for all preceding analyses.
autdeps <- aggregate(dep.autumn$dep.autumn, by = list(dep.autumn$ID, dep.autumn$yr), FUN = "min")
colnames(autdeps) <- c("ID", "yr", "dep.autumn")

spr.times <- merge(arr.spring, dep.spring, all.x = T)
aut.times <- merge(arr.autumn, autdeps, all.x = T)

times <- merge(spr.times, aut.times, all = T)

times$arr.spr.yday <- yday(times$arr.spring)
times$arr.aut.yday <- yday(times$arr.autumn)
times$dep.spr.yday <- yday(times$dep.spring)
times$dep.aut.yday <- yday(times$dep.autumn)

times$spr.dur <- times$arr.spr.yday - times$dep.spr.yday + 1
times$aut.dur <- times$arr.aut.yday - times$dep.aut.yday + 1

times <- merge(times, unique(dayset[, c("ID", "pop")]), all.x = T)

#### EXTRACT MEAN NR OF STOP DAYS, TRAVEL DAYS, DAILY DISTANCES,
#### DAILY V WINDS PER TRIP FROM DAYSET TABLE
################################################################################
dayset$yr <- year(dayset$date)

# SUBSET DAYSET DATASET PER SEASON
ds.aut <- subset(dayset, dayset$season == "Autumn")
ds.spr <- subset(dayset, dayset$season == "Spring")

na.mean <- function(x) mean(na.omit(x))

# WE WILL USE SEGMENT LENGHTS TO CALCULATE TOTAL TIME SPENT ON STOPOVERS
cycleyears <- unique(data[, c("cycle", "yr")])
segs <- merge(segs, cycleyears, all.x = T)
segs$yr <- as.factor(segs$yr)
sum.nr <- function(x) sum(as.numeric(x))

## AUTUMN STATS
dist.aut <- aggregate(ds.aut$daily.dist, by = list(ds.aut$ID, ds.aut$yr), FUN = "na.mean")
colnames(dist.aut)[1:3] <- c("ID", "yr", "d.dist.aut")

tailwind.aut <- aggregate(ds.aut$tail, by = list(ds.aut$ID, ds.aut$yr), FUN = "na.mean")
colnames(tailwind.aut)[1:3] <- c("ID", "yr", "tailwind.aut")

cul <- function(x) length(unique(x))
na.mean <- function(x) mean(na.omit(x))
travel.aut <- aggregate(ds.aut[which(ds.aut$travel == 1), ]$date, by = list(ds.aut[which(ds.aut$travel == 1), ]$ID, ds.aut[which(ds.aut$travel == 1), ]$yr), FUN = "cul")
colnames(travel.aut)[1:3] <- c("ID", "yr", "travel.days.aut")

# stop.aut <- aggregate(ds.aut[which(ds.aut$travel == 0),]$date,by=list(ds.aut[which(ds.aut$travel == 0),]$ID,ds.aut[which(ds.aut$travel == 0),]$yr),FUN='cul')
# colnames(stop.aut)[1:3] <- c("ID","yr","stop.days.aut")

stop.aut <- aggregate(segs[which(segs$type == "migration" & segs$season == "Autumn"), ]$st.dur, by = list(segs[which(segs$type == "migration" & segs$season == "Autumn"), ]$ID, segs[which(segs$type == "migration" & segs$season == "Autumn"), ]$yr), FUN = "sum.nr")
colnames(stop.aut)[1:3] <- c("ID", "yr", "stop.days.aut")

## SPRING STATS
dist.spr <- aggregate(ds.spr$daily.dist, by = list(ds.spr$ID, ds.spr$yr), FUN = "na.mean")
colnames(dist.spr)[1:3] <- c("ID", "yr", "d.dist.spr")

tailwind.spr <- aggregate(ds.spr$tail, by = list(ds.spr$ID, ds.spr$yr), FUN = "na.mean")
colnames(tailwind.spr)[1:3] <- c("ID", "yr", "tailwind.spr")

cul <- function(x) length(unique(x))
travel.spr <- aggregate(ds.spr[which(ds.spr$travel == 1), ]$date, by = list(ds.spr[which(ds.spr$travel == 1), ]$ID, ds.spr[which(ds.spr$travel == 1), ]$yr), FUN = "cul")
colnames(travel.spr)[1:3] <- c("ID", "yr", "travel.days.spr")

# stop.spr <- aggregate(ds.spr[which(ds.spr$travel == 0),]$date,by=list(ds.spr[which(ds.spr$travel == 0),]$ID,ds.spr[which(ds.spr$travel == 0),]$yr),FUN='cul')
# colnames(stop.spr)[1:3] <- c("ID","yr","stop.days.spr")

stop.spr <- aggregate(segs[which(segs$type == "migration" & segs$season == "Spring"), ]$st.dur, by = list(segs[which(segs$type == "migration" & segs$season == "Spring"), ]$ID, segs[which(segs$type == "migration" & segs$season == "Spring"), ]$yr), FUN = "sum.nr")
colnames(stop.spr)[1:3] <- c("ID", "yr", "stop.days.spr")

# COLLATE AUTUMN STATS
aut.stats <- merge(tailwind.aut, dist.aut, all = T)
aut.stats <- merge(aut.stats, travel.aut, all = T)
aut.stats <- merge(aut.stats, stop.aut, all = T)
aut.stats$stop.days.aut <- ifelse(is.na(aut.stats$stop.days.aut) == TRUE & is.na(aut.stats$travel.days.aut) == FALSE, 0, aut.stats$stop.days.aut)
aut.stats$all.days.aut <- aut.stats$travel.days.aut + aut.stats$stop.days.aut

# COLLATE SPRING STATS
spr.stats <- merge(tailwind.spr, dist.spr, all = T)
spr.stats <- merge(spr.stats, travel.spr, all = T)
spr.stats <- merge(spr.stats, stop.spr, all = T)
spr.stats$stop.days.spr <- ifelse(is.na(spr.stats$stop.days.spr) == TRUE & is.na(spr.stats$travel.days.spr) == FALSE, 0, spr.stats$stop.days.spr)
spr.stats$all.days.spr <- spr.stats$travel.days.spr + spr.stats$stop.days.spr

## MERGE AUTUMN AND SPRING STATS
stats <- merge(aut.stats, spr.stats, all = T)

## MERGE STATISTICS FROM BEHAVIOUR TABLE WITH THOSE FROM DAILY STATS TABLE
################################################################################
full <- merge(times, stats, all = T)
full <- full[order(full$ID, full$yr), ]

# full$cov.aut <- full$all.days.aut / full$aut.dur
# full$cov.spr <- full$all.days.spr / full$spr.dur

full2 <- full[, !(colnames(full) %in% c("yr", "name", "origin", "pop", "arr.spring", "dep.spring", "arr.autumn", "dep.autumn"))]
full2$ID <- as.factor(full2$ID)

full2a <- full2[, (colnames(full2) %in% c("ID", "arr.spr.yday", "dep.spr.yday", "travel.days.spr", "stop.days.spr", "all.days.spr", "d.dist.spr", "tailwind.spr", "spr.dur"))]
full2b <- full2[, (colnames(full2) %in% c("ID", "arr.aut.yday", "dep.aut.yday", "travel.days.aut", "stop.days.aut", "all.days.aut", "d.dist.aut", "tailwind.aut", "aut.dur"))]
colnames(full2a)[1:9] <- c("ID", "arr", "dep", "dur", "tailwind", "d.dist", "travel.days", "stop.days", "all.days")
full2a$season <- rep("Spring")
colnames(full2b)[1:9] <- c("ID", "arr", "dep", "dur", "tailwind", "d.dist", "travel.days", "stop.days", "all.days")
full2b$season <- rep("Autumn")

full3 <- rbind(full2a, full2b)

## AGGREGATE TO INDIVIDUAL LEVEL
################################################################################

df <- aggregate(full3[, c("arr", "dep", "stop.days", "tailwind", "dur")], by = list(full3$ID, full3$season), FUN = "na.mean")
colnames(df)[1:2] <- c("ID", "season")
df <- merge(df, unique(full[, c("ID", "pop")]), all.x = T)

na.se <- function(x) sd(na.omit(x))
df2 <- aggregate(full3[, c("arr", "dep", "stop.days", "tailwind", "dur")], by = list(full3$ID, full3$season), FUN = "na.se")
colnames(df2)[1:2] <- c("ID", "season")
colnames(df2)[3:7] <- c("arr.sd", "dep.sd", "stop.days.sd", "tailwind.sd", "dur.sd")
df2 <- merge(df2, unique(full[, c("ID", "pop")]), all.x = T)

df <- merge(df, df2, all = T)

## COUNT NR OF TRIPS PER SEASON PER INDIVIDUAL
################################################################################
cul <- function(x) length(unique(na.omit(x)))
nr.auts <- aggregate(full[which(is.na(full$arr.autumn) == F), ]$yr, by = list(full[which(is.na(full$arr.autumn) == F), ]$ID), FUN = "cul")
colnames(nr.auts) <- c("ID", "nr.trips")
nr.auts$season <- "Autumn"
nr.sprs <- aggregate(full[which(is.na(full$arr.spring) == F), ]$yr, by = list(full[which(is.na(full$arr.spring) == F), ]$ID), FUN = "cul")
colnames(nr.sprs) <- c("ID", "nr.trips")
nr.sprs$season <- "Spring"

nr.trips <- rbind(nr.sprs, nr.auts)

df <- merge(df, nr.trips, all.x = T)

write_csv(df, here::here("reports", "tables", "TableS1_IndividualTiming&Stops.csv"))



## COUNT NR AND DURATION OF STOPOVERS PER REGION, PER SEASON AND PER BIRD
#########################################################################
stops <- subset(segs, segs$season %in% c("Autumn", "Spring"))

stops$region <- ifelse(stops$st.lat > 30 & stops$st.lat < 35, "N Afr",
  ifelse(stops$st.lat < 30 & stops$st.lat > 20, "Sah",
    ifelse(stops$st.lat > 35, "Eur", "other")
  )
)

stops.pbpy.nr <- aggregate(stops$segment, by = list(stops$ID, stops$season, stops$region, stops$yr), FUN = "cul")
colnames(stops.pbpy.nr) <- c("ID", "season", "region", "yr", "nr.stops")
stops.pbpy.dur <- aggregate(stops$st.dur, by = list(stops$ID, stops$season, stops$region, stops$yr), FUN = "na.mean")
colnames(stops.pbpy.dur) <- c("ID", "season", "region", "yr", "dur.stops")

nr.stops.mean <- aggregate(stops.pbpy.nr$nr.stops, by = list(stops.pbpy.nr$ID, stops.pbpy.nr$season, stops.pbpy.nr$region), FUN = "na.mean")
colnames(nr.stops.mean) <- c("ID", "season", "region", "mean.nr.stops")
nr.stops.sd <- aggregate(stops.pbpy.nr$nr.stops, by = list(stops.pbpy.nr$ID, stops.pbpy.nr$season, stops.pbpy.nr$region), FUN = "sd")
colnames(nr.stops.sd) <- c("ID", "season", "region", "sd.nr.stops")

dur.stops.mean <- aggregate(stops.pbpy.dur$dur.stops, by = list(stops.pbpy.dur$ID, stops.pbpy.dur$season, stops.pbpy.dur$region), FUN = "na.mean")
colnames(dur.stops.mean) <- c("ID", "season", "region", "mean.dur.stops")
dur.stops.sd <- aggregate(stops.pbpy.dur$dur.stops, by = list(stops.pbpy.dur$ID, stops.pbpy.dur$season, stops.pbpy.dur$region), FUN = "sd")
colnames(dur.stops.sd) <- c("ID", "season", "region", "sd.dur.stops")

stopovers <- merge(nr.stops.mean, nr.stops.sd, all.x = T)
stopovers <- merge(stopovers, dur.stops.mean, all.x = T)
stopovers <- merge(stopovers, dur.stops.sd, all.x = T)

write_csv(stopovers, here::here("reports", "tables", "TableS9_StopoversPerRegionandSeason.csv"))
