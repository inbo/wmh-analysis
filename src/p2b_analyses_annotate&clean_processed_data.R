##### WV determined the start and end dates of migratory and
##### non-migratory periods through expert interpretation of
##### tracks in GE. We here annotate our dataset according to
##### different stages in the annual cycle.

#### STEP 1: Process Dutch/Belgian annotation data
###################################################
behaviour <- read_csv(here::here("data", "raw", "annotation.csv"))
behaviour$date <- as.Date(behaviour$date)
colnames(behaviour)[1] <- "name"

## Add corrections: ignore pre-migratory movements! these should be classified as migration
# give all data on fixed winter locations same type
behaviour$type2 <- ifelse(behaviour$type %in% c("winter.stage1", "winter.stage2", "winter.stage3"), "winter", as.character(behaviour$type))

# extract season info to correct clasisfication
behaviour$cycle2 <- as.character(behaviour$cycle)
behaviour$cycle2 <- gsub("[.]", ":", behaviour$cycle2)
behaviour$season2 <- gsub(":.*", "", behaviour$cycle2)

# reclassify pre-migr and move during migration periods as migration
behaviour$type2 <- ifelse(behaviour$type2 %in% c("move", "pre-migration") & behaviour$season2 == "autumn", "migration", as.character(behaviour$type2))

#### STEP 2: Process Swedish annotation data
###################################################
swedes.behaviour <- read_csv(here::here("data", "raw", "annotation_swedes.csv"))
swedes.behaviour$date <- as.Date(swedes.behaviour$date)
colnames(swedes.behaviour)[1] <- "name"

## Add corrections: ignore pre-migratory movements! these should be classified as migration
# give all data on fixed winter locations same type
swedes.behaviour$type2 <- ifelse(swedes.behaviour$type %in% c("winter.stage1", "winter.stage2", "winter.stage3"), "winter", as.character(swedes.behaviour$type))

# extract season info to correct clasisfication
swedes.behaviour$cycle2 <- as.character(swedes.behaviour$cycle)
swedes.behaviour$cycle2 <- gsub("[.]", ":", swedes.behaviour$cycle2)
swedes.behaviour$season2 <- gsub(":.*", "", swedes.behaviour$cycle2)

#### STEP 3: Annotate tracking data with annual cycles
###################################################
# We merge behaviour dfs with tracking df, resulting in values
# for "type" and "cycle" at start dates
behaviour <- rbind(behaviour, swedes.behaviour)
data <- merge(data, behaviour[, c("name", "date", "cycle", "type2", "type", "complete")], all.x = T)

# Fill NA?s with last known annual cycle
data <- data[order(data$name, data$dt), ]
data$type <- na.locf(data$type)
data$type2 <- na.locf(data$type2)
data$cycle <- na.locf(data$cycle)
data$complete <- na.locf(data$complete)

#### STEP 4: Use annotation info to further clean data
######################################################
# Drop data for birds without migration data or tracks recorded after death or incomplete stages
data <- subset(data, data$type2 != "dead")
data <- subset(data, data$complete == 1)
data <- subset(data, data$name %in% c("Ben", "Jozef", "Peter", "Roelof", "Raymond", "William", "SW_F1", "SW_F3", "SW_M1", "SW_M2"))

# Replace old season column (2 seasons) with season in cycle (4 seasons)
data$cycle <- as.character(data$cycle)
data$cycle <- gsub("[.]", ":", data$cycle)
data$season <- gsub(":.*", "", data$cycle)

### Determine day of cycle (DOC) for plotting lat vs timing
### 1 = 1st of July / 365 = 30th of June
data$doc <- data$yday - as.numeric(yday(as.Date(as.character(paste(year(data$dt), "-07-01", sep = "")))))
data$doc <- ifelse(data$doc < 0, data$doc + 365, data$doc)

#### STEP 5: CALCULATE DURATION AND LOC OF STOP OVER EVENTS
######################################################
data <- data[order(data$name, data$dt), ]

# Classify traveld Days
data$travel <- ifelse(data$daily.dist / 1000 < 25 | data$type %in% c("winter.stage1", "winter.stage2", "winter.stage3"), 0, 1)

# Manually correct some incorrect clasisfications for autumn
data[which(data$name == "SW_M1" & data$date %in% c(as.Date("2007-09-23"), as.Date("2007-09-25"))), ]$travel <- 1
data[which(data$name == "SW_F1" & data$date %in% c(as.Date("2011-09-20"), as.Date("2008-09-18"), as.Date("2009-09-17"), as.Date("2009-09-19"), as.Date("2010-10-01"), as.Date("2010-09-24"))), ]$travel <- 1
data[which(data$name == "SW_F3" & data$date %in% c(as.Date("2009-09-17"), as.Date("2009-09-22"))), ]$travel <- 1
data[which(data$name == "SW_M2" & data$date %in% c(as.Date("2008-09-20"))), ]$travel <- 1

# Manually correct some incorrect clasisfications for spring
data[which(data$name == "SW_F1" & data$date %in% c(as.Date("2008-03-26"), as.Date("2012-04-04"))), ]$travel <- 1

## Segment track
data <- data[order(data$name, data$dt), ]
data$segment <- as.numeric(c(0, cumsum(as.logical(diff(as.numeric(as.factor(paste(data$name, data$type2, data$travel))))))))
data$segment <- sprintf("%03d", data$segment)
data$segment <- paste(data$name, data$segment, sep = "_")

## Classify daytypes
# data$type <- ifelse(data$type == 'migration' & data$daily.dist/1000 < 25,"migration stop-over",
# 		ifelse(data$type == 'migration' & data$daily.dist/1000 >= 25,"migration travel",as.character(data$type)))

####
data <- data[order(data$name, data$dt), ]
data$season <- factor(as.factor(data$season), levels = c("autumn", "winter", "spring", "summer"), labels = c("Autumn", "Winter", "Spring", "Summer"))

### Generate coordinates and durations of all stopovers
source(here("src", "sidescript_CalcStopovers.R"))

data$pop <- ifelse(data$origin == "LUND", "SW", "LC")
segs$pop <- ifelse(segs$origin == "LUND", "SW", "LC")

#### STEP 6: PREPARE DF FOR MAPPING BY CREATING SUBSETS,
#### ADJUSTING IDs, ....
######################################################
migration <- subset(data, data$type2 == "migration")
winter <- subset(data, data$season %in% c("Winter") & data$type2 == "winter")
move <- subset(data, data$season %in% c("Winter") & data$type2 != "winter")

lc.names <- c("BE_M1", "BE_M2", "BE_M3", "BE_M4", "NL_M1", "NL_M2")
names <- unique(as.character(data[which(data$pop == "LC"), ]$name))
names.new <- as.data.frame(cbind(lc.names, names))
colnames(names.new) <- c("ID", "name")

segs <- merge(segs, names.new, all.x = T)
migration <- merge(migration, names.new, all.x = T)
move <- merge(move, names.new, all.x = T)
winter <- merge(winter, names.new, all.x = T)
data <- merge(data, names.new, all.x = T)

winter$ID <- ifelse(is.na(winter$ID) == TRUE, as.character(winter$name), as.character(winter$ID))
migration$ID <- ifelse(is.na(migration$ID) == TRUE, as.character(migration$name), as.character(migration$ID))
move$ID <- ifelse(is.na(move$ID) == TRUE, as.character(move$name), as.character(move$ID))
segs$ID <- ifelse(is.na(segs$ID) == TRUE, as.character(segs$name), as.character(segs$ID))
data$ID <- ifelse(is.na(data$ID) == TRUE, as.character(data$name), as.character(data$ID))

migration <- migration[order(migration$name, migration$dt), ]
data <- data[order(data$name, data$dt), ]
move <- move[order(move$name, move$dt), ]
