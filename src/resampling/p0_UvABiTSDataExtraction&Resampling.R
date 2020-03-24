######################################################
### Read Marsh Harrier data from uva-bits server  ####
######################################################

### NOTE: only registered UvA-BiTS users can access the raw data through an ODBC. 
## Unregistered users cannot actually run the code below, but we provide for the sake of 
## transparancy. 

library(RODBC)
db.file <- "GPS". # "GPS" is the name of the ODBC on my work station
db <- odbcConnect(db.file) 

data <- sqlQuery(db, query = "SELECT t.device_info_serial as dev, i.remarks as name,
date_part('year'::text, t.date_time) as yr,
date_part('month'::text, t.date_time) as mth,
date_part('day'::text, t.date_time) as day,
date_part('hour'::text, t.date_time) as hr,
date_part('minute'::text, t.date_time) as min,
date_part('second'::text, t.date_time) as sec,
t.latitude as lat, t.longitude as long, t.altitude as alt
 FROM gps.ee_track_session_limited s
 JOIN gps.ee_individual_limited i
   ON s.individual_id = i.individual_id
 JOIN gps.ee_tracking_speed_limited t
   ON t.device_info_serial = s.device_info_serial
WHERE i.species_latin_name = 'Circus aeruginosus' 
AND t.latitude is not null AND t.longitude is not null AND t.userflag = 0
AND t.date_time > s.start_date
ORDER by t.device_info_serial, t.date_time")


# formate datetimes
data$mth <- sprintf("%02d", data$mth)
data$day <- sprintf("%02d", data$day)
data$hr <- sprintf("%02d", data$hr)
data$min <- sprintf("%02d", data$min)
data$sec <- sprintf("%02d", data$sec)

data$date <- as.Date(paste(data$yr,data$mth,data$day,sep='-'))
data$time <- paste(data$hr,data$min,data$sec,sep=':')

data$dt <- paste(data$date,data$time,sep=' ')
data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

# subset columns
data <- data[,c("dev","name","dt","lat","long","alt")]

## extract year and month columns for tracking data and nests
library(lubridate)
data$date <- as.Date(data$dt)
data$mth <- month(data$dt)
data$yr <- year(data$dt)
data$season <- ifelse(data$mth > 7,'autumn','spring')

## create new bird ID as character
data$dev <- factor(sprintf("%04d",data$dev))
data$dev <- paste('B',data$dev,sep='')

# correct erronous bird name 
data$name <- ifelse(data$name == "Almut\r\n","Almut",as.character(data$name))

# extract julian day since 1st january 2000 for tracking data 
data$julian <- as.numeric(julian(trunc(data$dt, "days"), origin = as.POSIXct("2000-01-01 00:00:00", tz="UTC")))
data$julian <- factor(sprintf("%05d",data$julian))

# extract day of year
data$yday <- yday(data$dt)

# add column dev/julian
data$indday <- as.factor(paste(data$name, data$julian, sep ='_', collapse = NULL))

# append column with track definition (dav,yr,season)
data$migr <- as.factor(paste(data$name, data$yr, data$season, sep ='_', collapse = NULL))

# add column wt country name
gron <- c("B0291","B0634","B6064","B6065")
data$origin <- ifelse(data$dev %in% gron, 'WGK','INBO')


##############################################
### Filter MH data based on speed filter   ###
##############################################

# Calculate movement statistics
source('pt2pt_fxns.R')

# order dataframe
data<-data[order(data$dev,data$dt),]

## define functions to calculate forward distances and duration
calcdist <- function(x) pt2pt.distance(longitude=x$long,latitude=x$lat)
calcdur <- function(x) pt2pt.duration(datetime=x$dt)
## define functions to calculate backward distances and duration
calcdist.b <- function(x) pt2pt.back.distance(longitude=x$long,latitude=x$lat)
calcdur.b <- function(x) pt2pt.back.duration(datetime=x$dt)

## We must order the dataframe in order to ensure the correct application of our coming functions
data <- data[order(data$dev,data$dt),]
v1 <- lapply(split(data,data$dev),"calcdist")
v2 <- lapply(split(data,data$dev),"calcdur")
v1b <- lapply(split(data,data$dev),"calcdist.b")
v2b <- lapply(split(data,data$dev),"calcdur.b")

data$dist <- as.numeric(unlist(v1))
data$dur <- as.numeric(unlist(v2))
data$spd <- data$dist/data$dur
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b

data$high <- ifelse(data$spd > 30 | data$spd.b > 30,1,0)

data <- subset(data,data$high == 0)

data <- data[order(data$dev,data$dt),]
v1 <- lapply(split(data,data$dev),"calcdist")
v2 <- lapply(split(data,data$dev),"calcdur")
v1b <- lapply(split(data,data$dev),"calcdist.b")
v2b <- lapply(split(data,data$dev),"calcdur.b")

data$dist <- as.numeric(unlist(v1))
data$dur <- as.numeric(unlist(v2))
data$spd <- data$dist/data$dur
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b

# order dataframe
data<-data[order(data$dev,data$dt),]

data$high <- ifelse(data$spd > 30 | data$spd.b > 30,1,0)

data <- subset(data,data$high == 0)

data <- data[order(data$dev,data$dt),]
v1 <- lapply(split(data,data$dev),"calcdist")
v2 <- lapply(split(data,data$dev),"calcdur")
v1b <- lapply(split(data,data$dev),"calcdist.b")
v2b <- lapply(split(data,data$dev),"calcdur.b")

data$dist <- as.numeric(unlist(v1))
data$dur <- as.numeric(unlist(v2))
data$spd <- data$dist/data$dur
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b

########################################################
#####     Resample track after removing outliers    ####
#####     and calculate final movement stats        ####     
########################################################
# read resample function
source('Rfunction_resampling_movement.R')
data_resample<-moveTimeSample(data,data$dt,data$dev,60,10,subset=TRUE)

write.csv(data_resample,'MH-resampled-23092017.csv')

