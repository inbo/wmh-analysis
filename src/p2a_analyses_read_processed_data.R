library(lubridate);library(circular);library(ggplot2)
library(maptools);library(lattice);library(fossil)
library(RColorBrewer)
library(here)
library(readr)

#### STEP 1: GATHER ALL DATA
#############################
# gather INBO and WGK data from csv produced previously, containing tracks
# resampled to hourly intervals
data <- read_csv(here("data", "MH-resampled-23092017.csv"))
data <- data[,c("dev","name","dt","lat","long","alt","date","mth","yr","season","yday","migr","origin","indday")]

data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

# extract julian day since 1st january 2000 for tracking data
data$date <- as.Date(data$dt,tz='UTC')
data$julian <- as.numeric(julian(trunc(data$dt, "days"), origin = as.POSIXct("2000-01-01 00:00:00", tz="UTC")))
# create unique identifier for each ID/day based on julian day
data$julian <- factor(sprintf("%05d",data$julian))
data$indday <- as.factor(paste(data$name, data$julian, sep ='_', collapse = NULL))

# generate additional useful stats regarding season, time of year, ...
data$season <- ifelse(data$mth > 7,'Autumn','Spring')
data$mth <- as.numeric(data$mth)
data$yr <- as.numeric(data$yr)
data$yday <- as.numeric(data$yday)

data$origin <- as.character(data$origin)

# Gather Swedish data with all the same columns as df "data" created above
source(here("src", "sidescript_readSwedes.R"))
swedes <- subset(swedes,swedes$long != 0 & is.na(swedes$long) == FALSE)

# Combine al datasets
data <- rbind(data,swedes)

#### STEP 2: CALCULATE HOURLY & DAILY MOVEMENT STATISTICS
##########################################################
# manually remove some outliers
touts <- c("2012-03-24 04:09:06","2012-03-24 19:09:07","2008-09-18 19:43:10","2011-09-20 18:46:48","2011-10-01 10:55:04","2011-10-02 10:47:15","2011-10-03 10:39:06","2011-10-04 10:29:29")
touts <- as.POSIXct(strptime(touts, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
rem <- which(data$name == "SW_F1" & data$dt %in% touts)
data <- data[-rem,]

# Read custom-made functions for computing trajectory (i.e. between fixes) movement statistics
source(here("src", "pt2pt_fxns.R"))

## define functions to calculate forward distances and duration
calcdist <- function(x) pt2pt.distance(longitude=x$long,latitude=x$lat)
calcdur <- function(x) pt2pt.duration(datetime=x$dt)
## define functions to calculate backward distances and duration
calcdist.b <- function(x) pt2pt.back.distance(longitude=x$long,latitude=x$lat)
calcdur.b <- function(x) pt2pt.back.duration(datetime=x$dt)

calcdir <- function(x) pt2pt.direction(longitude=x$long,latitude=x$lat)

# Order dataframe chronologically per bird
data<-data[order(data$dev,data$dt),]

# Split df per bird and apply functions
v1 <- lapply(split(data,data$dev),"calcdist")
v2 <- lapply(split(data,data$dev),"calcdur")
v1b <- lapply(split(data,data$dev),"calcdist.b")
v2b <- lapply(split(data,data$dev),"calcdur.b")
v3 <- lapply(split(data,data$dev),"calcdir")

# Put results back into original df
data$dist <- as.numeric(unlist(v1))
data$dur <- as.numeric(unlist(v2))
data$spd <- data$dist/data$dur
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b
data$dir <- as.numeric(unlist(v3))

### Calculate Daily Statistics
source(here("src", "sidescript_CalcDailyStats.R"))
data$date <- as.Date(data$date,tz='UTC')

#### STEP 3: DISTINGUISH DAY/NIGHT OBSERVATIONS
################################################
crds <- cbind(data$long,data$lat)
crds <- SpatialPoints(crds,proj4string=CRS("+proj=longlat +datum=WGS84"))

dates <- as.POSIXct(strftime(data$dt,format="%Y-%m-%d"),tz="UTC")

# calculate sunrise times
srise <- sunriset(crds, dates, direction=c("sunrise"),POSIXct.out=TRUE)
colnames(srise)[1:2] <- c("srise.dayfrac","srise.time")

# calculate sunset times
sset <- sunriset(crds, dates, direction=c("sunset"),POSIXct.out=TRUE)
colnames(sset)[1:2] <- c("sset.dayfrac","sset.time")

# append sunrise and sunset in exact times back to dataframe
data$srise_R <- srise[,2]
data$sset_R <- sset[,2]

# classify day and night locations
data$daynight <- ifelse(data$dt > data$srise_R -3600 & data$dt < data$sset_R + 3600,'day','night')

# remove a few nighttime outliers detected based on threshold spd 5 ms
data <- data[-which(data$name == "Ben" & data$dt == as.POSIXct(strptime("2016-11-20 05:43:06", format="%Y-%m-%d %H:%M:%S"), tz='UTC')),]
data <- data[-which(data$name == "Ben" & data$dt == as.POSIXct(strptime("2016-11-22 03:56:25", format="%Y-%m-%d %H:%M:%S"), tz='UTC')),]

#### STEP 4: ANNOTATE GEOGRAPHICAL INFORMATION
################################################
library(rgdal);library(sp);library(raster)

if(!dir.exists("data/maps/ne_50m_admin_0_countries")) {
  download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip","data/maps/ne_50m_admin_0_countries.zip")
  unzip(zipfile = "data/maps/ne_50m_admin_0_countries.zip", exdir = "data/maps/ne_50m_admin_0_countries")
  file.remove("data/maps/ne_50m_admin_0_countries.zip")
}

countries <- shapefile(here("data", "maps", "ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp"))

countries <- countries[countries$CONTINENT %in% c("Europe","Africa"),]

np <- data[,c("name","dt","long","lat")]
coordinates(np) <- ~ long + lat
proj4string(np) <- proj4string(countries)

## Extract country per point
require(spatialEco)
out <- point.in.poly(np, countries["NAME_EN"])
out <- as.data.frame(out)
colnames(out)[3] <- "country"
data <- merge(data,out[,c("name","dt","country")],all.x=T)

## Extract continent per point
out <- point.in.poly(np, countries["CONTINENT"])
out <- as.data.frame(out)
colnames(out)[3] <- "continent"
data <- merge(data,out[,c("name","dt","continent")],all.x=T)
data$continent <- ifelse(is.na(data$continent) == T,'sea',as.character(data$continent))
data$continent <- factor(as.factor(data$continent),levels=c("Europe","Africa","sea"))