
swedes <- read.csv('swedes.csv')
swedes <- subset(swedes,swedes$filter == 1)

dev <- c("Fe71-07","Ma72-07","Juv73-08","Ma74-08","Fe72-09","Fe65-09") 
name <- c("SW_F1","SW_M1","SW_J1","SW_M2","SW_F2","SW_F3") 

metaswede <- as.data.frame(cbind(dev,name))

swedes <- merge(swedes,metaswede,all.x=T)
swedes <- swedes[,!(colnames(swedes) %in% c("dif","filter"))]

swedes <- swedes[,c("dev","name","dt","lat","long","alt")]


swedes$dt <- as.POSIXct(strptime(swedes$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

## extract year and month columns for tracking swedes and nests
library(lubridate)
swedes$date <- as.Date(swedes$dt)
swedes$mth <- as.numeric(month(swedes$dt))
swedes$yr <- as.numeric(year(swedes$dt))
swedes$season <- ifelse(swedes$mth > 7,'Autumn','Spring')

# extract julian day since 1st january 2000 for tracking swedes 
swedes$julian <- as.numeric(julian(trunc(swedes$dt, "days"), origin = as.POSIXct("2000-01-01 00:00:00", tz="UTC")))
swedes$julian <- factor(sprintf("%05d",swedes$julian))

# extract day of year
swedes$yday <- yday(swedes$dt)

# add column dev/julian
swedes$indday <- as.factor(paste(swedes$name, swedes$julian, sep ='_', collapse = NULL))

# append column with track definition (dav,yr,season)
swedes$migr <- as.factor(paste(swedes$name, swedes$yr, swedes$season, sep ='_', collapse = NULL))

# add column wt country name
swedes$origin <- rep("LUND")

# order dataframe
swedes <- swedes[,c("dev","name","dt","lat","long","alt","date","mth","yr","season","yday","migr","origin","indday","julian")]
swedes<-swedes[order(swedes$dev,swedes$dt),]

