### Generate table with duration and location of stop-overs (incl premigr and winter and breeding stages)
stat <- subset(data,data$type %in% c("winter.stage1","winter.stage2","winter.stage3") | (data$type2 %in% c("migration","move") & data$travel == 0))

# calculate mean lat per stage
mean.lats <- aggregate(stat$lat,by=list(stat$segment),FUN="median")
colnames(mean.lats) <- c("segment","st.lat")

# calculate mean long per stage
mean.longs <- aggregate(stat$long,by=list(stat$segment),FUN="median")
colnames(mean.longs) <- c("segment","st.long")

# calculate duration stage
calcdur <- function(x) difftime(max(x),min(x),units="days")
st.durs <- aggregate(stat$dt,by=list(stat$segment),FUN="calcdur")
colnames(st.durs) <- c("segment","st.dur")

segs <- merge(mean.lats,mean.longs,all.x=T)
segs <- merge(segs,st.durs,all.x=T)

segs <- merge(segs,unique(stat[,c("segment","name","cycle","season","type2","type","origin")]),all.x=T)

