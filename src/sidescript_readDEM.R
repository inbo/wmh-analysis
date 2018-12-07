download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_30s_bil.zip", "data/maps/alt_30s_bil.zip")
unzip(zipfile = "data/maps/alt_30s_bil.zip", exdir = "data/maps/alt_30s_bil")
file.remove("data/maps/alt_30s_bil.zip")

dem <- raster(here("data", "maps", "alt_30s_bil", "alt.bil"))

latlimits <- c(min(data$lat)-0.5,max(data$lat)+0.5) 
longlimits <- c(min(data$long)-0.5,max(data$long)+0.5) 

dem2 <- crop(dem, extent(longlimits[1],longlimits[2],latlimits[1],latlimits[2]))
dem3 <- aggregate(dem2,5,FUN='mean')
hdf2 <- rasterToPoints(dem3)
hdf2 <- data.frame(hdf2)
colnames(hdf2) <- c("long","lat","alt")