## FIG S2: OVERWINTERING
########################

####
longlimits <- c(-18, 1)
latlimits <- c(5, 19)

# Reload DEM at higher res
dem2 <- crop(dem, extent(longlimits[1], longlimits[2], latlimits[1], latlimits[2]))
hdf2 <- rasterToPoints(dem2)
hdf2 <- data.frame(hdf2)
colnames(hdf2) <- c("long", "lat", "alt")


wintersegs <- segs[which(segs$type %in% c("winter.stage1", "winter.stage2", "winter.stage3")), ]
wintersegs$lab <- ifelse(wintersegs$type == "winter.stage1", "1", ifelse(wintersegs$type == "winter.stage2", "2", "3"))

w.lats <- aggregate(wintersegs$st.lat, by = list(wintersegs$name, wintersegs$type), FUN = "median")
w.longs <- aggregate(wintersegs$st.long, by = list(wintersegs$name, wintersegs$type), FUN = "median")
w.durs <- aggregate(wintersegs$st.dur, by = list(wintersegs$name, wintersegs$type), FUN = "mean")
colnames(w.lats) <- c("name", "type", "w.lat")
colnames(w.longs) <- c("name", "type", "w.long")
colnames(w.durs) <- c("name", "type", "w.dur")

wsegs <- merge(w.lats, w.longs, all = T)
wsegs <- merge(wsegs, w.durs, all = T)
wsegs$lab <- ifelse(wsegs$type == "winter.stage1", "1", ifelse(wsegs$type == "winter.stage2", "2", "3"))
wsegs <- merge(wsegs, unique(segs[, c("name", "pop", "ID")]))

move <- move[order(move$name, move$dt), ]

#### INITIATE MAPPING
ggplot() +
  ## Add Digital elevation model
  geom_raster(data = hdf2, mapping = aes(long, lat, alpha = alt), fill = "grey10") +
  scale_alpha_continuous(name = "Altitude", range = c(1.00, 0.10), breaks = c(100, 500, 1000, 2000, 3000)) +
  ## Add tracks
  geom_path(data = move, aes(x = long, y = lat, group = migr, col = ID), size = .6) +
  ## Add wintering sites
  geom_point(data = wsegs, aes(x = w.long, y = w.lat, size = as.numeric(w.dur), col = ID, fill = ID)) +
  # 	scale_colour_manual(values=c("LC"="blue","SW"="red"))+
  # 	scale_fill_manual(values=c("LC"="blue","SW"="red"))+
  scale_colour_manual(name = "ID", values = c(
    "BE_M1" = "cyan3", "BE_M2" = "darkorange2", "BE_M3" = "magenta2",
    "BE_M4" = "chartreuse3", "NL_M1" = "red2", "NL_M2" = "gold2",
    "SW_F1" = "cadetblue3", "SW_F3" = "tomato3", "SW_M1" = "springgreen2",
    "SW_M2" = "khaki2"
  )) +
  scale_fill_manual(name = "ID", values = c(
    "BE_M1" = "cyan3", "BE_M2" = "darkorange2", "BE_M3" = "magenta2",
    "BE_M4" = "chartreuse3", "NL_M1" = "red2", "NL_M2" = "gold2",
    "SW_F1" = "cadetblue3", "SW_F3" = "tomato3", "SW_M1" = "springgreen2",
    "SW_M2" = "khaki2"
  )) +
  scale_size_continuous(name = "Stopover duration", range = c(2, 5), breaks = c(1, 30, 60, 90, 120), labels = c("1-29", "30-59", "60-89", "90-119", "120-153")) +
  geom_text(data = wsegs, aes(x = w.long + 0.4, y = w.lat + 0.3, label = lab, col = ID), size = 4, fontface = "bold", show.legend = FALSE) +
  geom_point(data = wsegs, aes(x = w.long, y = w.lat), size = 1.2, col = "black") +
  ## Add stopovers during movements
  # 	geom_point(data=segs[which(segs$type2 %in% c("move")),],aes(x=st.long,y=st.lat,col=name,size=st.dur),alpha=.5)+
  ## Layout map
  coord_quickmap(xlim = longlimits, ylim = latlimits, expand = FALSE) +
  theme_bw() +
  facet_grid(. ~ season) +
  xlab("\nLong[°]") +
  ylab("Lat[°]\n") +
  ## Layout text items
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    panel.border = element_rect(colour = "white"),
    strip.background = element_rect(fill = "white", colour = "NA"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 10, face = "bold")
  )

ggsave(here::here("reports", "figures", "FigS2.tiff"), dpi = 300, width = 7, height = 5)
