#### LOAD LAYERS FOR MAPPING
##############################

longlimits <- c(min(c(data$long)) - 0.5, max(c(data$long)) + 0.5)
latlimits <- c(min(c(data$lat)) - 0.5, max(c(data$lat)) + 0.5)

# create df for country polygons
political <- fortify(countries, region = "NAME_EN")
political$country <- political$id
political$group <- ifelse(political$country %in% unique(data$country) & political$piece == 1, 1, 0)

# Read DEM
source(here::here("src", "sidescript_readDEM.R"))

# Load Google Earth map
# africa <- get_map(location=c(-4,30), zoom = 4, maptype ='satellite')

#### PRODUCE MAP IN GGPLOT2
##############################
# make map wt tracks against political map highlighting countries frequented by birds
## Google Earth option
# 	ggmap(africa) +
## Highlight countries passed
ggplot() +
  # 	geom_polygon(data=political,aes(long,lat,group=paste(country,piece),fill=factor(group)),col='white')+
  # 	scale_fill_manual(values=c("0"="grey60","1"="grey50"))+
  ## Add Digital elevation model
  geom_raster(data = hdf2, mapping = aes(long, lat, alpha = alt), fill = "grey30") +
  scale_alpha_continuous(name = "Elevation", range = c(1, 0.1), breaks = c(100, 500, 1000, 2000, 3000)) +
  ## Add country boundaries
  geom_polygon(data = political, aes(long, lat, group = paste(country, piece)), fill = "transparent", col = "black", size = .5) +
  ## Add tracks
  geom_path(data = migration, aes(x = long, y = lat, group = migr, col = pop), size = .6) +
  ## Add stopover points
  geom_point(data = segs[which(segs$season %in% c("Autumn", "Spring")), ], aes(x = st.long, y = st.lat, size = as.numeric(st.dur), col = pop, fill = pop)) +
  scale_fill_manual(values = c("LC" = "blue", "SW" = "red")) +
  scale_colour_manual(values = c("LC" = "blue", "SW" = "red")) +
  # 	scale_colour_manual(name="ID",values=c("BE_M1"="cyan3","BE_M2"="darkorange2","BE_M3"="magenta2",
  # 							   "BE_M4"="chartreuse3","NL_M1"="red2","NL_M2"="gold2",
  # 							   "SW_F1"="cadetblue3","SW_F3"="tomato3","SW_M1"="springgreen2",
  # 							   "SW_M2"="khaki2"))+
  # 	scale_fill_manual(name="ID",values=c("BE_M1"="cyan3","BE_M2"="darkorange2","BE_M3"="magenta2",
  # 							   "BE_M4"="chartreuse3","NL_M1"="red2","NL_M2"="gold2",
  # 							   "SW_F1"="cadetblue3","SW_F3"="tomato3","SW_M1"="springgreen2",
  # 							   "SW_M2"="khaki2"))+
  scale_size_continuous(name = "Stopover duration", range = c(2, 5), breaks = c(1, 2, 5, 10, 15), labels = c("1", "2-5", "6-10", "11-15", "15+")) +
  geom_point(data = segs[which(segs$season %in% c("Autumn", "Spring")), ], aes(x = st.long, y = st.lat), size = 1.2, col = "black") +
  ## Layout map
  coord_quickmap(xlim = longlimits, ylim = latlimits, expand = FALSE) +
  theme_bw() +
  facet_grid(. ~ season) +
  xlab("\nLong[º]") +
  ylab("Lat[º]\n") +
  ## Layout text items
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    panel.border = element_rect(colour = "white"),
    strip.background = element_rect(fill = "white", colour = "NA"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  guides(pop = FALSE)

#### SAVE OUTPUT
##############################
ggsave(here::here("reports", "figures", "Fig1A_Routes&Stopovers-bw.tiff"), dpi = 300, width = 8, height = 8)
