### LATITUDE VS TIME
ggplot() +
  # Add tracks
  geom_path(data = data[which(data$type2 != "migration"), ], aes(x = doc, y = lat, group = paste(yr, mth, segment)), col = "grey40", size = .6) +
  geom_path(data = move[which(move$season == "Winter"), ], aes(x = doc, y = lat, group = paste(yr, mth, segment), col = ID), linetype = "dashed", size = .6) +
  geom_path(data = migration[which(migration$pop == "LC"), ], aes(x = doc, y = lat, group = paste(yr, mth, segment), col = ID), size = .6, alpha = .8) +
  geom_path(data = migration[which(migration$pop == "SW"), ], aes(x = doc, y = lat, group = paste(yr, mth, segment), col = ID), size = .6, alpha = .8) +
  # Add stopovers
  geom_point(data = migration[which(migration$travel == 0), ], aes(x = doc, y = lat, col = ID), size = .9) +
  geom_point(data = migration[which(migration$travel == 0), ], aes(x = doc, y = lat), size = .2, col = "black") +
  # Define colour scale for pop or ID
  ## COLOUR SCHEME FOR POPS
  # scale_colour_manual(values=c("LC"="blue","SW"="red"))+
  ## NEW COLOUR SCHEME FOR INDIVIDUALS
  # scale_colour_manual(name="ID",values=c("BE_M1"="firebrick1","BE_M2"="red","BE_M3"="brown4",
  #                                       "BE_M4"="darkorange2","NL_M1"="darkgoldenrod1","NL_M2"="gold1",
  #                                       "SW_F1"="turquoise3","SW_F3"="blue1","SW_M1"="violetred1","SW_M2"="magenta4"))+
  ## OLD COLOUR SCHEME FOR INDIVIDUALS
  scale_colour_manual(name = "ID", values = c(
    "BE_M1" = "cyan3", "BE_M2" = "darkorange2", "BE_M3" = "magenta2",
    "BE_M4" = "chartreuse3", "NL_M1" = "red2", "NL_M2" = "gold2",
    "SW_F1" = "cadetblue3", "SW_F3" = "tomato3", "SW_M1" = "springgreen2",
    "SW_M2" = "khaki2"
  )) +
  # layout figure
  scale_x_continuous(
    breaks = c(
      0, 0 + 31, 0 + 31 + 28, 0 + 2 * 31 + 28, 0 + 28 + 2 * 31 + 1 * 30, 0 + 29 + 3 * 31 + 1 * 30, 0 + 29 + 3 * 31 + 2 * 30,
      0 + 29 + 4 * 31 + 2 * 30, 0 + 29 + 4 * 31 + 3 * 30, 0 + 29 + 5 * 31 + 3 * 30, 0 + 29 + 5 * 31 + 4 * 30, 0 + 29 + 6 * 31 + 4 * 30
    ),
    labels = c("Jul", "Aug", "Sep", "Okt", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
  ) +
  theme_bw() +
  xlab("\nDay of Year") +
  ylab("Lat[º]\n") +
  ## Layout text items
  theme(
    legend.position = "none",
    legend.direction = "vertical",
    panel.border = element_rect(colour = "white"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold")
  )

ggsave(here("reports", "figures", "FigS1_Lat-vs-yday.tiff"), dpi = 300, width = 8, height = 4)
