## Location Heatmaps

zone_path <- tibble(
  plate_x = c(-0.85, -0.85, 0.85, 0.85, -0.85),
  plate_z = c(1.6, 3.5, 3.5, 1.6, 1.6)
)

pitch_savant <- read.csv("cards_savant_data.csv")

pitch_savant_fb <- pitch_savant %>% 
  filter(pitch_name == "4-Seam Fastball" | pitch_name == "Sinker" | pitch_name == "Cutter") %>% 
  select(plate_x, plate_z)

pitch_savant_break <- pitch_savant %>% 
  filter(pitch_name == "Slider" | pitch_name == "Curveball" | pitch_name == "Sweeper") %>% 
  select(plate_x, plate_z)

pitch_savant_off <- pitch_savant %>% 
  filter(pitch_name == "Changeup" | pitch_name == "Split-finger") %>% 
  select(plate_x, plate_z)

cardinals_pitchers_fb <- ggplot(data = pitch_savant_fb, 
                                aes(x = plate_x, y = plate_z)) + 
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(density)),
    contour = FALSE,
    show.legend = FALSE
  ) + 
  scale_fill_gradient(low = "gray", high = "red") + 
  geom_path(data = zone_path, color = "black", linewidth = .8) + 
  ylim(0.5, 4.25) +
  xlim(-1.7, 1.7) + 
  coord_fixed() + 
  theme_bw() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 12, hjust = .5, face = "bold")
  ) + 
  labs(x = " ",
       y = " ", 
       title = "Fastballs")

cardinals_pitchers_break <- ggplot(data = pitch_savant_break, 
                                   aes(x = plate_x, y = plate_z)) + 
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(density)),
    contour = FALSE,
    show.legend = FALSE
  ) + 
  scale_fill_gradient(low = "gray", high = "red") + 
  geom_path(data = zone_path, color = "black", linewidth = .8) + 
  ylim(0.5, 4.25) +
  xlim(-1.7, 1.7) + 
  coord_fixed() + 
  theme_bw() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 12, hjust = .5, face = "bold")
  ) + 
  labs(x = " ",
       y = " ", 
       title = "Breaking")

cardinals_pitchers_off <- ggplot(data = pitch_savant_off, 
                                 aes(x = plate_x, y = plate_z)) + 
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(density)),
    contour = FALSE,
    show.legend = FALSE
  ) + 
  scale_fill_gradient(low = "gray", high = "red") + 
  geom_path(data = zone_path, color = "black", linewidth = .8) + 
  ylim(0.5, 4.25) +
  xlim(-1.7, 1.7) + 
  coord_fixed() + 
  theme_bw() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 12, hjust = .5, face = "bold")
  ) + 
  labs(x = " ",
       y = " ", 
       title = "Offspeed")

grid.arrange(cardinals_pitchers_fb, cardinals_pitchers_break, cardinals_pitchers_off, 
             ncol = 3,
             bottom = textGrob("Data from Statcast", gp = gpar(fontsize = 6)))

pitch_savant_fb_2 <- pitch_savant %>% 
  filter(pitch_name == "4-Seam Fastball" | pitch_name == "Sinker" | pitch_name == "Cutter") %>% 
  filter(strikes == 2) %>% 
  select(plate_x, plate_z)

pitch_savant_break_2 <- pitch_savant %>% 
  filter(pitch_name == "Slider" | pitch_name == "Curveball" | pitch_name == "Sweeper") %>% 
  filter(strikes == 2) %>% 
  select(plate_x, plate_z)

pitch_savant_off_2 <- pitch_savant %>% 
  filter(pitch_name == "Changeup" | pitch_name == "Split-finger") %>% 
  filter(strikes == 2) %>% 
  select(plate_x, plate_z)

cardinals_pitchers_fb_2 <- ggplot(data = pitch_savant_fb_2, 
                                  aes(x = plate_x, y = plate_z)) + 
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(density)),
    contour = FALSE,
    show.legend = FALSE
  ) + 
  scale_fill_gradient(low = "gray", high = "red") + 
  geom_path(data = zone_path, color = "black", linewidth = .8) + 
  ylim(0.5, 4.25) +
  xlim(-1.7, 1.7) + 
  coord_fixed() + 
  theme_bw() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 12, hjust = .5, face = "bold")
  ) + 
  labs(x = " ",
       y = " ", 
       title = "Fastballs")

cardinals_pitchers_break_2 <- ggplot(data = pitch_savant_break_2, 
                                     aes(x = plate_x, y = plate_z)) + 
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(density)),
    contour = FALSE,
    show.legend = FALSE
  ) + 
  scale_fill_gradient(low = "gray", high = "red") + 
  geom_path(data = zone_path, color = "black", linewidth = .8) + 
  ylim(0.5, 4.25) +
  xlim(-1.7, 1.7) + 
  coord_fixed() + 
  theme_bw() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 12, hjust = .5, face = "bold")
  ) + 
  labs(x = " ",
       y = " ", 
       title = "Breaking")

cardinals_pitchers_off_2 <- ggplot(data = pitch_savant_off_2, 
                                   aes(x = plate_x, y = plate_z)) + 
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(density)),
    contour = FALSE,
    show.legend = FALSE
  ) + 
  scale_fill_gradient(low = "gray", high = "red") + 
  geom_path(data = zone_path, color = "black", linewidth = .8) + 
  ylim(0.5, 4.25) +
  xlim(-1.7, 1.7) + 
  coord_fixed() + 
  theme_bw() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 12, hjust = .5, face = "bold")
  ) + 
  labs(x = " ",
       y = " ", 
       title = "Offspeed")

grid.arrange(cardinals_pitchers_fb_2, cardinals_pitchers_break_2, cardinals_pitchers_off_2, 
             ncol = 3, 
             top = textGrob("With 2 Strikes", gp = gpar(fontsize = 18, fontface = "bold")),
             bottom = textGrob("Data from Statcast", gp = gpar(fontsize = 6)))
