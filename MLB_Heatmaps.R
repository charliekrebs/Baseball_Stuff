## TEAM LOCATION HEATMAPS BY PITCH TYPE

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

## BATTER HARD-HIT HEATMAPS

cards_mlb_data <- read.csv("cards_bat_savant_data.csv")

zone_path <- tibble(
  plate_x = c(-0.85, -0.85, 0.85, 0.85, -0.85),
  plate_z = c(1.6, 3.5, 3.5, 1.6, 1.6)
)

hard_hit_heatmap <- function(hitter, data = cards_mlb_data){
  
  cards_mlb_data %>%
    mutate(hard_hit = if_else(launch_speed >= 95, 1, 0)) %>%
    summarize(
      num_hard_hit = sum(hard_hit, na.rm = T),
      .by = player_name
    ) %>%
    filter(num_hard_hit >= 5)
    
    plot_title <- paste0(
      'Hard-Hit ',
      'Heatmap: ',
      hitter
    )
    
    data %>%
      mutate(hard_hit = if_else(launch_speed >= 95, TRUE, FALSE)) %>%
      filter(
        player_name == hitter, 
        hard_hit,
        !is.na(plate_x),
        !is.na(plate_z),
        abs(plate_x) < 1.75,
        plate_z > 0.5,
        plate_z < 4.25
      ) %>%
      ggplot(aes(plate_x, plate_z)) +
      stat_density_2d(
        geom = "raster",
        aes(fill = after_stat(density)),
        contour = FALSE,
        show.legend = FALSE
      ) +
      scale_fill_gradient(low = "gray", high = "red") +
      geom_path(data = zone_path, color = "black", linewidth = .8) +
      xlim(-1.75, 1.75) +
      ylim(0.5, 4.3) +
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
      labs(
        x = " ",
        y = " ",
        title = plot_title
      )
  
}

Paul_Goldschmidt <- hard_hit_heatmap("Goldschmidt, Paul")
Nolan_Arenado <- hard_hit_heatmap("Arenado, Nolan")
Willson_Contreras <- hard_hit_heatmap("Contreras, Willson")
Nolan_Gorman <- hard_hit_heatmap("Gorman, Nolan")
Brendan_Donovan <- hard_hit_heatmap("Donovan, Brendan")
Tommy_Edman <- hard_hit_heatmap("Edman, Tommy")
Paul_DeJong <- hard_hit_heatmap("DeJong, Paul")
Lars_Nootbaar <- hard_hit_heatmap("Nootbaar, Lars")
Jordan_Walker <- hard_hit_heatmap("Walker, Jordan")
Alec_Burleson <- hard_hit_heatmap("Burleson, Alec")
Dylan_Carlson <- hard_hit_heatmap("Carlson, Dylan")
Andrew_Knizner <- hard_hit_heatmap("Knizner, Andrew")

grid.arrange(Paul_Goldschmidt, Nolan_Arenado, Willson_Contreras, Nolan_Gorman, Brendan_Donovan, Tommy_Edman,
             Paul_DeJong, Lars_Nootbaar, Jordan_Walker, Alec_Burleson, Dylan_Carlson, Andrew_Knizner, ncol = 4, nrow = 3)

## PITCHER HEATMAPS BY PITCH TYPE

## FASTBALLS
pitch_mlb_data <- read.csv("cards_savant_data.csv")

pitch_fb_heatmap <- function(pitcher_choice, data = pitch_mlb_data){
    
    plot_title <- paste0(
      'Fastball ',
      'Heatmap: ',
      pitcher_choice
    )
    
    data %>%
      filter(
        player_name == pitcher_choice, 
        pitch_name == "4-Seam Fastball" | pitch_name == "Sinker" | pitch_name == "Cutter",
        !is.na(plate_x),
        !is.na(plate_z),
        abs(plate_x) <= 1.75,
        plate_z > 0,
        plate_z <= 4.25
      ) %>%
      ggplot(aes(plate_x, plate_z)) +
      stat_density_2d(
        geom = "raster",
        aes(fill = after_stat(density)),
        contour = FALSE,
        show.legend = FALSE
      ) +
      scale_fill_gradient(low = "gray", high = "red") +
      geom_path(data = zone_path, color = "black", linewidth = .8) +
      ylim(0, 4.3) +
      xlim(-1.75, 1.75) +
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
      labs(
        x = " ",
        y = " ",
        title = plot_title
      )
  
}

Miles_Mikolas <- pitch_fb_heatmap("Mikolas, Miles")
Jordan_Montgomery <- pitch_fb_heatmap("Montgomery, Jordan")
Jack_Flaherty <- pitch_fb_heatmap("Flaherty, Jack")
Steven_Matz <- pitch_fb_heatmap("Matz, Steven")
Adam_Wainwright <- pitch_fb_heatmap("Wainwright, Adam")
Chris_Stratton <- pitch_fb_heatmap("Stratton, Chris")
Jake_Woodford <- pitch_fb_heatmap("Woodford, Jake")
Drew_VerHagen <- pitch_fb_heatmap("VerHagen, Drew")
Jordan_Hicks <- pitch_fb_heatmap("Hicks, Jordan")
Giovanny_Gallegos <- pitch_fb_heatmap("Gallegos, Giovanny")
Matthew_Liberatore <- pitch_fb_heatmap("Liberatore, Matthew")
Andre_Pallante <- pitch_fb_heatmap("Pallante, Andre")
Genesis_Cabrera <- pitch_fb_heatmap("Cabrera, Génesis")
Ryan_Helsley <- pitch_fb_heatmap("Helsley, Ryan")
James_Naile <- pitch_fb_heatmap("Naile, James")
Zack_Thompson <- pitch_fb_heatmap("Thompson, Zack")

grid.arrange(Miles_Mikolas, Jordan_Montgomery, Jack_Flaherty, Steven_Matz, Adam_Wainwright, Matthew_Liberatore, 
             ncol = 3, 
             nrow = 2)

grid.arrange(Chris_Stratton, Jake_Woodford, Drew_VerHagen, Jordan_Hicks, Giovanny_Gallegos, Andre_Pallante, 
             Genesis_Cabrera, Ryan_Helsley, James_Naile, Zack_Thompson, 
             ncol = 5, 
             nrow = 2)

## BREAKING BALLS

pitch_break_heatmap <- function(pitcher_choice, data = pitch_mlb_data){
  
  plot_title <- paste0(
    'Breaking ',
    'Heatmap: ',
    pitcher_choice
  )
  
  data %>%
    filter(
      player_name == pitcher_choice, 
      pitch_name == "Slider" | pitch_name == "Curveball" | pitch_name == "Sweeper",
      !is.na(plate_x),
      !is.na(plate_z),
      abs(plate_x) <= 1.75,
      plate_z > 0,
      plate_z <= 4.25
    ) %>%
    ggplot(aes(plate_x, plate_z)) +
    stat_density_2d(
      geom = "raster",
      aes(fill = after_stat(density)),
      contour = FALSE,
      show.legend = FALSE
    ) +
    scale_fill_gradient(low = "gray", high = "red") +
    geom_path(data = zone_path, color = "black", linewidth = .8) +
    ylim(0, 4.3) +
    xlim(-1.75, 1.75) +
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
    labs(
      x = " ",
      y = " ",
      title = plot_title
    )
  
}

Miles_Mikolas2 <- pitch_break_heatmap("Mikolas, Miles")
Jordan_Montgomery2 <- pitch_break_heatmap("Montgomery, Jordan")
Jack_Flaherty2 <- pitch_break_heatmap("Flaherty, Jack")
Steven_Matz2 <- pitch_break_heatmap("Matz, Steven")
Adam_Wainwright2 <- pitch_break_heatmap("Wainwright, Adam")
Chris_Stratton2 <- pitch_break_heatmap("Stratton, Chris")
Jake_Woodford2 <- pitch_break_heatmap("Woodford, Jake")
Drew_VerHagen2 <- pitch_break_heatmap("VerHagen, Drew")
Jordan_Hicks2 <- pitch_break_heatmap("Hicks, Jordan")
Giovanny_Gallegos2 <- pitch_break_heatmap("Gallegos, Giovanny")
Matthew_Liberatore2 <- pitch_break_heatmap("Liberatore, Matthew")
Andre_Pallante2 <- pitch_break_heatmap("Pallante, Andre")
Genesis_Cabrera2 <- pitch_break_heatmap("Cabrera, Génesis")
Ryan_Helsley2 <- pitch_break_heatmap("Helsley, Ryan")
James_Naile2 <- pitch_break_heatmap("Naile, James")
Zack_Thompson2 <- pitch_break_heatmap("Thompson, Zack")

grid.arrange(Miles_Mikolas2, Jordan_Montgomery2, Jack_Flaherty2, Steven_Matz2, Adam_Wainwright2, Matthew_Liberatore2, 
             ncol = 3, 
             nrow = 2)

grid.arrange(Chris_Stratton2, Jake_Woodford2, Drew_VerHagen2, Jordan_Hicks2, Giovanny_Gallegos2, Andre_Pallante2, 
             Genesis_Cabrera2, Ryan_Helsley2, James_Naile2, Zack_Thompson2, 
             ncol = 5, 
             nrow = 2)

## OFFSPEAD PITCHES

pitch_off_heatmap <- function(pitcher_choice, data = pitch_mlb_data){
  
  plot_title <- paste0(
    'Offspeed ',
    'Heatmap: ',
    pitcher_choice
  )
  
  data %>%
    filter(
      player_name == pitcher_choice, 
      pitch_name == "Changeup" | pitch_name == "Split-finger",
      !is.na(plate_x),
      !is.na(plate_z),
      abs(plate_x) <= 1.75,
      plate_z > 0,
      plate_z <= 4.25
    ) %>%
    ggplot(aes(plate_x, plate_z)) +
    stat_density_2d(
      geom = "raster",
      aes(fill = after_stat(density)),
      contour = FALSE,
      show.legend = FALSE
    ) +
    scale_fill_gradient(low = "gray", high = "red") +
    geom_path(data = zone_path, color = "black", linewidth = .8) +
    ylim(0, 4.3) +
    xlim(-1.75, 1.75) +
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
    labs(
      x = " ",
      y = " ",
      title = plot_title
    )
  
}

Miles_Mikolas3 <- pitch_off_heatmap("Mikolas, Miles")
Jordan_Montgomery3 <- pitch_off_heatmap("Montgomery, Jordan")
Jack_Flaherty3 <- pitch_off_heatmap("Flaherty, Jack")
Steven_Matz3 <- pitch_off_heatmap("Matz, Steven")
Adam_Wainwright3 <- pitch_off_heatmap("Wainwright, Adam")
Chris_Stratton3 <- pitch_off_heatmap("Stratton, Chris")
Jake_Woodford3 <- pitch_off_heatmap("Woodford, Jake")
Drew_VerHagen3 <- pitch_off_heatmap("VerHagen, Drew")
Jordan_Hicks3 <- pitch_off_heatmap("Hicks, Jordan")
Giovanny_Gallegos3 <- pitch_off_heatmap("Gallegos, Giovanny")
Matthew_Liberatore3 <- pitch_off_heatmap("Liberatore, Matthew")
Genesis_Cabrera3 <- pitch_off_heatmap("Cabrera, Génesis")
James_Naile3 <- pitch_off_heatmap("Naile, James")

grid.arrange(Miles_Mikolas3, Jordan_Montgomery3, Jack_Flaherty3, Steven_Matz3, Adam_Wainwright3, Matthew_Liberatore3, 
             ncol = 3, 
             nrow = 2)

grid.arrange(Chris_Stratton3, Jake_Woodford3, Drew_VerHagen3, Jordan_Hicks3, Giovanny_Gallegos3,
             Genesis_Cabrera3, James_Naile3, 
             ncol = 4, 
             nrow = 2)
