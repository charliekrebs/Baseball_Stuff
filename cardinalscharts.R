data <- daily_batter_bref("2022-07-22","2022-08-18")

data %>% 
  filter(PA > 25, Team == "St. Louis") %>% 
  ggplot(aes(x = OBP, y = SLG)) + 
  geom_point(aes(cex = PA, alpha = .5)) +
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .01) + 
  theme(legend.position = "none") +
  labs(title = "Slugging % vs. On-Base %", subtitle = "Dot size indicates PA", caption = "All data from baseballr")


data2 <- daily_pitcher_bref("2022-04-07","2022-08-08")

data2 %>%
  filter(IP > 2.0, Team == "St. Louis") %>%
  fip_plus %>%
  select(Name, Team, G, IP, ER, WHIP, BAbip, wOBA_against, wOBA_CON_against, FIP) %>%
  ggplot(aes(x = wOBA_against, y = ER)) + 
  geom_point(aes(cex = IP, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .5) +  
  theme(legend.position = "none") +
  labs(x = "wOBA", y = "ER", title = "Earned Runs vs. Weighted On-Base Average over the Last Month (6/6 - 7/6)", 
       subtitle = "Dot size indicates IP", caption = "All data from baseballr")

data %>% 
  filter(PA > 25, Team == "St. Louis") %>% 
  woba_plus %>%
  select(Name, Team, PA, R, wOBA, wOBA_CON, OPS, SLG) %>%
  ggplot(aes(x = SLG, y = wOBA_CON)) + 
  geom_point(aes(cex = PA, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .007) +  
  theme(legend.position = "none") +
  labs(x = "R/PA", y = "wOBA", title = "Weighted On-Base Average vs. Runs per Plate Appearances", 
       subtitle = "Dot size indicates PA", caption = "All data from baseballr")

fg_data <- fg_bat_leaders(x = 2022, y = 2022, league = "nl")
fg_data %>% 
  filter(Team == "STL") %>% 
  filter(PA > 50) %>% 
  ggplot(aes(x = ISO, y = wRC_plus)) + 
  geom_point(aes(cex = PA, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = 4) +  
  theme(legend.position = "none") +
  labs(x = "ISO", y = "wRC+", title = "Weighted Runs Created Plus vs. Isolated Power", 
       subtitle = "Dot size indicates PA", caption = "All data from baseballr and FanGraphs")

fg_data <- read.csv("FanGraphs Leaderboard.csv") 
fg_data %>% 
  filter(PA > 50) %>% 
  ggplot(aes(x = ISO, y = wRC.)) + 
  geom_point(aes(cex = PA, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .2) +  
  theme(legend.position = "none") +
  labs(x = "ISO", y = "wRC+", title = "Weighted Runs Created Plus vs. Isolated Power", 
       subtitle = "Dot size indicates PA", caption = "All data from baseballr and FanGraphs")

data2 %>% 
  filter(Team == "St. Louis", IP > 10.0) %>% 
  ggplot(aes(x = Str, y = WHIP)) + 
  geom_point(aes(cex = IP, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .025) +  
  theme(legend.position = "none") +
  labs(x = "Strike %", y = "WHIP", title = "Walks+Hits per Innings Pitched vs. Strike %", 
       subtitle = "Dot size indicates IP", caption = "All data from baseballr")
   

fg_pitch_data <- fg_pitch_leaders(x = 2022, y = 2022, league = "nl")
fg_pitch_data %>% 
  filter(Team == "STL", IP > 10.0) %>% 
  ggplot(aes(x = xFIP, y = WPA)) + 
  geom_point(aes(cex = IP, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .025) +  
  theme(legend.position = "none") +
  labs(x = "xFIP", y = "WPA", title = "", 
       subtitle = "Dot size indicates IP", caption = "All data from baseballr and FanGraphs")

fg_minor_bat <- read.csv("fangraphs-minor-league-leaders.csv")
fg_minor_bat %>% 
  filter(PA > 100) %>% 
  ggplot(aes(x = BABIP, y = wRC.)) + 
  geom_point(aes(cex = PA, alpha = .5, color = Level)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = 2.5) +  
  guides(cex = "none", alpha = "none") + 
  scale_color_manual(values = c("#e9f235", "#2803fc", "#0d9424", "#fcba03", "#a103fc", "#544e4e", "#87ace6", "#fc0303")) + 
  labs(x = "BABIP", y = "wRC+", title = "Weighted Runs Created Plus vs. Batting Average on Balls in Play", 
       subtitle = "Dot size indicates PA", caption = "All data from baseballr and FanGraphs")

data %>% 
  filter(Name == "Albert Pujols") %>% 
  select(PA, AB, H, HR, RBI, BA, OBP, SLG, OPS) 

fg_bat_2022 <- fg_batter_leaders(x = 2022, y = 2022, league = "nl", exc_p = TRUE)

payrolls2023 %>% 
  ggplot(aes(reorder(team, payroll, mean), y = payroll, color = teamcolor)) + 
  geom_point(stat = "identity", size = 4) + 
  coord_flip()

team_abbr <- valid_team_names()
team_abbr <- team_abbr[!team_abbr %in% c("NL", "AL", "MLB")]



# keep alpha == 1 for all teams including an "A"
matches <- grepl("A", team_abbr)
df$alpha <- ifelse(matches, 1, 0.2)
# also set a custom fill colour for the non "A" teams
df$colour <- ifelse(matches, NA, "gray")

# scatterplot of all logos
ggplot(df, aes(x = a, y = b)) +
  geom_mlb_logos(aes(team_abbr = teams), width = 0.075) +
  geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
  theme_void()


salary_war_2022 <- data.frame(player = c("Paul Goldschmidt", "Nolan Arenado", "Tommy Edman", "Brendan Donovan", "Lars Nootbaar", "Dylan Carlson", "Albert Pujols", "Tyler O'Neill", "Nolan Gorman", "Corey Dickerson", "Yadier Molina", "Paul DeJong", "Juan Yepez", "Andrew Knizner", "Harrison Bader"),
                              PA = c(651, 620, 630, 468, 347, 488, 351, 383, 313, 297, 270, 237, 274, 293, 264),
                              fwar = c(7.1, 7.3, 5.6, 2.7, 2.7, 2.4, 1.8, 1.3, .5, .4, .1, .1, .1, -.3, 1.5),
                              salary = c(26.3, 35, .72, .7, .7, .7, 2.6, 3.4, .7, 5, 10, 6.1, .7, .72, 3.5))

cards_2022_fg <- fg_batter_leaders(x = 2022, y = 2022, qual = 237) %>% 
                  filter(Team == "STL") %>% 
                  select(Name, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, BB, SO, AVG, BB_K, OBP, SLG, BABIP, wOBA, WPA, wRC)

fg_plus_data <- read.csv("fg_plus_data.csv") 

cards2022_plus_data <- fg_plus_data %>% 
  select(Name, PA, BB.., K..) %>% 
  arrange(-PA) %>% 
  filter(!row_number() %in% c(7, 10, 13, 14))

cards2023_plus_data <- rbind(cards2022_plus_data, c("Willson Contreras", 487, 111, 94))

cards2023_plus_data <- cards2023_plus_data %>% 
  mutate_at(c("PA", "BB..", "K.."), as.integer)

cards2023_plus_data <- cards2023_plus_data %>% 
  arrange(-PA)
  
flextable(data = cards2023_plus_data) %>% 
  set_header_labels(
    Name = "",
    PA = "PA",
    BB.. = "BB%+",
    K.. = "K%+"
  ) %>% 
  bold(bold = TRUE, j = 1) %>% 
  bold(bold = TRUE, part = "header") %>% 
  autofit() 

set_flextable_defaults(
    font.family = "sHelvetica", font.size = 10,
    text.align = "center",
    border.color = "#e5383b",
    padding = 8,
    background.color = "#EFEFEF")
  

fg_statcast_2022 <- read.csv("fg_stat.csv")

fg_statcast_2022 %>% 
  ggplot(aes(x = (Barrels / PA), y = (xwOBA - wOBA))) + 
  geom_point(aes(cex = PA, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .0025) 

dff1 <- bref_daily_batter("2022-04-07", "2022-07-16")
dff2 <- bref_daily_batter("2022-07-22", "2022-10-05")

lars1 <- woba_plus(dff1) %>% 
  filter(Name == "Lars Nootbaar") %>% 
  select(PA, BA, OBP, SLG, wOBA)

lars2 <- woba_plus(dff2) %>% 
  filter(Name == "Lars Nootbaar") %>% 
  select(PA, BA, OBP, SLG, wOBA)

rc_data <- read.csv("fg_wRC_data.csv") %>% 
  select(Team, wRC) 

rc_data1 <- rc_data[order(rc_data$Team),] %>% 
  mutate(wRC2 = c(751, 835, 747, 786, 669, 796, 718, 773, 774, 695, 898, 730, 799, 813, 669, 741, 775, 779, 783, 665, 762, 728, 786, 763, 737, 802, 776, 719, 818, 705)) %>% 
  mutate_at(c("wRC2"), as.integer) %>% 
  mutate(chg = wRC2 - wRC)

steamer_rc_table <- flextable(data = rc_data1) %>% 
  set_header_labels(
    Team = "Team",
    wRC = "2022 wRC",
    wRC2 = "2023 Projected wRC",
    chg = "Projected Difference"
  ) %>% 
  bold(bold = TRUE, j = 1) %>% 
  bold(bold = TRUE, part = "header") %>% 
  autofit() %>% 
  footnote(i = 1, j = 3:4,
            value = as_paragraph(
              c("Projections from Steamer (FanGraphs)")
            ),
            ref_symbols = c("*"),
           part = "header")
steamer_rc_table

rc_data2 <- rc_data[order(rc_data$Team),] %>% 
  mutate(wRC3 = c(720, 826, 671, 749, 701, 746, 703, 740, 774, 636, 886, 652, 831, 850, 607, 739, 826, 707, 751, 567, 735, 719, 805, 781, 725, 778, 695, 698, 848, 653)) %>% 
  mutate_at(c("wRC3"), as.integer) %>% 
  mutate(chg = wRC3 - wRC)

thebat_rc_table <- flextable(data = rc_data2) %>% 
  set_header_labels(
    Team = "Team",
    wRC = "2022 wRC",
    wRC3 = "2023 Projected wRC",
    chg = "Projected Difference"
  ) %>% 
  bold(bold = TRUE, j = 1) %>% 
  bold(bold = TRUE, part = "header") %>% 
  autofit() %>% 
  footnote(i = 1, j = 3:4,
           value = as_paragraph(
             c("Projections from THE BAT (FanGraphs)")
           ),
           ref_symbols = c("*"),
           part = "header")
thebat_rc_table

mean(rc_data$wRC)
mean(rc_data1$wRC2)
mean(rc_data2$wRC3)

fg_stat_pitch <- read.csv("fg_statcast_pitch.csv")

fg_stat_pitch %>% 
  ggplot(aes(x = K.., y = BB..)) + 
  geom_point(aes(cex = IP, alpha = .5)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = 2) +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed", alpha=0.5) + 
  geom_vline(xintercept = 100, color = "red", linetype = "dashed", alpha=0.5) + 
  theme(legend.position = "none") + 
  labs(x = "K%+",
       y = "BB%+", 
       title = "2022 Walk and Strikeout Percentages Compared to the MLB Average",
       subtitle = "Dot size indicates IP (min. 20 IP)", caption = "Data from FanGraphs")
  

fl_18 <- data.frame(pitch_type = c("4-Seam Fastball", "Slider", "Curveball", "Sinker", "Changeup"), 
                    pitch_num = c(1034, 774, 289, 412, 77), 
                    woba = c(.285, .282, .190, .332, .176))

fl_19 <- data.frame(pitch_type = c("4-Seam Fastball", "Slider", "Curveball", "Sinker", "Changeup"), 
                    pitch_num = c(1477, 876, 383, 379, 64), 
                    woba = c(.261, .244, .257, .218, .373))

fl_20 <- data.frame(pitch_type = c("4-Seam Fastball", "Slider", "Curveball", "Sinker", "Changeup"), 
                    pitch_num = c(318, 206, 98, 82, 15), 
                    woba = c(.328, .258, .297, .251, .530))

fl_21 <- data.frame(pitch_type = c("4-Seam Fastball", "Slider", "Curveball", "Sinker", "Changeup"), 
                    pitch_num = c(685, 383, 177, 84, 24), 
                    woba = c(.288, .286, .213, .203, .474))

fl_22 <- data.frame(pitch_type = c("4-Seam Fastball", "Slider", "Curveball", "Sinker", "Changeup"), 
                    pitch_num = c(320, 167, 109, 39, 5), 
                    woba = c(.457, .237, .177, .295, .720))

fl <- data.frame(year = rep(c("2018", "2019", "2020", "2021", "2022"), each = 5), 
                 pitch_type = c("4-Seam Fastball", "Slider", "Curveball", "Sinker", "Changeup"), 
                 woba = c(.285, .282, .190, .332, .176, .261, .244, .257, .218, .373, .328, .258, .297, .251, .530, .288, .286, .213, .203, .474, .457, .237, .177, .295, .720))

fl2 <- data.frame(year = rep(c("2018", "2019", "2020", "2021", "2022"), each = 4), 
                 pitch_type = c("4-Seam Fastball", "Slider", "Curveball", "Sinker"), 
                 woba = c(.285, .282, .190, .332, .261, .244, .257, .218, .328, .258, .297, .251, .288, .286, .213, .203, .457, .237, .177, .295))

ggplot(data = fl, aes(x = year, y = woba, group = pitch_type)) +
  geom_line(aes(color = pitch_type)) +
  geom_point(aes(color = pitch_type)) + 
  labs(x = "Season", 
       y = "wOBA", 
       color = "Pitch Type", 
       title = "Jack Flaherty wOBA by Pitch Type", 
       caption = "Data from Baseball Savant")

ggplot(data = fl2, aes(x = year, y = woba, group = pitch_type)) +
  geom_line(aes(color = pitch_type)) +
  geom_point(aes(color = pitch_type)) + 
  labs(x = "Season", 
       y = "wOBA", 
       color = "Pitch Type", 
       title = "Jack Flaherty wOBA by Pitch Type (Excluding Changeup)", 
       caption = "Data from Baseball Savant")


war2022 <- data.frame(
  teams = team_abbr, 
  bat_war = c(28.9, 19.8, 18.9, 18.0, 15.7, 5.2, 21.7, 6.8, 15.3, 2.5, 29.5, 10.7, 11.9, 38.9, 10.6, 24.3, 21.1, 31.3, 35.1, 6.8, 21.6, 6.7, 21.2, 22.9, 15.6, 32.8, 19.8, 18.9, 31.2, 6.1), 
  pit_war =  c(22.7, 7.7, 11.9, 9.8, 8.7, 9.9, 18.6, 12.6, 17.3, 9.4, 27.0, 7.5, 17.0, 24.6, 14.1, 14.9, 10.7, 20.3, 19.6, 3.6, 22.7, 9.5, 19.3, 13.8, 20.0, 13.4, 16.5, 8.9, 17.0, 1.0)
)

ggplot(data = war2022, 
       aes(x = bat_war, y = pit_war)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .03) + 
  geom_hline(yintercept = mean(war2022$pit_war), color = "red", linetype = "dashed", alpha = .5) + 
  geom_vline(xintercept = mean(war2022$bat_war), color = "red", linetype = "dashed", alpha = .5) + 
  labs(x = "Batting WAR", 
       y = "Pitching WAR", 
       title = "2022 Season Team WAR", 
       caption = "Data from FanGraphs") + 
  theme(plot.title = element_text(size = 14, hjust = .5, face = "bold"))

fg_statcast_2022 <- read.csv("fg_stat.csv")

ggplot(data = fg_statcast_2022, 
       aes(x = (Barrels / PA), y = wOBA)) + 
  geom_point(aes(cex = PA, alpha = .2)) + 
  geom_smooth(method = "lm", se = FALSE, alpha = .5, color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .005) +
  # geom_hline(yintercept = 100, color = "red", linetype = "dashed", alpha=0.5) + 
  # geom_vline(xintercept = 100, color = "red", linetype = "dashed", alpha=0.5) + 
  theme(legend.position = "none") + 
  labs(x = "Barrels / PA",
       y = "wOBA", 
       title = "",
       subtitle = "Dot size indicates PA", caption = "Data from FanGraphs")

pitch_value <- read.csv("pitch-arsenal-stats.csv")
pitch_value$name <- paste(pitch_value$first_name, " ", pitch_value$last_name)

pitch_value <- pitch_value %>% 
  select(name, pitch_name, run_value, pitch_usage) %>% 
  filter(run_value < -5) %>% 
  arrange(run_value) 
pitch_value

set_flextable_defaults(
  font.family = "sHelvetica", font.size = 10,
  text.align = "center",
  border.color = "#e5383b",
  padding = 8,
  background.color = "#EFEFEF")

pitch_value_table <- flextable(data = pitch_value) %>% 
  set_header_labels(
    name = " ",
    pitch_name = "Pitch",
    run_value = "Run Value",
    pitch_usage = "Usage %"
  ) %>% 
  bold(bold = TRUE, j = 1) %>% 
  bold(bold = TRUE, part = "header") %>% 
  autofit() %>% 
  add_footer_lines(1, values = c("Data from Baseball Savant"))
pitch_value_table

pitch_value <- pitch_value %>% 
  select(name, pitch_name, run_value, pitch_usage) %>% 
  filter(run_value > 4) %>% 
  arrange(-run_value) 

pitch_value_table2 <- flextable(data = pitch_value) %>% 
  set_header_labels(
    name = " ",
    pitch_name = "Pitch",
    run_value = "Run Value",
    pitch_usage = "Usage %"
  ) %>% 
  bold(bold = TRUE, j = 1) %>% 
  bold(bold = TRUE, part = "header") %>% 
  autofit() %>% 
  add_footer_lines(1, values = c("Data from Baseball Savant"))
pitch_value_table2

fg_statcast_2022 <- read.csv("fg_stat.csv")

fg_statcast_2022 %>% 
  select(Name, PA, Barrel., wOBA, xwOBA) %>% 
  mutate(luck = xwOBA - wOBA) %>% 
  arrange(-luck)

pitch_exp <- read.csv("expected_stats.csv")
pitch_exp$name <- paste(pitch_exp$first_name, " ", pitch_exp$last_name)

pitch_exp <- pitch_exp %>% 
  select(name, pa, est_ba_minus_ba_diff, est_slg_minus_slg_diff, est_woba_minus_woba_diff, era_minus_xera_diff)
pitch_exp

pitch_plus <- data.frame(player = c("Ryan Helsley", "Giovanny Gallegos", "Chris Stratton", "Steven Matz", "Adam Wainwright", "Miles Mikolas", "Jordan Montgomery", "Jake Woodford", "Jordan Hicks", "Genesis Cabrera", "Zack Thompson", "Drew VerHagen", "Anthony Misiewicz", "Packy Naughton", "Andrew Pallante", "Jack Flaherty", "Matthew Liberatore", "Dakota Hudson", "Jake Walsh", "JoJo Romero"),  
                    stuff = c(145.4, 116.1, 103.0, 99.8, 106.1, 90.1, 99.6, 89.7, 121.4, 99.4, 82.9, 94.1, 96.3, 81.4, 88.4, 96.5, 85.0, 80.6, 99.1, 85.3), 
                    location = c(99.0, 100.4, 102.5, 106.6, 103.8, 106.3, 102.6, 103.7, 92.6, 96.0, 99.4, 92.9, 101.3, 102.2, 97.8, 95.5, 96.6, 97.1, 85.1, 93.3), 
                    pitching = c(112.9, 105.2, 103.4, 103.2, 100.9, 100.8, 99.8, 99.2, 98.4, 97.3, 96.9, 96.8, 95.9, 95.9, 95.5, 94.7, 92.7, 92, 91.6, 91))

ggplot(data = pitch_plus, 
       aes(x = location, y = stuff)) + 
  geom_point()  + 
  geom_text(aes(label = player), size = 3, nudge_y = 1) +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = 100, color = "red", linetype = "dashed") + 
  theme(legend.position = "none") + 
  labs(x = "Location+",
       y = "Stuff+",
       caption = "Data from @enosarris")

pitch_plus_table <- flextable(data = pitch_plus) %>% 
  set_header_labels(
    player = " ",
    stuff = "Stuff+",
    location = "Location+",
    pitching = "Pitching+"
  ) %>% 
  bold(bold = TRUE, j = 1) %>% 
  bold(bold = TRUE, part = "header") %>% 
  autofit() %>% 
  add_footer_lines(1, values = c("Data from @enosarris"))
pitch_plus_table

carlson <- scrape_statcast_savant(start_date = "2022-04-07", end_date = "2022-10-05", playerid = 666185)

cf <- carlson %>% 
  filter(pitch_type != "SL") %>% 
  filter(pitch_type != "CU") %>% 
  filter(pitch_type != "CH") %>% 
  filter(pitch_type != "ST") %>% 
  filter(pitch_type != "KC") %>% 
  filter(pitch_type != "SV") %>% 
  select(pitch_type, estimated_woba_using_speedangle) %>% 
  na.omit()

cf
mean(cf$estimated_woba_using_speedangle)
  

cal_fb <- carlson %>% 
  filter(pitch_type == "CU", "FF", "SI", "FC") %>% 
  select(estimated_woba_using_speedangle, woba_value) %>% 
  na.omit()

mean(cal_fb$estimated_woba_using_speedangle)
mean(cal_fb$woba_value)

burl <- scrape_statcast_savant(start_date = "2022-04-07", end_date = "2022-10-05", playerid = 676475)

burl_fb <- burl %>% 
  filter(release_speed >= 93.0)

batwar <- read.csv("fgbatwar5.csv")
pitwar <- read.csv("fgpitchwar5.csv")
batwar <- batwar %>% 
  arrange(Team)
pitwar <- pitwar %>% 
  arrange(Team)

team_abbr <- valid_team_names()
team_abbr <- team_abbr[!team_abbr %in% c("NL", "AL", "MLB")]
team_abbr[2] <- "ARI"
team_abbr[9] <- "CHW"
team_abbr <- sort(team_abbr)

war2023 <- data.frame(
  teams = team_abbr, 
  bat_war = batwar$WAR, 
  pit_war = pitwar$WAR)

ggplot(data = war2023, 
       aes(x = bat_war, y = pit_war)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .03) + 
  geom_hline(yintercept = mean(war2023$pit_war), color = "red", linetype = "dashed", alpha = .5) + 
  geom_vline(xintercept = mean(war2023$bat_war), color = "red", linetype = "dashed", alpha = .5) + 
  labs(x = "Batting WAR", 
       y = "Pitching WAR", 
       title = "2023 Season Team WAR", 
       caption = "Data from FanGraphs") + 
  theme(plot.title = element_text(size = 14, hjust = .5, face = "bold"))

fieldwar <- read.csv("fgfieldwar5.csv")
fieldwar$Team <- c("MIL", "SEA", "LAA", "ARI", "DET", "TB", "HOU", "NYM", "KC", "SF", "SD", "NYY", "CHC", "TOR", "MIN", "STL", "WSH", "OAK", "PHI", "TEX", "MIA", "LAD", "BOS", "ATL", "CHW", "PIT", "CIN", "CLE", "COL", "BAL")
ggplot(data = fieldwar, 
       aes(x = reorder(Team, -Def), y = Def)) + 
  geom_bar(stat = "identity", width = .75, color = "black", fill = "white") + 
  geom_text(aes(label = Def), vjust = 1.5) + 
  labs(x = "Team", 
       y = "Def", 
       title = "2023 Team Defensive Runs Above Average ", 
       caption = "Data from FanGraphs")


minor_bat <- read.csv("fgminorbat2.csv")
minor_bat1 <- minor_bat %>% 
  select(Name, Level, PA, BB.K, wOBA, wRC.)

ggplot(data = minor_bat1, 
       aes(x = wRC., y = Level)) + 
  geom_point(aes(size = PA, color = wRC.)) + 
  scale_size(range = c(4,10)) + 
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100) + 
  geom_text_repel(aes(label = Name), size = 3, nudge_y = .15) + 
  geom_vline(xintercept = 100, color = "red", linetype = "dashed", alpha = .5) + 
  theme(legend.position = "none", plot.title = element_text(face = "bold")) + 
  labs(x = "wRC+",
       y = "Level", 
       title = "wRC+ for the Farm System",
       subtitle = "Dot size indicates PA", caption = "Data from FanGraphs")

minor_pitch <- read.csv("fgminorpitch2.csv")
minor_pitch1 <- minor_pitch %>% 
  select(Name, Level, IP, K.BB., FIP, xFIP)

mean(minor_pitch1$xFIP)

ggplot(data = minor_pitch1, 
       aes(x = xFIP, y = Level)) + 
  geom_point(aes(size = IP, color = xFIP)) + 
  scale_size(range = c(4,10)) + 
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 4.6) + 
  geom_vline(xintercept = 4.5, color = "red", linetype = "dashed", alpha = .5) +
  geom_text_repel(aes(label = Name), size = 3, nudge_y = .2) + 
  theme(legend.position = "none", plot.title = element_text(face = "bold")) + 
  labs(x = "xFIP",
       y = "Level", 
       title = "xFIP for the Farm System",
       subtitle = "Dot size indicates IP", caption = "Data from FanGraphs")

minorpitchlead <- read.csv("fgminorpitchlead.csv")
quantile(minorpitchlead$xFIP)

babip23 <- read.csv("fgteambabip23.csv") %>% 
  arrange(Team) %>% 
  select(Team, BABIP)
babip22 <- read.csv("fgteambabip22.csv") %>% 
  arrange(Team) %>% 
  select(Team, BABIP)

team_abbr <- valid_team_names()
team_abbr <- team_abbr[!team_abbr %in% c("NL", "AL", "MLB")]
team_abbr[2] <- "ARI"
team_abbr[9] <- "CHW"
team_abbr <- sort(team_abbr)

babip <- data.frame(
  teams = team_abbr, 
  babip23 = babip23$BABIP, 
  babip22 = babip22$BABIP)

mean(babip23$BABIP) - mean(babip22$BABIP)

ggplot(data = babip, 
       aes(x = babip22, y = babip23)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .03) + 
  geom_hline(yintercept = mean(babip23$BABIP), color = "red", linetype = "dashed", alpha = .5) + 
  geom_vline(xintercept = mean(babip22$BABIP), color = "red", linetype = "dashed", alpha = .5) + 
  scale_x_continuous(labels = number_format(accuracy = 0.001)) + 
  labs(x = "2022 BABIP", 
       y = "2023 BABIP", 
       title = "2022 and 2023 BABIP Comparison", 
       caption = "Data from FanGraphs") + 
  theme(plot.title = element_text(size = 14, hjust = .5, face = "bold")) + 
  annotate("text", x = .2675, y = .370, label = "Better in 2023", size = 5.5) + 
  annotate("text", x = .3125, y = .370, label = "Similar Good", size = 5.5) + 
  annotate("text", x = .3125, y = .255, label = "Better in 2022", size = 5.5) + 
  annotate("text", x = .2675, y = .255, label = "Similar Bad", size = 5.5)

teamfip <- read.csv("fgteampitch.csv") %>% 
  arrange(Team)

teamfip1 <- data.frame(
  teams = team_abbr, 
  xFIP = teamfip$xFIP, 
  BABIP = teamfip$BABIP)

ggplot(data = teamfip1, 
       aes(x = xFIP, y = BABIP)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .03) + 
  geom_hline(yintercept = mean(teamfip$BABIP), color = "red", linetype = "dashed", alpha = .5) + 
  geom_vline(xintercept = mean(teamfip$xFIP), color = "red", linetype = "dashed", alpha = .5) + 
  scale_x_continuous(labels = number_format(accuracy = 0.001)) + 
  labs(x = "xFIP", 
       y = "BABIP", 
       title = "2023 Team BABIP vs. xFIP", 
       caption = "Data from FanGraphs") + 
  theme(plot.title = element_text(size = 14, hjust = .5, face = "bold")) 

fg_bat <- read.csv("fg2023allbat.csv")

fg_bat$Pull. <- as.numeric(sub("%","",fg_bat$Pull.))/100
fg_bat$GB. <- as.numeric(sub("%","",fg_bat$GB.))/100
fg_bat$LD. <- as.numeric(sub("%","",fg_bat$LD.))/100

mean(fg_bat$Pull.)
mean(fg_bat$GB.)

fg_bat %>% 
  filter(Pull. >= .42, GB. >= .45)

stuff_loc <- read.csv("fgstuffloc.csv")

ggplot(data = stuff_loc, 
       aes(x = Stuff., y = Location.)) + 
  geom_point(aes(size = IP, alpha = .5)) + 
  geom_point(data = stuff_loc %>% filter(Name == "Miles Mikolas"), color = "red") + 
  geom_point(data = stuff_loc %>% filter(Name == "Adam Wainwright"), color = "red") + 
  geom_point(data = stuff_loc %>% filter(Name == "Jack Flaherty"), color = "red") + 
  geom_point(data = stuff_loc %>% filter(Name == "Jake Woodford"), color = "red") + 
  geom_point(data = stuff_loc %>% filter(Name == "Steven Matz"), color = "red") + 
  geom_point(data = stuff_loc %>% filter(Name == "Jordan Montgomery"), color = "red") + 
  geom_text(aes(label = Name), size = 3, nudge_y = .45) +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = 100, color = "red", linetype = "dashed") + 
  theme(legend.position = "none") + 
  labs(x = "Stuff+",
       y = "Location+", 
       title = "2023 Cardinals Pitching+",
       subtitle = "Dot size indicates IP", caption = "Data from FanGraphs") + 
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    axis.title.x = element_text(color = "black", size = 13, face = "bold"),
    axis.title.y = element_text(color = "black", size = 13, face = "bold")
  )




