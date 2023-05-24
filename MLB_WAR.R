# WAR Charts

team_bat_war <- fg_team_batter(x = 2023, y = 2023)
team_pitch_war <- fg_team_pitcher(x = 2023, y = 2023)

team_bat_war <- team_bat_war %>% 
  arrange(Team)
team_pitch_war <- team_pitch_war %>% 
  arrange(Team)
team_abbr <- valid_team_names()
team_abbr <- team_abbr[!team_abbr %in% c("NL", "AL", "MLB")]
team_abbr[2] <- "ARI"
team_abbr[9] <- "CHW"
team_abbr <- sort(team_abbr)

war2023 <- data.frame(
  teams = team_abbr, 
  team_bat_war = team_bat_war$WAR, 
  team_pitch_war = team_pitch_war$WAR)

ggplot(data = war2023, 
       aes(x = team_bat_war, y = team_pitch_war)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .03) + 
  geom_hline(yintercept = mean(war2023$team_pitch_war), color = "red", linetype = "dashed", alpha = .5) + 
  geom_vline(xintercept = mean(war2023$team_bat_war), color = "red", linetype = "dashed", alpha = .5) + 
  labs(x = "Batting WAR", 
       y = "Pitching WAR", 
       title = "2023 Season Team fWAR", 
       caption = "Data from FanGraphs") + 
  theme(plot.title = element_text(size = 14, hjust = .5, face = "bold")) + 
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    axis.title.x = element_text(color = "black", size = 13, face = "bold"),
    axis.title.y = element_text(color = "black", size = 13, face = "bold")
  )

wrc_fip <- data.frame(
  teams = team_abbr, 
  team_wRC_plus = team_bat_war$wRC_plus, 
  team_FIP = team_pitch_war$FIP)

ggplot(data = wrc_fip, 
       aes(x = team_wRC_plus, y = team_FIP)) + 
  geom_mlb_logos(aes(team_abbr = teams), width = .03) + 
  geom_hline(yintercept = mean(wrc_fip$team_FIP), color = "red", linetype = "dashed", alpha = .5) + 
  geom_vline(xintercept = mean(wrc_fip$team_wRC_plus), color = "red", linetype = "dashed", alpha = .5) + 
  labs(x = "wRC+", 
       y = "FIP", 
       title = "2023 Team FIP vs wRC+", 
       caption = "Data from FanGraphs") + 
  theme(plot.title = element_text(size = 14, hjust = .5, face = "bold")) + 
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    axis.title.x = element_text(color = "black", size = 13, face = "bold"),
    axis.title.y = element_text(color = "black", size = 13, face = "bold")
  )

ggplot(data = team_bat_war, 
       aes(x = reorder(Team, -Fld), y = Fld)) + 
  geom_bar(stat = "identity", width = .75, color = "black", fill = "white") + 
  geom_text(aes(label = Fld), vjust = 1.5) + 
  labs(x = "Team", 
       y = "Def", 
       title = "2023 Team Defensive Runs Above Average ", 
       caption = "Data from FanGraphs")

