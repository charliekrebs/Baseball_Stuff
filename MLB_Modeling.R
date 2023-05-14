## Modeling 2022 WAR for Pitchers

# Data
savant_data <- read.csv("savantpitch22.csv")
fangraphs_data <- read.csv("fgpitch22.csv")

# Data Cleaning
savant_data$name <- paste(savant_data$first_name, savant_data$last_name)
savant_data$Name <- trimws(savant_data$name)
savant_data <- savant_data %>% 
  rename("IP" = "p_formatted_ip", 
         "SO" = "p_strikeout", 
         "BB" = "p_walk", 
         "K." = "p_k_percent", 
         "BB." = "p_bb_percent", 
         "BA" = "batting_avg", 
         "SLG" = "slg_percent", 
         "xBA" = "xba", 
         "xSLG" = "xslg", 
         "wOBA" = "woba", 
         "xwOBA" = "xwoba", 
         "wOBACON" = "wobacon", 
         "xwOBACON" = "xwobacon", 
         "xISO" = "xiso", 
         "EV" = "exit_velocity_avg", 
         "LA" = "launch_angle_avg", 
         "Barrel." = "barrel_batted_rate"
         )
savant_data <- savant_data %>% 
  select(Name, IP, SO, BB, K., BB., BA, xBA, SLG, xSLG, wOBA, xwOBA, wOBACON, xwOBACON, xISO, EV, LA, Barrel.) %>% 
  arrange(Name)

fangraphs_data <- fangraphs_data %>% 
  select(Name, IP, K.9, BB.9, BABIP, FIP, xFIP, WAR) %>% 
  arrange(Name)

comb_data <- merge(savant_data, fangraphs_data)

# Modeling

pre_process_mod <- preProcess(comb_data, method = c("YeoJohnson", "center", "scale"))
dat_processed <- predict(pre_process_mod, newdata = comb_data)  

ind_train <- sample(1:45, .7 * 45)
dat_train <- dat_processed[ind_train,]
dat_test <- dat_processed[-ind_train,]

train_mod <- lm(WAR ~ . -Name, data = dat_train)
test_mod <- lm(WAR ~ . -Name , data = dat_test)

plot(train_mod) # Could be a few outliers
plot(test_mod)

cv_5 <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5)

aic_mod <- train(WAR ~ . -Name, 
                 data = dat_train,
                 method = "lmStepAIC",
                 trControl = cv_5,
                 trace = 0)
ridge_mod <- train(WAR ~ . -Name, 
                   data = dat_train, 
                   trControl = cv_5, 
                   method = "ridge")
glm_mod <- train(WAR ~ . -Name, 
                 data = dat_train, 
                 trControl = cv_5, 
                 method = "glm")

aic_mod$results
ridge_mod$results
glm_mod$results

summary(aic_mod)
summary(ridge_mod)
summary(glm_mod)

# xwOBA, xwOBACON, K.9, BB.9, FIP most significant
# All 3 models showed similar results. I would probably use the AIC model as it seems more consistent than the ridge and glm. Small data sample probably causes some issues.
