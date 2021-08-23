library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsoccer)
library(zoo)

## data taken from here:
# https://github.com/metrica-sports/sample-data

data_home <- read.csv("./Sample_Game_1/Sample_Game_1_RawTrackingData_Home_Team.csv")
data_home <- data_home[-1, ]
names(data_home) <- as.character(unlist(data_home[1,]))
data_home <- data_home[-1, ]
colnames(data_home) <- c("period", "frame", "time", 
                         "player11_x", "player11_y",
                         "player1_x", "player1_y",
                         "player2_x", "player2_y",
                         "player3_x", "player3_y",
                         "player4_x", "player4_y",
                         "player5_x", "player5_y",
                         "player6_x", "player6_y",
                         "player7_x", "player7_y",
                         "player8_x", "player8_y",
                         "player9_x", "player9_y",
                         "player10_x", "player10_y",
                         "player12_x", "player12_y",
                         "player13_x", "player13_y",
                         "player14_x", "player14_y",
                         "ball_x", "ball_y")

data_away <- read.csv("./Sample_Game_1/Sample_Game_1_RawTrackingData_Away_Team.csv")
data_away <- data_away[-1, ]
names(data_away) <- as.character(unlist(data_away[1,]))
data_away <- data_away[-1, ]
colnames(data_away) <- c("period", "frame", "time", 
                         "player25_x", "player25_y",
                         "player15_x", "player15_y",
                         "player16_x", "player16_y",
                         "player17_x", "player17_y",
                         "player18_x", "player18_y",
                         "player19_x", "player19_y",
                         "player20_x", "player20_y",
                         "player21_x", "player21_y",
                         "player22_x", "player22_y",
                         "player23_x", "player23_y",
                         "player24_x", "player24_y",
                         "player26_x", "player26_y",
                         "player27_x", "player27_y",
                         "player28_x", "player28_y",
                         "ball_x", "ball_y")

data_events <- read.csv("./Sample_Game_1/Sample_Game_1_RawEventsData.csv")


#### transform data from wide to long format ####

data_home <- data_home %>% mutate_if(is.character, as.numeric)
data_away <- data_away %>% mutate_if(is.character, as.numeric)

#### home ####
res_df_home <- data_home %>% 
  select(period, frame, time, ball_x, ball_y) %>% 
  rename(x = ball_x, y = ball_y) %>% mutate(player = "ball")

for(i in 1:14){
  des_column1 <- paste0("player", i, "_x")
  des_column2 <- paste0("player", i, "_y")
  des_player <- paste0("player", i)
  res_player <- data_home %>% 
    select(period, frame, time, all_of(des_column1), all_of(des_column2)) %>% 
    rename(x = des_column1, y = des_column2) %>% mutate(player = des_player)
  res_df_home <- bind_rows(res_df_home, res_player)
}

#### away ####
res_df_away <- data_away %>% 
  select(period, frame, time, ball_x, ball_y) %>% 
  rename(x = ball_x, y = ball_y) %>% mutate(player = "ball")

for(i in 15:28){
  des_column1 <- paste0("player", i, "_x")
  des_column2 <- paste0("player", i, "_y")
  des_player <- paste0("player", i)
  res_player <- data_away %>% 
    select(period, frame, time, all_of(des_column1), all_of(des_column2)) %>% 
    rename(x = des_column1, y = des_column2) %>% mutate(player = des_player)
  res_df_away <- bind_rows(res_df_away, res_player)
}

#### remove NaN ####
res_df_home <- res_df_home %>% filter(!is.nan(x)) 
res_df_away <- res_df_away %>% filter(!is.nan(x)) 


#### transform positions from Metrica units to meters (origin: center circle) ####
field_dimen <- c(106, 68)

res_df_home$x_meter <- (res_df_home$x-0.5 ) * field_dimen[1]
res_df_home$y_meter <- -1 * ( res_df_home$y-0.5 ) * field_dimen[2]

res_df_away$x_meter <- (res_df_away$x-0.5 ) * field_dimen[1]
res_df_away$y_meter <- -1 * ( res_df_away$y-0.5 ) * field_dimen[2]

res_df_home$team <- "home"
res_df_home$team[res_df_home$player == "ball"] <- "ball"
res_df_away$team <- "away"
res_df_away$team[res_df_away$player == "ball"] <- "ball"


res_df_home$x_onedirect <- NA
res_df_home$x_onedirect[res_df_home$period == 1] <- res_df_home$x[res_df_home$period == 1]
res_df_home$x_onedirect[res_df_home$period == 2] <- 1 - res_df_home$x[res_df_home$period == 2]       
res_df_home$y_onedirect <- NA
res_df_home$y_onedirect[res_df_home$period == 1] <- res_df_home$y[res_df_home$period == 1]
res_df_home$y_onedirect[res_df_home$period == 2] <- 1 - res_df_home$y[res_df_home$period == 2]

res_df_away$x_onedirect <- NA
res_df_away$x_onedirect[res_df_away$period == 1] <- res_df_away$x[res_df_away$period == 1]
res_df_away$x_onedirect[res_df_away$period == 2] <- 1 - res_df_away$x[res_df_away$period == 2]
res_df_away$y_onedirect <- NA
res_df_away$y_onedirect[res_df_away$period == 1] <- res_df_away$y[res_df_away$period == 1]
res_df_away$y_onedirect[res_df_away$period == 2] <- 1 - res_df_away$y[res_df_away$period == 2]

res_df_both <- rbind(res_df_home, res_df_away[res_df_away$team != "ball",])

pitch_custom <- list(
  length = 105,
  width = 68,
  penalty_box_length = 17.5,
  penalty_box_width = 34,
  six_yard_box_length = 5.6,
  six_yard_box_width = 17.68,
  penalty_spot_distance = 11.2,
  goal_width = 8,
  origin_x = -52.5,
  origin_y = -34
)


res_df_home %>% filter(frame == 16, player == "player9" | player == "player8") %>%
  ggplot(aes(x_meter, y_meter, colour = team)) + 
  annotate_pitch(dimensions = pitch_custom, colour = "white", fill = "#7fc47f", limits = FALSE) +
  theme_pitch() +
  coord_fixed() +
  geom_point() + 
  scale_colour_manual(name = "", values = c("black", "brown", "blue")) 

res_df_home <- res_df_home  %>% mutate(scaled = as.vector(scale(x_onedirect)))
defense <- res_df_home %>% filter(player == "player1" | player == "player2" | player == "player3" | player == "player4", period == 1)
midfield <- res_df_home %>% filter(player == "player5" | player == "player6" | player == "player7" | player == "player8", period == 1)
sturmer <- res_df_home %>% filter(player == "player10" | player == "player9", period == 1)

data_events$pos <- ifelse(data_events$Team == "Home", 1, 2)
data_events$change <- c(0,diff(data_events$pos))# <- ifelse(data_events$Team == "Home", 1, 2)
framechanges <- data_events %>% filter(change != 0, Period == 1)
pos_home <- c()
for (i in 1:(nrow(framechanges)/2)) {
  if(framechanges[c(1+(i-1)*2),]$Start.Frame < framechanges[c(i*2),]$Start.Frame-1){
    pos_home <- c(pos_home, seq(framechanges[c(1+(i-1)*2),]$Start.Frame, framechanges[c(i*2),]$Start.Frame-1, 1))
  }
}

defense <- defense %>% mutate(poss = ifelse(frame %in% pos_home, 1, 0))
midfield <- midfield %>% mutate(poss = ifelse(frame %in% pos_home, 1, 0))
sturmer <- sturmer %>% mutate(poss = ifelse(frame %in% pos_home, 1, 0))



#### match types to frames from event data to metrica #### 
data_events_home = subset(data_events, data_events$Team == "Home")
data_events_away = subset(data_events, data_events$Team == "Away")


## match passes to frames for home team
data_home$pass = 0
for (i in 1:length(rownames(data_events_home))) {
  if (data_events_home$Type[i] == "PASS") {
    start_frame = data_events_home$Start.Frame[i]
    end_frame = data_events_home$End.Frame[i]
    data_home$pass[which(data_home$frame == start_frame):which(data_home$frame == end_frame)] = 1
  }
}

## match passes to frames for away team
data_away$pass = 0
for (i in 1:length(rownames(data_events_away))) {
  if (data_events_away$Type[i] == "PASS") {
    start_frame = data_events_away$Start.Frame[i]
    end_frame = data_events_away$End.Frame[i]
    data_away$pass[which(data_away$frame == start_frame):which(data_away$frame == end_frame)] = 1
  }
}


## match shots to frames for home team
data_home$shot = 0
for (i in 1:nrow(data_events_home)) {
  if (data_events_home$Type[i] == "SHOT") {
    start_frame = data_events_home$Start.Frame[i]
    end_frame = data_events_home$End.Frame[i]
    data_home$shot[which(data_home$frame == start_frame):which(data_away$frame == end_frame)] = 1
  }
}

## match shots to frames for away team
data_away$shot = 0
for(i in 1:nrow(data_events_away)) {
  if (data_events_away$Type[i] == "SHOT") {
    start_frame = data_events_away$Start.Frame[i]
    end_frame = data_events_away$End.Frame[i]
    data_away$shot[which(data_away$frame == start_frame):which(data_away$frame == end_frame)] = 1
  }
}


#### add score difference ####
data_home$score.diff = 0
data_away$score.diff = 0
# set up types and sub types that lead to a goal
unique(data_events_home$Subtype)
unique(data_events_away$Subtype)
goal = c("HEAD-ON TARGET-GOAL", "WOODWORK-GOAL", "ON TARGET-GOAL")

###insert goal column
data_home$goal = 0
data_away$goal = 0

for (i in nrow(data_events_home)) {
  if (is.element(data_events_home$Subtype[i], goal)) {
    start_frame = data_events_home$Start.Frame[i]
    end_frame = data_events_home$End.Frame[i]
    data_home$goal[which(data_home$frame == start_frame):which(data_home$frame == end_frame)] = 1
  }
}

for (i in nrow(data_events_away)) {
  if (is.element(data_events_away$Subtype[i], goal)) {
    start_frame = data_events_away$Start.Frame[i]
    end_frame = data_events_away$End.Frame[i]
    data_away$goal[which(data_away$frame == start_frame): which(data_home$frame == end_frame)] = 1
  }
}

# score diff home team
for (i in 1:nrow(data_events_home)) {
  if (is.element(data_events_home$Subtype[i], goal)) {
    end_frame = data_events_home$End.Frame[i]
    data_home$score.diff[which(data_home$frame == end_frame):nrow(data_home)] = data_home$score.diff[which(data_home$frame == end_frame):nrow(data_home)] + 1
    data_away$score.diff[which(data_away$frame == end_frame):nrow(data_away)] = data_away$score.diff[which(data_away$frame == end_frame):nrow(data_away)] - 1
  }
}

# score diff away team
for (i in 1:nrow(data_events_away)) {
  if (is.element(data_events_away$Subtype[i], goal)) {
    end_frame = data_events_away$End.Frame[i]
    data_away$score.diff[which(data_away$frame == end_frame):nrow(data_away)] = data_away$score.diff[which(data_away$frame == end_frame):nrow(data_away)] + 1
    data_home$score.diff[which(data_home$frame == end_frame):nrow(data_home)] = data_home$score.diff[which(data_home$frame == end_frame):nrow(data_home)] - 1
  }
}

### Ball in play one-hot
data_home$ballinplay = 1
data_away$ballinplay = 1

for (i in 1:nrow(data_home)) {
  if (is.nan(data_home$ball_x) & is.nan(data_home$ball_y)) {
    data_home$ballinplay = 0
  }
}

for (i in 1:nrow(data_away)) {
  if (is.nan(data_away$ball_x) & is.nan(data_away$ball_y)) {
    data_away$ballinplay = 0
  }
}


