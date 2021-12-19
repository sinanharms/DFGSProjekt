library(dplyr)
library(tidyverse)
library(data.table)


# load both data sets
df_plays = read.csv("./plays.csv")
df_week = read.csv("./week1.csv")


# get an overview over the lineups -> only using first 10 unique lineups since count for other is mostly 1
plays_per_personnel = count(df_plays, df_plays$personnelO, sort = 0)
colnames(plays_per_personnel) = c("personnel", "plays")
unique(plays_per_personnel$personnel)[1:10]


# set up players and result, not really necessary since it's also initialized in the combined data set
players = c("RB1", "RB2", "TE1", "TE2", "WR1", "WR2", "WR3", "WR4")
results = c("RB1_target", "RB2_target", "TE1_target", "TE2_target", "WR1_target", "WR2_target", "WR3_target",
            "WR4_target")


### initialize combined data frame
columns = c("gameId", "playId", "playDescription", "quarter", "down", "yardsToGo", "possessionTeam", "playType",
            "offenseFormation", "personnelO", "RB1", "RB2", "RB3", "TE1", "TE2", "TE3", "WR1", "WR2", "WR3",
            "WR4", "RB1_target", "RB2_target", "RB3_target", "TE1_target", "TE2_target", "TE3_target", "WR1_target",
            "WR2_target", "WR3_target", "WR4_target", "typeDropback", "gameClock", "absoluteYardlineNumber", "passResult", 
            "offensePlayResult")

df_combined = data.frame(matrix(NA, nrow = dim(df_plays[1]), ncol = length(columns)))
colnames(df_combined) = columns


# combining the data sets plays and weeks by assigning the matching columns from plays to the combined one
for (i in colnames(df_combined)) {
  for (j in colnames(df_plays)) {
    if (i == j) {
      df_combined[i] = df_plays[j]
    }
  }
}


# filter by first frame since this gives us the lineup at beginning of the play
# after that every frame tracks the movement of each player in the play
week_filtered = subset(df_week, df_week$frameId == 1)


# set all NA to 0
df_combined[is.na(df_combined)] = 0


# count and assign RB
for (i in unique(df_combined$gameId)) {
  tmp = subset(df_combined, df_combined$gameId == i)
  for (j in unique(tmp$playId)) {
    count = 0
    week_tmp = subset(week_filtered, week_filtered$playId == j & week_filtered$gameId == i)
    for (k in week_tmp$position) {
      if (k == "RB") {
        count = count + 1
        rb = paste("RB", count, sep = "")
        df_combined[df_combined$gameId == i & df_combined$playId == j, rb] = 1
      }
    }
  }
}


# count and assign WR 
for (i in unique(df_combined$gameId)) {
  tmp = subset(df_combined, df_combined$gameId == i)
  for (j in unique(tmp$playId)) {
    count = 0
    week_tmp = subset(week_filtered, week_filtered$playId == j & week_filtered$gameId == i)
    for (k in week_tmp$position) {
      if (k == "WR") {
        count = count + 1
        wr = paste("WR", count, sep = "")
        df_combined[df_combined$gameId == i & df_combined$playId == j, wr] = 1
      }
    }
  }
}


# count and assign TE
for (i in unique(df_combined$gameId)) {
  tmp = subset(df_combined, df_combined$gameId == i)
  for (j in unique(tmp$playId)) {
    count = 0
    week_tmp = subset(week_filtered, week_filtered$playId == j & week_filtered$gameId == i)
    for (k in week_tmp$position) {
      if (k == "TE") {
        count = count + 1
        te = paste("TE", count, sep = "")
        df_combined[df_combined$gameId == i & df_combined$playId == j, te] = 1
      }
    }
  }
}



### target determination
## go through every game and subsample it, then look at each unique play
## create a subset by filtering the week data by playid and gameid, this is necessary since there are duplicate playids
## iterate through the positions of the play if they are either WR, RB or TE count them 
## then get name of the player and search in the play description if the name is present, this means he is the 
## intended target of the pass 
## annotation: sometimes the number of widereceivers doesn't match this is because full backs are individually listed
## but are also runningbacks this can easily be fixed in count and assign runningbacks by modifying the if condition
for (k in unique(df_combined$gameId)) {
  tmp = subset(df_combined, df_combined$gameId == k)
  for (i in unique(tmp$playId)) {
    print(c(k,i))
    wr_count = 0
    rb_count = 0
    te_count = 0
    wr_tmp = subset(week_filtered, week_filtered$playId == i & week_filtered$gameId == k)
    for (j in 1:length(wr_tmp$position)) {
      if (wr_tmp$position[j] == "WR" ) {
        wr_count = wr_count + 1
        name = str_split(wr_tmp$displayName[j], " ")[[1]][2]
        if (grepl(name, tmp$playDescription[tmp$playId == i]) == TRUE) {
          wrt = paste("WR", wr_count, "_target", sep = "")
          df_combined[df_combined$playId == i & df_combined$gameId == k, wrt] = 1
        } 
      } else  if (wr_tmp$position[j] == "RB") {
        rb_count = rb_count + 1
        name = str_split(wr_tmp$displayName[j], " ")[[1]][2]
        if (grepl(name, tmp$playDescription[tmp$playId == i]) == TRUE) {
          rbt = paste("RB", rb_count, "_target", sep = "")
          df_combined[df_combined$playId == i & df_combined$gameId == k, rbt] = 1
        }
      } else if (wr_tmp$position[j] == "TE") {
        te_count = te_count + 1
        name = str_split(wr_tmp$displayName[j], " ")[[1]][2]
        if (grepl(name, tmp$playDescription[tmp$playId == i]) == TRUE) {
          tet = paste("TE", te_count, "_target", sep = "")
          df_combined[df_combined$playId == i & df_combined$gameId == k, tet] = 1
        }
      }
    }
  }
}






