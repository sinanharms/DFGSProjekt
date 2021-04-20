library(R6)
library(data.table)
library(RCurl)
library(XML)
library(dplyr)
library(tidyverse)


WyScoutData = R6Class(
  "WyScoutLoader",
  public = list(
    root = "./wyscoutdata/",
    
    competitions = function(){
      directory = paste(self$root, "competitions.json", sep = "")
      competition = RJSONIO::fromJSON(directory)
      competition = lapply(competition, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      competition = rbindlist(competition, fill = TRUE)
      
      return(competition)
    },
    
    matches = function(country){
      directory = paste(self$root, "matches_", as.character(country), ".json", sep = "")
      games = RJSONIO::fromJSON(directory)
      games = lapply(games, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      games = rbindlist(games, fill = TRUE)
      
      return(games)
    },
    
    actions = function(match_id){
      directory = paste(self$root, "actions/", "actions_", match_id, ".json", sep = "")
      actions = RJSONIO::fromJSON(directory)
      actions = lapply(actions, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      actions = rbindlist(actions, fill = TRUE)
      
      return(actions)
    }, 
    
    vaep_scores = function(match_id){
      directory = paste(self$root, "vaep_scores/", "vaep_score_", match_id, ".json", sep = "")
      score = RJSONIO::fromJSON(directory)
      score = lapply(score, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      score = rbindlist(score, fill = TRUE)
      
      return(score)
    },
    
    most_valuable_player = function(match_id){
      directory = paste(self$root, "mvp/", "mvp_game_", match_id, ".json", sep = "")
      mvp = RJSONIO::fromJSON(directory)
      mvp = lapply(mvp, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      mvp = rbindlist(mvp, fill = TRUE)
      mvp = mvp[order(-mvp$vaep_value),]
      
      return(mvp)
    }
  )
)


wyscout = WyScoutData$new()
wyscout$competitions()
matches = wyscout$matches("Germany")
action = wyscout$actions(2517036)
mvp = wyscout$most_valuable_player(2517036)                          
View(mvp
     )
