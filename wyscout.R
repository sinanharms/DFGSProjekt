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
    
    # loads an overview with all competitions contained in the wyscout data
    competitions = function(){
      directory = paste(self$root, "competitions.json", sep = "")
      competition = RJSONIO::fromJSON(directory)
      competition = lapply(competition, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      competition = rbindlist(competition, fill = TRUE)
      
      return(competition)
    },
    
    # loads data listing all the matches from specified country -> country has to be as string
    matches = function(country){
      directory = paste(self$root, "matches_", as.character(country), ".json", sep = "")
      games = RJSONIO::fromJSON(directory)
      games = lapply(games, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      games = rbindlist(games, fill = TRUE)
      
      return(games)
    },
    
    # loads spadl actions dataframe from specified match -> use maatch id from matches dataframe
    actions = function(match_id){
      directory = paste(self$root, "actions/", "actions_", match_id, ".json", sep = "")
      actions = RJSONIO::fromJSON(directory)
      actions = lapply(actions, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      actions = rbindlist(actions, fill = TRUE)
      
      return(actions)
    }, 
    
    # adds vaep scores to actions data 
    vaep_scores = function(match_id){
      directory = paste(self$root, "vaep_scores/", "vaep_score_", match_id, ".json", sep = "")
      action_directory = paste(self$root, "actions/", "actions_", match_id, ".json", sep = "")
      score = RJSONIO::fromJSON(directory)
      score = lapply(score, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      score = rbindlist(score, fill = TRUE)
      actions = RJSONIO::fromJSON(action_directory)
      actions = lapply(actions, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      actions = rbindlist(actions, fill = TRUE)
      score = cbind(action, score[,19:22])
      
      return(score)
    },
    
    most_valuable_player = function(match_id){
      directory = paste(self$root, "mvp/", "mvp_game_", match_id, ".json", sep = "")
      mvp = RJSONIO::fromJSON(directory)
      mvp = lapply(mvp, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      mvp = rbindlist(mvp, fill = TRUE)
      mvp = mvp[order(-mvp$vaep_value),]
      
      return(mvp)
    }, 
    
    # function to load all wyscout data with vaep scores and store it in one dataframe 
    vaep_fullset = function(){
      all.vaep.files = list.files(paste(self$root, "vaep_scores/", sep = ""), full.names = T, recursive = T)
      all.action.files = list.files(paste(self$root, "actions/", sep = ""), full.names = T, recursive = T)
      
      score.list = list()
      for (i in 1:length(all.vaep.files)) {
        vaep.tmp = RJSONIO::fromJSON(all.vaep.files[i])
        vaep = lapply(vaep.tmp, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
        vaep.df = rbindlist(vaep, fill = TRUE)
        actions.tmp = RJSONIO::fromJSON(all.action.files[i])
        action = lapply(actions.tmp, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
        action.df = rbindlist(action, fill = TRUE)
        score = cbind(action.df, vaep.df[,19:22])
        
        score.list[[i]] = score
      }
      
      all.scores = data.frame(rbindlist(score.list, fill = TRUE))
      
      return(all.scores)
    }
  )
)



## Examples on how to use the functions
wyscout = WyScoutData$new()
wyscout$competitions()
matches = wyscout$matches("Germany")
action = wyscout$actions(2517036)
vaep = wyscout$vaep_scores(2517036)
mvp = wyscout$most_valuable_player(2517036)                          
all.scores = wyscout$vaep_fullset()

