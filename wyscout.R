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
      colnames(competition) = c("name", "wyID", "format", "country", "country_id", "country_code", "country_code2", "type")
      return(competition)
    },
    
    matches = function(country){
      directory = paste(self$root, "matches_", as.character(country), ".json", sep = "")
      games = RJSONIO::fromJSON(directory)
      games = lapply(games, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      games = rbindlist(games, fill = TRUE)
      
      return(games)
    }
  )
)


wyscout = WyScoutData$new()
wyscout$competitions()

a = RJSONIO::fromJSON("./wyscoutdata/matches_Italy.json")
a = lapply(a, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
a = rbindlist(a, fill = TRUE)

c = RJSONIO::fromJSON("./wyscoutdata/competitions.json")
c = lapply(c, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
c = rbindlist(c, fill = TRUE)

e = RJSONIO::fromJSON("./wyscoutdata/actions/game_2499719.json")
