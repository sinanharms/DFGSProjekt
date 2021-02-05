library(R6)
library(data.table)
library(RCurl)
library(XML)
library(dplyr)



StatsBombLoader = R6Class(
  "StatsBombLoader",
  public = list(
    # properties
    competition.root = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/" ,
    matches.root = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/",
    events.root = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/",
    field_length = 105.0,
    field_width = 68.0,
    pass.event = NULL,
    carry.event = NULL,
    shot.event = NULL,
    ball.recovery.event = NULL,
    goalkeeper.event = NULL,
    duel.event = NULL,
    dribble.event = NULL,
    foul.commited.event = NULL,
    foul.won.event = NULL,
    bad.behaviour.event = NULL,
    substitution.event = NULL,
    
    
    
    

  
    competition = function(){
      url = paste(self$competition.root, "competitions.json", sep = "")
      competitions = RJSONIO::fromJSON(url)
      competitions = data.frame(do.call(rbind, competitions), stringsAsFactors = FALSE)
    },
  
    games = function(competition_id, season_id){
      url = paste(self$matches.root, competition_id, "/", season_id, ".json", sep = "")
      games = RJSONIO::fromJSON(url)
      match = lapply(games, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      match = rbindlist(match, fill = TRUE)
    },
    
    events = function(match_id){
      url = paste(self$events.root, match_id, ".json",sep = "")
      event = jsonlite::fromJSON(url)
      event = cbind(game_id = match_id, event)
      event = subset(event, event$type$name != "Ball Receipt*" & event$type$name != "Starting XI" &
                       event$type$name != "Half Start" & event$type$name != "Camera On" & event$type$name != "Camera off" &
                       event$type$name != "Half End" & event$type$name != "Tactical Shift" )
      event.df = data.frame(matrix(NA, ncol = 18, nrow = dim(event)[1]))
      colnames(event.df) = c("event_id", "index", "timestamp", "minute", "second", "possession", "possession_team",
                             "type", "playpattern", "team_name", "duration", "related_event", "player_name", "position_id",
                             "position_name", "location", "underpressure", "counterpress")
      event.df$event_id = event$game_id
      event.df$index = event$index
      event.df$timestamp = event$timestamp
      event.df$minute = event$minute
      event.df$second = event$second
      event.df$possession = event$possession
      event.df$possession_team = event$possession_team$name
      event.df$type = event$type
      event.df$playpattern = event$play_pattern$name
      event.df$team_name = event$team$name
      event.df$duration = event$duration
      event.df$related_event = event$related_events
      event.df$player_name = event$player$name
      event.df$position_id = event$position$id
      event.df$position_name = event$position$name
      event.df$location = event$location
      event.df$under_pressure = event$under_pressure
      event.df$counterpress = event$counterpress
      
      self$pass.event = event$pass
      self$carry.event = event$carry
      self$ball.recovery.event = event$ball_recovery
      self$shot.event = event$shot
      self$goalkeeper.event = event$goalkeeper
      self$duel.event = event$duel
      self$dribble.event = event$dribble
      self$foul.won.event = event$foul_won
      self$bad.behaviour.event = event$bad_behaviour
      self$substitution.event = event$substitution
      
      
      return(event.df)
    },
    
    convert.to.actions = function(events, team_name){
      columns = c("game_id", "original_event_id", "period_id", "time_seconds", "team", "player", "start_x",
                  "start_y", "end_x", "end_y", "type", "result", "body_part")
      event = subset(event, event$possession_team$name == team_name)
      actions = data.frame(NA, nrow = 1, ncol = length(columns))
      colnames(actions) = columns
      actions$game_id = event$game_id
      actions$original_id = event$id
      actions$period_id = event$period
      actions$time_seconds = event$timestamp
      actions$team = event$team.name
      actions$type = event$type$name
      actions$start_x = ((event$location[[1]][1] - 1)/119) * self$field_length
      actions$start_y = ((event$location[[1]][1] - 1)/79) *self$field_width
      
      
    },
    
    lineups = function(match_id){
      url = paste(self$events.root, match_id, ".json",sep = "")
      event = RJSONIO::fromJSON(url)
      lineup.list = list()
      #teamids = c()
      for (s in 1:2) {
        lineup.team = data.frame(matrix(t(unlist(event[[s]]$tactics$lineup)), ncol = 5,
                                   byrow = TRUE), stringsAsFactors = FALSE)
        colnames(lineup.team) = names(unlist(event[[s]]$tactics$lineup))[1:5]
        lineup.team$formation = event[[s]]$tactics$formation
        lineup.team$team_id = event[[s]]$team$id
        #teamids = c(teamids, event[[s]]$team$id)
        lineup.team$team_name = event[[s]]$team$name
        lineup.list[[s]] = lineup.team
      }
      for (i in length(lineup.list)) {
         lineup = rbind(data.frame(lineup.list[i-1]), data.frame(lineup.list[i]))
      }
      lineup = cbind(match.id = match_id, lineup)
      return(lineup)
    },
    
    parse.pass.event = function(event){
      a = "Pass" # default
      p = self$pass.event
      ptype = p$type$name
      height = p$height$name
      cross = p$cross
      cross[is.na(cross)] = FALSE
      if (ptype == "Free Kick") {
        if (height == "High Pass" | cross ) {
          a = "Free Kick Crossed"
        }else{
          a = "Free Kick Short"
          }
        } else if (ptype == "Corner") {
        if (height == "High Pass" | cross) {
          a = "Corner Crossed"
        } else{
          a = "Corner Short"
          }
        } else if (ptype == "Goal Kick") {
          a = "Goal Kick"
        } else if (ptype == "Throw-In") {
          a = "Throw_in"
        } else if (cross) {
          a = "cross"
        } else {
          a = "pass"
        }
      
      pass_outcome = p$outcome$name
      pass_outcome[is.na(pass_outcome)] = "Complete"
      if (pass_outcome == "Incomplete" | pass_outcome == "Out"  ) {
        r = "Fail"
      } else if (pass_outcome == "Pass Offside") {
        r = "Offside"
      } else {
        r = "Success"
      }
      
      bp = p$body_part$name
      
      
      
      return(list(a, r, bp))
    }
    
    
  )                     
)

spadl = StatsBombLoader$new()
game = games(11, 1)
a$matches.root
event = spadl$events(15946)
lineup = a$lineups(15946)
a = spadl$competition()
match_id = 9592

s = seq(1:10)
for (i in 1:10) {
  if (i < 5) {
    print(i)
  } else if (i == 5) {
    print(TRUE)
  } else {
    print(i)
  }
}
if (event$type$name == "Pass") {
  event$type$name = lapply(event, spadl$parse.pass.event)[1]
}
a = lapply(as.list(event$type$name) ,spadl$parse.pass.event)[,1:3]
a

