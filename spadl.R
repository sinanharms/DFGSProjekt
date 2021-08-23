library(R6)
library(data.table)
library(RCurl)
library(XML)
library(dplyr)
library(tidyverse)



StatsBombLoader = R6Class(
  "StatsBombLoader",
  public = list(
    # properties
    competition.root = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/" ,
    matches.root = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/",
    events.root = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/",
    field_length = 105.0,
    field_width = 68.0,
    event = NULL,
    interception.event = NULL,
    pass.event = NULL,
    carry.event = NULL,
    shot.event = NULL,
    ball.recovery.event = NULL,
    goalkeeper.event = NULL,
    duel.event = NULL,
    dribble.event = NULL,
    foul.committed.event = NULL,
    foul.won.event = NULL,
    bad.behaviour.event = NULL,
    substitution.event = NULL,
    vaep.root = "./data/",
    
    
    
    

  
    competition = function(){
      url = paste(self$competition.root, "competitions.json", sep = "")
      competitions = RJSONIO::fromJSON(url)
      competitions = data.frame(do.call(rbind, competitions), stringsAsFactors = FALSE)
      rel_cols = c("competition_id", "season_id", "competition_name", "competition_gender", "season_name")
      competitions %>% select(all_of(rel_cols))
    },
  
    games = function(competition_id, season_id){
      url = paste(self$matches.root, competition_id, "/", season_id, ".json", sep = "")
      games = RJSONIO::fromJSON(url)
      match = lapply(games, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      match = rbindlist(match, fill = TRUE)
      match = jsonlite::flatten(match)
      rel_cols = c("match_id", "match_date", "match_week", "competition.competition_name", "season.season_name", 
                   "home_team.home_team_name", "away_team.away_team_name", "home_score", "away_score", "stadium.name")
      match %>% select(all_of(rel_cols))
    },
    
    events = function(match_id){
      url = paste(self$events.root, match_id, ".json",sep = "")
      event.tmp = jsonlite::fromJSON(url)
      event = cbind(game_id = match_id, event.tmp)
      event = subset(event, event$type$name != "Ball Receipt*" & event$type$name != "Starting XI" &
                       event$type$name != "Half Start" & event$type$name != "Camera On" & event$type$name != "Camera off" &
                       event$type$name != "Half End" & event$type$name != "Tactical Shift" & event$type$name != "Pressure" &
                       event$type$name != "Ball Recovery" & event$type$name != "Dribbled Past" & event$type$name != "Block" &
                       event$type$name != "Dispossessed" & event$type$name != "Error" & event$type$name != "50/50" &
                       event$type$name != "Substituition" & event$type$name != "Foul Won")
      
      rownames(event) = seq(dim(event)[1])
      
      
      event.df = data.frame(matrix(NA, ncol = 20, nrow = dim(event)[1]))
      colnames(event.df) = c("event_id", "original_event_id", "period","index", "timestamp", "minute", "second", "possession", "possession_team",
                             "type", "playpattern", "team_name", "duration", "related_event", "player_name", "position_id",
                             "position_name", "location", "underpressure", "counterpress")
      event.df$event_id = event$game_id
      event.df$original_event_id = event$id
      event.df$period = event$period
      event.df$index = event$index
      event.df$timestamp = event$timestamp
      event.df$minute = event$minute
      event.df$second = event$second
      event.df$possession = event$possession
      event.df$possession_team = event$possession_team$name
      event.df$type = event$type$name
      event.df$playpattern = event$play_pattern$name
      event.df$team_name = event$team$name
      event.df$duration = event$duration
      event.df$related_event = event$related_events
      event.df$player_name = event$player$name
      event.df$position_id = event$position$id
      event.df$position_name = event$position$name
      event.df$location = event$location
      event.df$underpressure = event$under_pressure
      event.df$underpressure[is.na(event.df$underpressure)] = FALSE
      event.df$counterpress = event$counterpress
      event.df$counterpress[is.na(event.df$counterpress)] = FALSE
      
      rownames(event.df) = seq(1,nrow(event.df),1)
      
      
      self$event = jsonlite::flatten(event.tmp)
      self$pass.event = event$pass
      self$pass.event$outcome$name[is.na(self$pass.event$outcome$name)] = "Complete"
      self$carry.event = event$carry
      self$ball.recovery.event = event$ball_recovery
      self$shot.event = event$shot
      self$goalkeeper.event = event$goalkeeper
      self$duel.event = event$duel
      self$dribble.event = event$dribble
      self$interception.event = event$interception
      self$foul.committed.event = event$foul_committed
      self$foul.won.event = event$foul_won
      self$bad.behaviour.event = event$bad_behaviour
      self$substitution.event = event$substitution
      
      
      return(event.df)
    },
    
    convert.to.actions = function(events, hometeam){
      columns = c("game_id", "original_event_id", "period_id", "time_seconds", "team", "player", "start_x",
                  "start_y", "end_x", "end_y", "type", "result", "body_part")
      event = events
      actions = data.frame(matrix(NA, nrow = dim(event)[1], ncol = 13))
      colnames(actions) = columns
      
      actions$game_id = event$event_id
      actions$original_event_id = event$original_event_id
      actions$period_id = event$period
      actions$time_seconds = event$timestamp
      actions$team = event$team_name
      actions$player = event$player_name
      
      for (i in 1:dim(event)[1]) {
        actions$start_x[i] = ifelse(is.null(event$location[[i]][1]), 1, ((event$location[[i]][1] - 1)/119) * self$field_length)
        actions$start_x[i] = ifelse(event$team_name[i] != hometeam, self$field_length - actions$start_x[i], actions$start_x[i])
        actions$start_y[i] = ifelse(is.null(event$location[[i]][2]), 1, ((event$location[[i]][2] - 1)/79) *self$field_width)
        actions$start_y[i] = ifelse(event$team_name[i] != hometeam, self$field_width - actions$start_y[i], actions$start_y[i])
        actions$end_x[i] = ifelse(event$type == "Pass", ifelse(is.null(self$pass.event$end_location[[i]][1]), 1, ((self$pass.event$end_location[[i]][1] -1)/119)*self$field_length),
                                  ifelse(event$type == "Shot", ifelse(is.null(self$shot.event$end_location[[i]][1]), 1, ((self$shot.event$end_location[[i]][1] -1)/119)*self$field_length),
                                         ifelse(event$type == "Carry", ifelse(is.null(self$carry.event$end_location[[i]][1]), 1, ((self$carry.event$end_location[[i]][1] -1)/119)*self$field_length),
                                                1)))
        actions$end_x[i] = ifelse(event$team_name[i] != hometeam, self$field_length - actions$end_x[i], actions$end_x[i])
        actions$end_y[i] = ifelse(event$type == "Pass", ifelse(is.null(self$pass.event$end_location[[i]][2]), 1, ((self$pass.event$end_location[[i]][2] -1)/119)*self$field_length),
                                  ifelse(event$type == "Shot", ifelse(is.null(self$shot.event$end_location[[i]][2]), 1, ((self$shot.event$end_location[[i]][2] -1)/119)*self$field_length),
                                         ifelse(event$type == "Carry", ifelse(is.null(self$carry.event$end_location[[i]][2]), 1, ((self$carry.event$end_location[[i]][2] -1)/119)*self$field_length),
                                                1)))
        actions$end_y[i] = ifelse(event$team_name[i] != hometeam, self$field_width - actions$end_y[i], actions$end_y[i])
        
        actions$type[i] = ifelse(event$type[i] == "Pass", self$parse.pass.event.type(i), 
                                ifelse(event$type[i] == "Carry", self$parse.carry.event.type(i),
                                       ifelse(event$type[i] == "Dribble", self$parse.dribble.event.type(i),
                                              ifelse(event$type[i] == "Duel", self$parse.duel.event.type(i),
                                                     ifelse(event$type[i] == "Shot", self$parse.shot.event.type(i),
                                                            ifelse(event$type[i] == "Interception", self$parse.interception.event.type(i),
                                                                   ifelse(event$type[i] == "Own Goal", self$parse.owngoal.event.type(i),
                                                                          ifelse(event$type[i] == "Goal Keeper", self$parse.gk.event.type(i),
                                                                                 ifelse(event$type[i] == "Clearance", self$parse.clearance.event.type(i),
                                                                                        ifelse(event$type[i] == "Miscontrol", self$parse.miscontrol.event.type(i), self$parse.foul.event.type(i)))))))))))
        actions$result[i] = ifelse(event$type[i] == "Pass", self$parse.pass.event.outcome(i), 
                                   ifelse(event$type[i] == "Carry", self$parse.carry.event.outcome(i),
                                          ifelse(event$type[i] == "Dribble", self$parse.dribble.event.outcome(i),
                                                 ifelse(event$type[i] == "Duel", self$parse.duel.event.outcome(i),
                                                        ifelse(event$type[i] == "Shot", self$parse.shot.event.outcome(i),
                                                               ifelse(event$type[i] == "Interception", self$parse.interception.event.outcome(i),
                                                                      ifelse(event$type[i] == "Own Goal", self$parse.owngoal.event.outcome(i),
                                                                             ifelse(event$type[i] == "Goal Keeper", self$parse.gk.event.outcome(i),
                                                                                    ifelse(event$type[i] == "Clearance", self$parse.clearance.event.outcome(i),
                                                                                           ifelse(event$type[i] == "Miscontrol", self$parse.miscontrol.event.outcome(i), self$parse.foul.event.outcome(i)))))))))))
        actions$body_part[i] = ifelse(event$type[i] == "Pass", self$parse.pass.event.bp(i), 
                                      ifelse(event$type[i] == "Carry", self$parse.carry.event.bp(i),
                                             ifelse(event$type[i] == "Dribble", self$parse.dribble.event.bp(i),
                                                    ifelse(event$type[i] == "Duel", self$parse.duel.event.bp(i),
                                                           ifelse(event$type[i] == "Shot", self$parse.shot.event.bp(i),
                                                                  ifelse(event$type[i] == "Interception", self$parse.interception.event.bp(i),
                                                                         ifelse(event$type[i] == "Own Goal", self$parse.owngoal.event.bp(i),
                                                                                ifelse(event$type[i] == "Goal Keeper", self$parse.gk.event.bp(i),
                                                                                       ifelse(event$type[i] == "Clearance", self$parse.clearance.event.bp(i),
                                                                                              ifelse(event$type[i] == "Miscontrol", self$parse.miscontrol.event.bp(i), self$parse.foul.event.bp(i)))))))))))
      
        
        
      }
      for (i in 1:dim(actions)[1]) {
        if (actions$type[i] == "dribble") {
          actions$end_x[i] = actions$start_x[i+1]
          actions$end_y[i] = actions$start_y[i+1]
        }
      }
      
      return(actions)
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
    
    players = function(){
      event = self$event
      
    },
    
    substitutions = function(event){
      
    },
    
    xG = function(event){
      for (i in 1:dim(event)[1]) {
        
      }
    },
    
    add.vaep.scores = function(actions){
      event = actions
      match.id = event$game_id[1]
      path = paste(self$vaep.root, "vaep_score_", match.id, ".json", sep="")
      vaep.scores = jsonlite::fromJSON(path)
      vaep.scores = lapply(vaep.scores, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
      vaep.scores = rbindlist(vaep.scores, fill = TRUE)
      temp.vaep = semi_join(vaep.scores, actions, by="original_event_id")
      event$scores = temp.vaep$scores
      event$concedes = temp.vaep$concedes
      event$offensive_value = temp.vaep$offensive_value
      event$defensive_value = temp.vaep$defensive_value
      event$vaep_value = temp.vaep$vaep_value
      return(event)
    },
    
    
    
    # Auxiliary functions
    #pass
    parse.pass.event.type = function(idx){
      p = self$pass.event[idx,]
      #a = "pass"
      ptype = p$type$name
      height = p$height$name
      cross = p$cross
      cross[is.na(cross)] = FALSE
      a = ifelse(is.na(ptype), "pass", ifelse(ptype == "Freekick", ifelse(height == "Highpass", "freekick_crossed", "freekick_short"),
                                              ifelse(ptype == "Corner", ifelse(height == "Highpass", "corner_crossed", "corner_short"),
                                                     ifelse(ptype == "Goal Kick", "goalkick", 
                                                            ifelse(ptype == "Throw-in", "throw_in",
                                                                   ifelse(cross, "cross", "pass"))))))
      
      return(a)
    },
    parse.pass.event.outcome = function(idx){
      p = self$pass.event[idx,]
      pass_outcome = p$outcome$name
      r = ifelse(pass_outcome == "Complete", "success", ifelse(pass_outcome == "Pass Offside", "offside", "fail"))
      return(r)
    },
    parse.pass.event.bp = function(idx){
      
      b = self$pass.event$body_part$name[idx]
      bp = ifelse(is.na(b), "foot", ifelse(b %in% c("Right Foot", "Left Foot", "Drop Kick"), "foot", 
                                           ifelse(b == "Head", "head", "other")))
      
      
    },
    
    #dribble
    parse.dribble.event.type = function(idx){
      a = "take_on"
      
      return(a)
    }, 
    parse.dribble.event.outcome = function(idx){
      d = self$dribble.event[idx,]
      d$outcome$name[is.na(d$outcome$name)] = "Complete"
      r = switch (d$outcome$name,
        "Complete" = "success",
        "Incomplete" = "fail"
      )
    },
    parse.dribble.event.bp = function(idx){
      bp = "foot"
      
      return(bp)
    },
    
    #interception
    parse.interception.event.type = function(idx){
      a = "interception"
      
      return(a)
      
    },
    parse.interception.event.outcome = function(idx){
      outcome = self$interception.event$outcome$name[idx]
      r = ifelse(outcome %in% c("Lost in Play", "Lost Out"), "fail",
                 ifelse(outcome == "Won", "success", "success"))
      
      return(r)
    },
    parse.interception.event.bp = function(idx){
      bp = "foot"
      
      return(bp)
    },
    
    #shot
    parse.shot.event.type = function(idx){
      type = self$shot.event$type$name[idx]
      a = ifelse(type == "Free Kick", "shot_freekick", ifelse(type == "penalty", "shot_penalty", "shot"))
      
      return(a)
    }, 
    parse.shot.event.outcome = function(idx){
      outcome = self$shot.event$outcome$name[idx]
      r = ifelse(outcome == "Goal", "success", ifelse(outcome %in% c("Blocked", "Off T", "Post","Saved", "Wayward"), "fail",
                                                      "fail"))
      
      return(r)
    }, 
    parse.shot.event.bp = function(idx){
      b = self$shot.event$body_part$name[idx]
      bp = ifelse(is.na(b), "foot", ifelse(b == "Head", "head", "foot"))
      
      return(bp)
    }, 
    
    #carry
    parse.carry.event.type = function(idx){
      a = "dribble"
      
      return(a)
    },
    parse.carry.event.outcome = function(idx){
      r = "success"
      
      return(r)
    }, 
    parse.carry.event.bp = function(idx){
      bp = "foot"
      
      return(bp)
    },
    
    #duel
    parse.duel.event.type = function(idx){
      p = self$duel.event$type$name[idx]
      if (p == "Tackle") {
        a = "tackle"
      } else{
        a = "aerial"
      }
      #a = ifelse(p == "Tackle", "tackle", next)
      return(a)
    }, 
    parse.duel.event.outcome = function(idx){
      type = self$duel.event$type$name[idx]
      # r = ifelse(type == "Tackle", ifelse(self$duel.event$outcome$name[idx] %in% c("Lost In Play", "Lost Out"), 
      #                                     "fail", ifelse(self$duel.event$outcome$name[idx] %in% c("Success in Play", "Won"), 
      #                                                    "success", "success")))
      r = ifelse(type == "Tackle", ifelse(self$duel.event$outcome$name[idx] %in% c("Lost In Play", "Lost Out", NA), "fail",
                                           ifelse(self$duel.event$outcome$name[idx] %in% c("Success In Play", "Won"), "success", "success")), "success")
      # if (type == "Tackle") {
      #   outcome = self$duel.event$outcome$name[idx]
      #   r = ifelse(outcome %in% c("Lost In Play", "Lost Out"), "fail", ifelse(outcome %in% c("Success in Play", "Won"), 
      #                                                                         "success", "success"))
      #   
      #   return(r)
      #   }
    },
    parse.duel.event.bp = function(idx){
      if (self$duel.event$type$name[idx] == "Aerial Lost") {
        bp = "head"
      }else{
        bp = "foot"
      }
      
      
      return(bp)
    },
    
    #foul
    parse.foul.event.type = function(idx){
      a = "foul"
      
      return(a)
    },
    parse.foul.event.outcome = function(idx){
      outcome = self$foul.committed.event$card$name[idx]
      r = ifelse(is.na(outcome), "success", ifelse(outcome == "Yellow Card", "yellow_card", 
                                                   ifelse(outcome == "Red Card", "red_card", "success")))
    
      return(r)
    },
    parse.foul.event.bp = function(idx){
      bp = "foot"
      
      return(bp)
    },
    
    #own goal
    parse.owngoal.event.type = function(idx){
      a = "shot"
      
      return(a)
    },
    parse.owngoal.event.outcome = function(idx){
      r = "owngoal"
      
      return(r)
    }, 
    parse.owngoal.event.bp = function(idx){
      bp = "foot"
      
      return(bp)
    },
    
    #clearance
    parse.clearance.event.type = function(idx){
      a = "clearance"
      
      return(a)
    },
    parse.clearance.event.outcome = function(idx){
      r = "success"
      
      return(r)
    }, 
    parse.clearance.event.bp = function(idx){
      bp = "foot"
      
      return(bp)
    },
    
    #miscontrol
    parse.miscontrol.event.type = function(idx){
      a = "bad_touch"
      
      return(a)
    },
    parse.miscontrol.event.outcome = function(idx){
      r = "fail"
      
      return(r)
    },
    parse.miscontrol.event.bp = function(idx){
      bp = "foot"
    },
    
    #goal keeper 
    parse.gk.event.type = function(idx){
      type = self$goalkeeper.event$type$name[idx]
      a = ifelse(type == "Shot Saved", "keeper_save", ifelse(type %in% c("Collected", "Keeper Sweeper"), "keeper_claim",
                                                             ifelse(type == "Punch", "keeper_punch", "non_action")))
      
      return(a)
    },
    parse.gk.event.outcome = function(idx){
      outcome = self$goalkeeper.event$outcome$name[idx]
      r = ifelse(outcome %in% c("Claim", "Clear", "Collected Twice", "In Play Safe", "Success", "Touched Out"), "success",
                 ifelse(outcome %in% c("In Play Danger", "No Touch"), "fail", "success"))
      
      return(r)
    },
    parse.gk.event.bp = function(idx){
      b = self$goalkeeper.event$body_part$name[idx]
      bp = ifelse(is.na(b), "foot", ifelse(b == "Head", "head", "foot"))
      
      return(bp)
    }
    
  
  )
)




#### DEMO ######

spadl = StatsBombLoader$new()
game = spadl$games(11, 1)
event = spadl$events(7570)

actions = spadl$convert.to.actions(event, "Belgium")
vaep = spadl$add.vaep.scores(actions)
#### TODO ####
# add substitutions
# add players in game minutes played etc



