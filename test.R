library(rjson)
library(data.table)
library(RCurl)
library(XML)
library(dplyr)



### R Script for statsbomb data

## Obtain statsbomb competitions data
competitions = fromJSON(file = 'https://raw.githubusercontent.com/statsbomb/open-data/master/data/competitions.json',
                        unexpected.escape = "skip")

# convert list to data.frame
competitions.df = data.frame(do.call(rbind, competitions), stringsAsFactors = FALSE)

## obtain matches ##
competitions.id = c(2, 11, 16, 37, 43, 49, 72)
matches.path = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/"
match.files = list()
for (id in 1:length(competitions.id)){
  match.files[id] = paste(matches.path, competitions.id[id], sep = "")
  
}

matches.list = list()
for(i in 1:length(match.files)){
  matches.temp = fromJSON(file = match.files[[i]])
  matches = lapply(match.temp, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
  matches.df = rbindlist(matches, fill = TRUE)
  matches.list[[i]] = matches.df
}

all.matches.df = data.frame(rbindlist(matches.list, fill = TRUE))

### clean up
columns.to.keep = names(which(unlist(lapply(all.matches.df, function(x) length(which(is.na(x))))) == 0))

all.matches.clean = all.matches.df[, columns.to.keep]

#turn some colums to numeric
all.matches.clean$match_week = as.numeric(all.matches.clean$match_week)
all.matches.clean$home_score = as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score = as.numeric(all.matches.clean$away_score)

## obtain events ##
event.files = getURL("https://github.com/statsbomb/open-data/tree/master/data/events/")
event.files = getHTMLLinks(event.files, xpQuery = "//a/@href['.json'=substring(., string-length(.) - 4)]")

event.list = list()
for (i in 1:length(event.files)) {
  event.tmp = fromJSON(file = event.files[i])
  #get the unique teamids participating in a match
  teamids = c()
  #get index where find the event that talks about Starting XI
  starting.xi.index = which(unlist(lapply(event.tmp, function(x) x$type$name))=="Starting XI")
  starting.xi.list = list()
  for (s in 1:2) {
    starting.xi.team1 = data.frame(matrix(t(unlist(event.tmp[[s]]$tactics$lineup)), ncol = 5,
                                          byrow = TRUE), stringsAsFactors = FALSE)
    colnames(starting.xi.team1) = names(unlist(event.tmp[[s]]$tactics$lineup))[1:5]
    starting.xi.team1$formation = event.tmp[[s]]$tactics$formation
    starting.xi.team1$team_id = event.tmp[[s]]$team$id
    
    teamids = c(teamids, event.tmp[[s]]$team$id)
    starting.xi.team1$team_name = event.tmp[[s]]$team$name
    starting.xi.list[[s]] = starting.xi.team1
  }
  pass.index = which(unlist(lapply(event.tmp, function(x) x$type$name))=="Pass")
  pass.team1 = pass.index[which(unlist(lapply(pass.index, function(x) event.tmp[[x]]$team$id))==teamids[1])]
  pass.team1.df = data.frame(matrix(NA, nrow = 1, ncol = 11))
  colnames(pass.team1.df) = c("Possession", "Passer", "X.Pass", "Y.Pass", "Pass.Type",
                              "Receiver", "X.Receive", "Y.Receive", "Pass.Length", "Pass.Angle", 
                              "Body.Part")
  for (p in 1:length(pass.team1)) {
    pass.tmp = event.tmp[[pass.team1[p]]]
    possession = pass.tmp$possession
    passer = pass.tmp$player$id
    pass.location = pass.tmp$location
    pass.type = pass.tmp$pass$height$name
    receiver = pass.tmp$pass$recipient$id
    receive.location = pass.tmp$pass$end_location
    pass.length = pass.tmp$pass$length
    pass.angle = pass.tmp$pass$angle
    body.part = pass.tmp$pass$body_part$name
    
    row.toadd(possession, passer, pass.location, pass.type, receiver, receive.location, pass.length,
              pass.angle, body.part)
    pass.team1.df = rbind(pass.team1.df, row.toadd)
  }
  pass.team1.df = pass.team1.df[-1,]
  pass.team1.df[,c(1:4, 6:10)] = lapply(pass.team1.df[,c(1:4, 6:10)], as.numeric)
  
  pass.team1.df = pass.team1.df %>% group_by(Possession) %>% mutate(seq = row_number())
  pass.team1.df$team_id = teamids[1]
  
  pass.team2 = pass.index[which(unlist(lapply(pass.index, function(x) event.tmp[[x]]$team$id))=="Pass")]
  pass.team2.df = data.frame(matrix(NA, nrow = 1, ncol = 11))
  colnames(pass.team2.df) = c("Possession", "Passer", "X.Pass", "Y.Pass", "Pass.Type",
                              "Receiver", "X.Receive", "Y.Receive", "Pass.Length", "Pass.Angle", 
                              "Body.Part")
  
  for (p in 1:length(pass.team2)) {
    pass.tmp = event.tmp[[pass.team2[p]]]
    possession = pass.tmp$possession
    passer = pass.tmp$player$id
    pass.location = pass.tmp$location
    pass.type = pass.tmp$pass$height$name
    receiver = pass.tmp$pass$recipient$id
    receive.location = pass.tmp$pass$end_location
    pass.lenght = pass.tmp$pass$length
    pass.angle = pass.tmp$pass$angle
    body.part = pass.tmp$pass$body_part$name
    
    row.toadd = c(possession, passer, pass.location, pass.type, receiver, receive.location, pass.length, 
                  pass.angle, body.part)
    pass.team2.df = rbind(pass.team2, row.toadd)
  }
  pass.team2.df = pass.team2.df[-1, ]
  pass.team2.df[,c(1:4, 6:10)] = lapply(pass.team2.df[,c(1:4, 6:10)], as.numeric)
  pass.team2.df = passs.teams2.df %>% group_by(Possession) %>% mutate(seq = row_number())
  pass.team2.df$team_id = teamids[2]
  
  match.id = strsplit(basename(event.files[i]), "[.]")[[1]][1]
  event.list[[match.id]] = list(starting.xi.list, pass.list)
}

