# Class to load Event data should later be converted through base class and converter
# so that we can get SPADL representation
library(rjson)
library(R6)

StatsBombLoader = R6Class("StatsBombLoader", 
                          inherit = EventDataLoader,
                          public = list(
                            #probable inits
                            root = "https://raw.githubusercontent.com/statsbomb/open-data/master/data/"
                            comp.ids = c(2, 11, 16, 37, 43, 49, 72)
                            #method
                            competitions = function(){
                              # return a dataframe with all available competitions and seasons
                              # Returns
                              # --------
                              #   data.frame
                              #   a data frame containing all available competitions and seasons.
                              path = paste(root, "competitions.json")
                              obj = fromJSON(file = path)
                              competitions.df = data.frame(do.call(rbind(obj)), stringsAsFactors = FALSE)
                            },
                            
                            matches = function(){
                              
                            }
                            )
                          )
                          
                          
                      

