# Configuration of the SPADL


actiontypes_df = function(){
  df = data.frame(actiontypes)
  
  return(df)
}


fix.clearances = function(actions, home_team_id){
  next_actions = shift(actions, -1)
  next_actions[-1,] = actions[-1]
  clearance_idx = actions$type_id == which(actiontypes == "clearance"])[[1]]
}





