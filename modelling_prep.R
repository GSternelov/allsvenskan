
# Structured shot data
shotMap_w_xg = function(data, games){
  tempDT = list()
  for(i in 1:length(data)){
    tempSM = data.table(data[[i]]$shotmap)
    
    tempSM = tempSM[, .(player.shortName, player.position, isHome, shotType, situation,
                        playerCoordinates.x, playerCoordinates.y, bodyPart,
                        time, timeSeconds, game = i)]
    
    tempSM[isHome == TRUE, Team := strsplit(games[[i]], split = " - ")[[1]][1]]
    tempSM[isHome == FALSE, Team := strsplit(games[[i]], split = " - ")[[1]][2]]
    
    tempSM[isHome == TRUE, Opponent := strsplit(games[[i]], split = " - ")[[1]][2]]
    tempSM[isHome == FALSE, Opponent := strsplit(games[[i]], split = " - ")[[1]][1]]
    
    tempDT[[i]] = tempSM
    
  }
  tempDT = rbindlist(tempDT)
  
  # Create pitch coordinates given pitch size 105x68
  tempDT[, x2 := (playerCoordinates.x / 100) * 105 ]
  tempDT[, y2 := 68 - ((playerCoordinates.y / 100) * 68)]
  
  # Distance to center of goal
  tempDT[, distCenter := round(sqrt((x2)^2 + (y2- 34)^2), 2)]
  # Angle to goal
  tempDT[, angleGoal := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 34)^2 - (7.32/2)^2))]
  tempDT[angleGoal < 0, angleGoal := angleGoal + pi]
  tempDT[, angleGoal := angleGoal * 180 / pi]
  
  tempDT[, situation2 := ifelse(situation %in% c("regular", "assisted"), "open play", situation)]
  tempDT[, situation2 := ifelse(situation %in% c("corner", "set-piece", "throw-in-set-piece"), "set-piece", situation2)]
  
  tempDT[, bodyPart2 := ifelse(bodyPart %in% c("left-foot", "right-foot"), "foot", bodyPart)]
  
  tempDT[situation2 != "penalty", xG := predict(xg_mod, newdata = tempDT[situation2 != "penalty"], type = "response")]
  tempDT[situation2 == "penalty", xG := 0.76]
  
  tempDT[, Goal := ifelse(shotType == "goal", TRUE, FALSE)]
  
  return(tempDT)
}

# xG given game state
state_data_func = function(data, smData){
  temp_data = list()
  for(i in 1:length(data)){
    temp = smData[game == i][order(timeSeconds)]
    temp[, npxG := ifelse(situation2 == "penalty", 0, xG)]
    
    temp[, homeState := 0]
    temp[, awayState := 0]
    
    temp[Goal == TRUE, homeState := ifelse(isHome == TRUE, 1, -1)]
    temp[Goal == TRUE, awayState := ifelse(isHome == TRUE, -1, 1)]
    
    temp[, homeState := shift(cumsum(homeState), fill = 0)]
    temp[, awayState := shift(cumsum(awayState), fill = 0)]
    
    temp[, sequence := cumsum(c(0, abs(diff(homeState))))]
    
    tempHome = temp[, .(time = max(timeSeconds), xG = sum(ifelse(isHome == TRUE, npxG, 0)),
                        xG_Against = sum(ifelse(isHome == FALSE, npxG, 0))), .(sequence, state = homeState)]
    tempHome[, Team := temp[isHome == TRUE, unique(Team)]]
    tempHome[, Opponent := temp[isHome == FALSE, unique(Team)]]
    tempHome[, Home := 1]
    if(tempHome[, all(state == 0)]){
      tempHome[, timeState := time - shift(time, fill = 0)]
    }else{
      tempHome[, timeState := time - shift(time, fill = 0)]
      tempHome[.N, timeState := ifelse(time > (93*60) & timeState < 60, timeState + 60, timeState)]
    }
    
    tempAway = temp[, .(time = max(timeSeconds), xG = sum(ifelse(isHome == FALSE, npxG, 0)),
                        xG_Against = sum(ifelse(isHome == TRUE, npxG, 0))), .(sequence, state = awayState)]
    tempAway[, Team := temp[isHome == FALSE, unique(Team)]]
    tempAway[, Opponent := temp[isHome == TRUE, unique(Team)]]
    tempAway[, Home := 0]
    if(tempAway[, all(state == 0)]){
      tempAway[, timeState := time - shift(time, fill = 0)]
    }else{
      tempAway[, timeState := time - shift(time, fill = 0)]
      tempAway[.N, timeState := ifelse(time > (93*60) & timeState < 60, timeState + 60, timeState)]
    }
    temp_data[[i]] = rbind(tempHome, tempAway)
  }
  temp_data = rbindlist(temp_data)
  temp_data[, stateClass := sign(state)]
  
  return(temp_data)
}

# Touches in zone 14
touchAttZone = function(data){
  # Must fix a unique ID for each game
  data[, TeamLag := shift(Team, n = 1)]
  data[, OpponentLag := shift(Opponent, n = 1)]
  
  data[, Game := ifelse(Team == TeamLag & Opponent == OpponentLag, 0, 1)]
  data[1, Game := 0]
  data[, Game := cumsum(Game)]
  data[, Game := (ifelse((Game %% 2) == 1, (Game - 1) / 2, Game / 2)) + 1]
  
  data[, x2 := ((x-min(x)) * 105) / (max(x) - min(x))]
  data[, y2 := ((y-min(y)) * 68) / (max(y) - min(y))]
  
  # Always same direction, independent on home/away
  # hmTable23[Team == "IFK Norrköping" & Player == "Oscar Jansson", .(mean(x2), mean(y2)), .(Opponent, Game)]
  
  # In zone 14
  data[, inPenaltyArea := ifelse(x2 >= 105 - 16.5 & y2 >= 34 - 17.7 & y2 <= 34 + 17.7,
                                      1, 0)]
  
  data[, zone14 := ifelse(x2 <= 105 - 16.5 & x2 >= 105 - ((52.5 / 3) * 2) & y2 >= 34 - 6.7 & y2 <= 34 + 6.7,
                               1, 0)]
  
  return(data[, .(zone14 = sum(zone14), inPenaltyArea = sum(inPenaltyArea)), .(Team, Opponent, Game)])
}


# Predict upcoming games

predGame = function(ht, at, minDraw = 40, touchData, plot = TRUE, gamModel){
  
  
  
  predHome = predict(gamModel, data.table(Home = 1, 
                                          zone14 = touchData[Team == ht, mean(zone14)],
                                          inPenaltyArea = touchData[Team == ht, mean(inPenaltyArea)],
                                          Team = ht, Opponent = at), type = "response") * minDraw
  predAway = predict(gamModel, data.table(Home = 0, 
                                          zone14 = touchData[Team == at, mean(zone14)],
                                          inPenaltyArea = touchData[Team == at, mean(inPenaltyArea)],
                                          Team = at, Opponent = ht), type = "response") * minDraw
  
  # Want to view the prediction as the mean and the variance to be fixed
  
  V = gamma.shape(gamModel)[1]$alpha
  
  shapeHome = (predHome^2) / V
  shapeAway = (predAway^2) / V
  
  scaleHome = V / predHome
  scaleAway = V / predAway
  
  predDT = data.table(predHome = rgamma(n = 10000, shape = shapeHome, scale = scaleHome),
                      predAway = rgamma(n = 10000, shape = shapeAway, scale = scaleAway))
  
  predDT[, c("goalsHome", "goalsAway") := .(round(predHome, 0), round(predAway, 0))]
  predDT[, Sign := sign(goalsHome - goalsAway)]
  
  if(plot == TRUE){
    print(predDT[, .(Prob = .N / predDT[, .N], Odds = 1 / (.N / predDT[, .N])), Sign][order(Sign)])
    
    p1 = ggplot(predDT, aes(x = goalsHome)) +
      geom_histogram(binwidth = 1, fill = "darkorange2", alpha = .65) +
      geom_histogram(aes(x = goalsAway), binwidth = 1, fill = "seagreen", alpha = .65) +
      theme_bw()
    
    p2 = ggplot(predDT, aes(x = Sign)) +
      geom_histogram(binwidth = 1, fill = "darkorange2", alpha = .65) +
      theme_bw()
    
    grid.arrange(p1, p2, ncol = 2)
  }
  
  
  prediction = predDT[, .(Prob = .N / predDT[, .N]), Sign][order(-Sign)]
  prediction[, Game := paste(ht, at, sep = " - ")]
  
  return(prediction)
}
