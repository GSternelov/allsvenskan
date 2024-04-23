library(data.table)

# Functions for reating shot data tables for shiny app

# Code for easily updating tables for current season

# Structured shot data
shotMap_w_xg = function(data, dataPlayer, games){
  tempDT = list()
  for(i in 1:length(data)){
    tempSM = data.table(data[[i]]$shotmap)
    
    if("goalType" %in% colnames(tempSM)){
      tempSM = tempSM[, .(player.shortName, player.position, isHome, shotType, situation,
                          playerCoordinates.x, playerCoordinates.y, bodyPart, goalType,
                          time, timeSeconds, game = i)]
    }else{
      tempSM = tempSM[, .(player.shortName, player.position, isHome, shotType, situation,
                          playerCoordinates.x, playerCoordinates.y, bodyPart, goalType = NA,
                          time, timeSeconds, game = i)]
    }
    
    tempSM[isHome == TRUE, Team := strsplit(games[[i]], split = " - ")[[1]][1]]
    tempSM[isHome == FALSE, Team := strsplit(games[[i]], split = " - ")[[1]][2]]
    
    tempSM[isHome == TRUE, Opponent := strsplit(games[[i]], split = " - ")[[1]][2]]
    tempSM[isHome == FALSE, Opponent := strsplit(games[[i]], split = " - ")[[1]][1]]
    
    homeGK = data.table(dataPlayer[[i]]$home$players)[position == "G" & !is.na(statistics.minutesPlayed)]
    awayGK = data.table(dataPlayer[[i]]$away$players)[position == "G" & !is.na(statistics.minutesPlayed)]
    
    if(homeGK[, .N] == 1){
      tempSM[isHome == FALSE, Goalkeeper := homeGK[, player.shortName]]
    }else{
      tempSM[isHome == FALSE & time <= homeGK[substitute == FALSE, statistics.minutesPlayed],
             Goalkeeper := homeGK[substitute == FALSE, player.shortName]]
      tempSM[isHome == FALSE & time > homeGK[substitute == FALSE, statistics.minutesPlayed],
             Goalkeeper := homeGK[substitute == TRUE, player.shortName]]
    }
    
    if(awayGK[, .N] == 1){
      tempSM[isHome == TRUE, Goalkeeper := awayGK[, player.shortName]]
    }else{
      tempSM[isHome == TRUE & time <= awayGK[substitute == FALSE, statistics.minutesPlayed],
             Goalkeeper := awayGK[substitute == FALSE, player.shortName]]
      tempSM[isHome == TRUE & time > awayGK[substitute == FALSE, statistics.minutesPlayed],
             Goalkeeper := awayGK[substitute == TRUE, player.shortName]]
    }
    
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

shotsDataFunction <- function(data, data_season){
  
  shots_summary = data[situation2 != "penalty", .(.N, Goals = sum(Goal), xG = sum(xG),
                                                  distCenter = sum(distCenter), angleGoal = sum(angleGoal)),
                       .(player.shortName, player.position, playerTeam)][order(player.shortName, -N)]
  shots_summary[, position := head(player.position, 1), player.shortName]
  shots_summary[, position := zoo::na.locf.default(position, na.rm = FALSE)]
  
  shots_summary = shots_summary[N > 2, .(N = sum(N), Goals = sum(Goals), xG = sum(xG),
                                         distCenter = sum(distCenter) / sum(N), angleGoal = sum(angleGoal) / sum(N)),
                                .(player.shortName, position, playerTeam)][N > 15]
  
  shots_type = data[player.shortName %in% shots_summary[, player.shortName] & bodyPart2 != "other",
                    .N, .(player.shortName, playerTeam, bodyPart2)]
  shots_type[, P := N / sum(N), player.shortName]
  
  shots_summary = merge(shots_summary, 
                        dcast(shots_type, player.shortName + playerTeam ~ bodyPart2, value.var = "P", fill = 0),
                        by = c("player.shortName", "playerTeam"))
  
  gk_summary = data[situation2 != "penalty", .(.N, Goals = sum(Goal), xG = sum(xG),
                                               distCenter = mean(distCenter), angleGoal = mean(angleGoal)),
                    .(Goalkeeper, Team = Opponent)]
  
  gk_data = data[situation2 != "penalty", .(Goalkeeper, game, Team = Opponent, x2, y2, distCenter, angleGoal, situation2,
                                            bodyPart2, xG, Goal)]
  
  shots_summary[, season := data_season]
  gk_summary[, season := data_season]
  gk_data[, season := data_season]
  
  return(list(shots_summary, gk_summary, gk_data))
}

# 2024


# 2023

load("Allsv23Shotmaps.rda")
load("Allsv23Games.rda")
load("Allsv23PlayerData.rda")
load("xg_mod.rda")

shotMapDT23 = shotMap_w_xg(data = Allsv23Shotmaps, dataPlayer = Allsv23, games = Games23)

shotMapDT23 = shotMapDT23[(goalType == "own") == FALSE | is.na(goalType) ]

shotMapDT23[player.shortName == 'A. Carlen', player.shortName := "A. Carlén"]
shotMapDT23[player.shortName == 'T. Ali', player.shortName := "T. A. Ali"]
shotMapDT23[player.shortName == 'E. Júnior', player.shortName := "E. Junior"]
shotMapDT23[player.shortName == 'N. Dahlstrom', player.shortName := "N. Dahlström"]
shotMapDT23[player.shortName == 'R. Sjostedt', player.shortName := "R. Sjöstedt"]

shotMapDT23[, playerTeam := tail(Team, 1), player.shortName]

shotMapDT23[, season := "2023"]

shots_data_tables = shotsDataFunction(shotMapDT23, "2023")

shots_summary23 = shots_data_tables[[1]]
gk_summary23 = shots_data_tables[[2]]
gk_data23 = shots_data_tables[[3]]

load("minPlayer.rda")

minPlayer[, totMin := sum(Min), .(Player, Age)]

shots_summary23 = merge(shots_summary23, minPlayer[, .(player.shortName = Player ,playerTeam = Team, totMin)], by = c("player.shortName", "playerTeam"))

shots_summary23[, shots90 := round(N / (totMin / 90), 3)]
shots_summary23[, xGshot := xG / N]

save(shots_summary23, file = "shots_summary23.rda")
save(shotMapDT23, file = "shotMapDT23.rda")
save(gk_data23, file = "gk_data23.rda")
save(gk_summary23, file = "gk_summary23.rda")

# 2022

load("Allsv22Shotmaps.rda")
load("Allsv22Games.rda")
Games22 = as.list(Games22)
load("Allsv22PlayerData.rda")
load("xg_mod.rda")

shotMapDT22 = shotMap_w_xg(data = Allsv22Shotmaps, dataPlayer = Allsv22, games = Games22)

shotMapDT22 = shotMapDT22[(goalType == "own") == FALSE | is.na(goalType) ]

shotMapDT22[player.shortName == 'A. Al-Hamawi', player.shortName := "A. Al-Hamlawi"]

shotMapDT22[, playerTeam := tail(Team, 1), player.shortName]

shotMapDT22[, season := "2022"]

shots_data_tables = shotsDataFunction(shotMapDT22, "2022")

shots_summary22 = shots_data_tables[[1]]
gk_summary22 = shots_data_tables[[2]]
gk_data22 = shots_data_tables[[3]]

save(shots_summary22, file = "shots_summary22.rda")
save(shotMapDT22, file = "shotMapDT22.rda")
save(gk_data22, file = "gk_data22.rda")
save(gk_summary22, file = "gk_summary22.rda")
