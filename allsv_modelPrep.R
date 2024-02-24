library(data.table)
library(TTR)


load("Allsv21Games.rda")
load("Allsv21Heatmap.rda")
load("Allsv21PlayerData.rda")
load("Allsv21TeamData.rda")
load("GamesDate21.rda")

gameStat21 = list()
k = 0
for(i in 1:length(Allsv21)){
  k = k+1
  
  shotsInBoxInd = which(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]]$name == "Shots inside box")
  bgInd = which(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]]$name == "Big chances")
  
  temp = data.table(Allsv21[[i]]$home$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  temp[, bigChances := as.numeric(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][bgInd, 2])]
  temp[, shotsInsideBox := as.numeric(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][shotsInBoxInd, 2])]
  temp[, shotsOutsideBox := as.numeric(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][(shotsInBoxInd +1), 2])]
  temp[, Team := strsplit(Games21[[i]], split = " - ")[[1]][1]]
  temp[, Opponent := strsplit(Games21[[i]], split = " - ")[[1]][2]]
  temp[, oppOwnGaols := sum(Allsv21[[i]]$away$players$statistics$ownGoals, na.rm = TRUE)]
  temp[, goalsAgainst := sum(Allsv21[[i]]$away$players$statistics$goals, na.rm = TRUE)]
  gameStat21[[k]] <- temp
  k = k+1
  temp = data.table(Allsv21[[i]]$away$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  temp[, bigChances := as.numeric(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][bgInd, 3])]
  temp[, shotsInsideBox := as.numeric(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][shotsInBoxInd, 3])]
  temp[, shotsOutsideBox := as.numeric(Allsv21TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][(shotsInBoxInd + 1), 3])]
  temp[, Team := strsplit(Games21[[i]], split = " - ")[[1]][2]]
  temp[, Opponent := strsplit(Games21[[i]], split = " - ")[[1]][1]]
  temp[, oppOwnGaols := sum(Allsv21[[i]]$home$players$statistics$ownGoals, na.rm = TRUE)]
  temp[, goalsAgainst := sum(Allsv21[[i]]$home$players$statistics$goals, na.rm = TRUE)]
  gameStat21[[k]] <- temp
}
gameStat21 = rbindlist(gameStat21, fill = TRUE)
wrongInd = gameStat21[, which(is.na(shotsInsideBox))]
gameStat21[wrongInd, shotsInsideBox := bigChances]
gameStat21[wrongInd, bigChances := 0]

setnafill(x = gameStat21, fill = 0, cols = which(names(gameStat21) %in% c("Team", "Opponent") == FALSE))

# Must fix a unique ID for each game
hmTable[, TeamLag := shift(Team, n = 1)]
hmTable[, OpponentLag := shift(Opponent, n = 1)]

hmTable[, Game := ifelse(Team == TeamLag & Opponent == OpponentLag, 0, 1)]
hmTable[1, Game := 0]
hmTable[, Game := cumsum(Game) + 1]

hmTable[, inPenaltyArea := ifelse(x >= 100 - (100 / 105) * 16.5 & y >= 50 - (100 / 68) * 17.7 & y <= 50 + (100 / 68) * 17.7, 1, 0)]
hmTable[, finalThird := ifelse(x >= 66.67, 1, 0)]

# Create pitch coordinates given pitch size 105x68
hmTable[, x2 := ((x-min(x)) * 105) / (max(x) - min(x))]
hmTable[, y2 := ((y-min(y)) * 68) / (max(y) - min(y))]

# Check if touch was in 5% zone, i.e. 
# Distance to center of goal
hmTable[, distCenter := round(sqrt((x2 - 105)^2 + (y2- 34)^2), 2)]
# Angle to goal
hmTable[, angleGoal := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 34)^2 - (7.32/2)^2))]
hmTable[angleGoal < 0, angleGoal := angleGoal + pi]
hmTable[, angleGoal := angleGoal * 180 / pi]

load("xGmod.rda")

hmTable[, xG := predict(xg_mod, newdata = hmTable[, .(x1 = x2, y1 = y2, distCenter, angleGoal, head = FALSE)], type = "response")]

hmTable[, in5perc := ifelse(xG >= 0.05 & angleGoal > 5, 1, 0)]
hmTable[, in1perc := ifelse(xG >= 0.01, 1, 0)]

touchData = hmTable[, .(finalThird = sum(finalThird), inPenaltyArea = sum(inPenaltyArea), 
                        in5perc = sum(in5perc), in1perc = sum(in1perc)), .(Game, Team)]
gameStat21[, finalThird := touchData[, finalThird]]
gameStat21[, inPenaltyArea := touchData[, inPenaltyArea]]
gameStat21[, ft_longball := finalThird / totalLongBalls]
gameStat21[, ft_longball := finalThird / totalLongBalls]
gameStat21[, in5perc := touchData[, in5perc]]
gameStat21[, in1perc := touchData[, in1perc]]

gameStat21[, Points := ifelse((goals + oppOwnGaols) > (goalsAgainst + ownGoals), 3, 1)]
gameStat21[, Points := ifelse((goals + oppOwnGaols) < (goalsAgainst + ownGoals), 0, Points)]

gameStat21[, Sign := sign((goals + oppOwnGaols) - (goalsAgainst + ownGoals))]

gameStat21[, in5perc_w := in5perc / ((totalLongBalls / totalPass)*10)]


gameStat21[, Game := rep(1:length(Allsv21), each = 2)]

gameData21 = gameStat21[, .(duelP = duelWon / (duelWon + duelLost),
                            aerialDuelP = aerialWon / (aerialWon + aerialLost),
                            shotsIB = shotsInsideBox,
                            ft = finalThird,
                            pa = inPenaltyArea,
                            in5perc_w = in5perc_w,
                            in1perc = in1perc,
                            ft_lb = ft_longball,
                            bigChances = bigChances,
                            Sign,
                            Goals = goals - (penaltyWon - penaltyMiss)),
                        .(Team, Opponent, Game)]
gameData21[, h_a := 1:.N, Game]
gameData21[, Date := rep(GamesDate, each = 2)]
gameData21[, gameOrder := 1:.N, Team]
gameData21 = gameData21[order(Team, Date, Game)]

for(i in 1:gameData21[, .N]){
  oppData = gameData21[Game == gameData21[i, Game] & Team != gameData21[i, Team]]
  gameData21[i, oppShotsIB := oppData[, shotsIB]]
  gameData21[i, oppIn5perc_w := oppData[, in5perc_w]]
  gameData21[i, oppIn1perc := oppData[, in1perc]]
  gameData21[i, oppBigChances := oppData[, bigChances]]
  
}




load("Allsv22Games.rda")
load("Allsv22Heatmap.rda")
load("Allsv22PlayerData.rda")
load("Allsv22TeamData.rda")
load("GamesDate22.rda")

gameStat22 = list()
k = 0
for(i in 1:length(Allsv22)){
  k = k+1
  
  shotsInBoxInd = which(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]]$name == "Shots inside box")
  bgInd = which(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]]$name == "Big chances")
  
  temp = data.table(Allsv22[[i]]$home$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  temp[, bigChances := as.numeric(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][bgInd, 2])]
  temp[, shotsInsideBox := as.numeric(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][shotsInBoxInd, 2])]
  temp[, shotsOutsideBox := as.numeric(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][(shotsInBoxInd +1), 2])]
  temp[, Team := strsplit(Games22[[i]], split = " - ")[[1]][1]]
  temp[, Opponent := strsplit(Games22[[i]], split = " - ")[[1]][2]]
  temp[, oppOwnGaols := sum(Allsv22[[i]]$away$players$statistics$ownGoals, na.rm = TRUE)]
  temp[, goalsAgainst := sum(Allsv22[[i]]$away$players$statistics$goals, na.rm = TRUE)]
  gameStat22[[k]] <- temp
  k = k+1
  temp = data.table(Allsv22[[i]]$away$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  temp[, bigChances := as.numeric(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][bgInd, 3])]
  temp[, shotsInsideBox := as.numeric(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][shotsInBoxInd, 3])]
  temp[, shotsOutsideBox := as.numeric(Allsv22TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][(shotsInBoxInd + 1), 3])]
  temp[, Team := strsplit(Games22[[i]], split = " - ")[[1]][2]]
  temp[, Opponent := strsplit(Games22[[i]], split = " - ")[[1]][1]]
  temp[, oppOwnGaols := sum(Allsv22[[i]]$home$players$statistics$ownGoals, na.rm = TRUE)]
  temp[, goalsAgainst := sum(Allsv22[[i]]$home$players$statistics$goals, na.rm = TRUE)]
  gameStat22[[k]] <- temp
}
gameStat22 = rbindlist(gameStat22, fill = TRUE)
wrongInd = gameStat22[, which(is.na(shotsInsideBox))]
gameStat22[wrongInd, shotsInsideBox := bigChances]
gameStat22[wrongInd, bigChances := 0]

setnafill(x = gameStat22, fill = 0, cols = which(names(gameStat22) %in% c("Team", "Opponent") == FALSE))

# Must fix a unique ID for each game
hmTable[, TeamLag := shift(Team, n = 1)]
hmTable[, OpponentLag := shift(Opponent, n = 1)]

hmTable[, Game := ifelse(Team == TeamLag & Opponent == OpponentLag, 0, 1)]
hmTable[1, Game := 0]
hmTable[, Game := cumsum(Game) + 1]

hmTable[, inPenaltyArea := ifelse(x >= 100 - (100 / 105) * 16.5 & y >= 50 - (100 / 68) * 17.7 & y <= 50 + (100 / 68) * 17.7, 1, 0)]
hmTable[, finalThird := ifelse(x >= 66.67, 1, 0)]

# Create pitch coordinates given pitch size 105x68
hmTable[, x2 := ((x-min(x)) * 105) / (max(x) - min(x))]
hmTable[, y2 := ((y-min(y)) * 68) / (max(y) - min(y))]

# Check if touch was in 5% zone, i.e. 
# Distance to center of goal
hmTable[, distCenter := round(sqrt((x2 - 105)^2 + (y2- 34)^2), 2)]
# Angle to goal
hmTable[, angleGoal := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 34)^2 - (7.32/2)^2))]
hmTable[angleGoal < 0, angleGoal := angleGoal + pi]
hmTable[, angleGoal := angleGoal * 180 / pi]

load("xGmod.rda")

hmTable[, xG := predict(xg_mod, newdata = hmTable[, .(x1 = x2, y1 = y2, distCenter, angleGoal, head = FALSE)], type = "response")]

hmTable[, in5perc := ifelse(xG >= 0.05 & angleGoal > 5, 1, 0)]
hmTable[, in1perc := ifelse(xG >= 0.01, 1, 0)]

touchData = hmTable[, .(finalThird = sum(finalThird), inPenaltyArea = sum(inPenaltyArea), 
                        in5perc = sum(in5perc), in1perc = sum(in1perc)), .(Game, Team)]
gameStat22[, finalThird := touchData[, finalThird]]
gameStat22[, inPenaltyArea := touchData[, inPenaltyArea]]
gameStat22[, ft_longball := finalThird / totalLongBalls]
gameStat22[, ft_longball := finalThird / totalLongBalls]
gameStat22[, in5perc := touchData[, in5perc]]
gameStat22[, in1perc := touchData[, in1perc]]

# No own goals so far
# gameStat22[, Points := ifelse((goals) > (goalsAgainst), 3, 1)]
# gameStat22[, Points := ifelse((goals) < (goalsAgainst), 0, Points)]

gameStat22[, Points := ifelse((goals + oppOwnGaols) > (goalsAgainst + ownGoals), 3, 1)]
gameStat22[, Points := ifelse((goals + oppOwnGaols) < (goalsAgainst + ownGoals), 0, Points)]

gameStat22[, Sign := sign((goals + oppOwnGaols) - (goalsAgainst + ownGoals))]
# gameStat22[, Sign := sign((goals) - (goalsAgainst))]

gameStat22[, in5perc_w := in5perc / ((totalLongBalls / totalPass)*10)]


gameStat22[, Game := rep(1:length(Allsv22), each = 2)]

gameData22 = gameStat22[, .(duelP = duelWon / (duelWon + duelLost),
                            aerialDuelP = aerialWon / (aerialWon + aerialLost),
                            shotsIB = shotsInsideBox,
                            ft = finalThird,
                            pa = inPenaltyArea,
                            in5perc_w = in5perc_w,
                            in1perc = in1perc,
                            ft_lb = ft_longball,
                            bigChances = bigChances,
                            Sign,
                            Goals = goals - (penaltyWon - penaltyMiss)),
                            #Goals = goals - (penaltyWon)),
                        .(Team, Opponent, Game)]
gameData22[, h_a := 1:.N, Game]
gameData22[, Date := rep(GamesDate, each = 2)]
gameData22[, gameOrder := 1:.N, Team]
gameData22 = gameData22[order(Team, Date, Game)]

for(i in 1:gameData22[, .N]){
  oppData = gameData22[Game == gameData22[i, Game] & Team != gameData22[i, Team]]
  gameData22[i, oppShotsIB := oppData[, shotsIB]]
  gameData22[i, oppIn5perc_w := oppData[, in5perc_w]]
  gameData22[i, oppIn1perc := oppData[, in1perc]]
  gameData22[i, oppBigChances := oppData[, bigChances]]
  
}

gameData2122 = rbind(gameData21, gameData22)




gameDataAll = rbind(gameData[, 1:16], gameData21[, 1:16])
gameDataAll[1:480, Season := 2020]
gameDataAll[481:.N, Season := 2021]

for(i in 1:gameDataAll[, .N]){
  oppData = gameDataAll[Game == gameDataAll[i, Game] & Team != gameDataAll[i, Team] & Season == gameDataAll[i, Season]]
  gameDataAll[i, oppShotsIB := oppData[, shotsIB]]
  gameDataAll[i, oppIn5perc_w := oppData[, in5perc_w]]
  gameDataAll[i, oppIn1perc := oppData[, in1perc]]
  gameDataAll[i, oppBigChances := oppData[, bigChances]]
}
# To make the in5perc_wAvg easier to model, I set a max value of 50
gameDataAll[, in5perc_w := ifelse(in5perc_w > 50, 50, in5perc_w)]
gameDataAll[, oppIn5perc_w := ifelse(oppIn5perc_w > 50, 50, oppIn5perc_w)]

gameDataAll[, duelPavg := SMA(x = duelP, n = 8), Team]
gameDataAll[, aerialDuelPavg := SMA(x = aerialDuelP, n = 8), Team]
gameDataAll[, shotsIBavg := SMA(x = shotsIB, n = 8), Team]
gameDataAll[, ftAvg := SMA(x = ft, n = 8), Team]
gameDataAll[, paAvg := SMA(x = pa, n = 8), Team]
gameDataAll[, ft_lbAvg := SMA(x = ft_lb, n = 8), Team]
gameDataAll[, bigChancesAvg := SMA(x = bigChances, n = 8), Team]
gameDataAll[, in5perc_wAvg := SMA(x = in5perc_w, n = 8), Team]
gameDataAll[, in1percAvg := SMA(x = in1perc, n = 8), Team]

gameDataAll[, oppShotsIBavg := SMA(x = oppShotsIB, n = 8), Team]
gameDataAll[, oppIn5perc_wAvg := SMA(x = oppIn5perc_w, n = 8), Team]
gameDataAll[, oppIn1percAvg := SMA(x = oppIn1perc, n = 8), Team]
gameDataAll[, oppBigChancesAvg := SMA(x = oppBigChances, n = 8), Team]


trainData = list()
k = 0
# Starting at game 65, equal to 8 completed rounds
# Then, selecting the last game for each team by date instead of round or gameorder

gameDataAll[Season == 2021, Game := Game + 240]

for(i in 65:gameDataAll[, max(Game)]){
  homeTeam = gameDataAll[Game == i & h_a == 1, Team]
  awayTeam = gameDataAll[Game == i & h_a == 2, Team]
  k = k+1
  gameDate = gameDataAll[Game == i & h_a == 1, Date]
  
  # home team data
  h_g = gameDataAll[Team == homeTeam & Date == gameDate, Goals]
  trainData[[k]] = 
    cbind(tail(gameDataAll[Team == homeTeam & Date < gameDate, .(Team = homeTeam, game = i, Goals = h_g, h_a = 1, shotsIBavg, in5perc_wAvg, in1percAvg)], 1),
          tail(gameDataAll[Team == awayTeam & Date < gameDate, .(oppShotsIBavg, oppIn5perc_wAvg, oppIn1percAvg)], 1))
  k = k+1
  # Away team data
  a_g = gameDataAll[Team == awayTeam & Date == gameDate, Goals]
  
  trainData[[k]] = 
    cbind(tail(gameDataAll[Team == awayTeam & Date < gameDate, .(Team = awayTeam, game = i, Goals = a_g, h_a = 0, shotsIBavg, in5perc_wAvg, in1percAvg)], 1),
          tail(gameDataAll[Team == homeTeam & Date < gameDate, .(oppShotsIBavg, oppIn5perc_wAvg, oppIn1percAvg)], 1))
}
trainData = rbindlist(trainData)

bayesData <- list(X= as.matrix(na.omit(trainData[, c(4:10)])),  # predictors
                  Goals=na.omit(trainData)[, Goals],  
                  N=na.omit(trainData)[, .N])  # sample size

modelString = "model{
    ## Likelihood
    for(i in 1:N){
      Goals[i] ~ dpois(lambda[i])
      log(lambda[i]) <- mu[i]
      mu[i] <- b0 + b1 * X[i, 1] + b2 * X[i, 2] + b3 * X[i, 3] + b4 * X[i, 4] + b5 * X[i, 5] + b6 * X[i, 6] + b7 * X[i, 7]
    } 
    
    ## Priors 
    b0 ~ dnorm(0, 0.0625)
    b1 ~ dnorm(0, 0.0625)
    b2 ~ dnorm(0, 0.0625)
    b3 ~ dnorm(0, 0.0625)
    b4 ~ dnorm(0, 0.0625)
    b5 ~ dnorm(0, 0.0625)
    b6 ~ dnorm(0, 0.0625)
    b7 ~ dnorm(0, 0.0625)
}"

library(rjags)
jagsmodel <- jags.model(file = textConnection(modelString), data=bayesData, n.chains=3, n.adapt=20000)
## Burning some samples
update(jagsmodel, 12500)
## Generate MCMC samples
s1 <- coda.samples(jagsmodel, variable.names=c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7"), n.iter=20000, thin=5)
## Merge the MCMC chains into one matrix
ms1 <- as.matrix(s1)
ms1df <- data.frame(ms1)

exp(colMeans(ms1df))

ms1dt = data.table(ms1df)
### Save this table so I don't have to re-train the model every time
# Suppose it is fair to assume model parameters not should change that much from round to round
save(ms1dt, file = "allsv_bayesModel_posterior.rda")

load("allsv_bayesModel_posterior.rda")

ggplot(melt(ms1dt[, .(id = 1:.N, int = b0, h_a = b1, shotsIBavg = b2, in5perc_wAvg = b3, in1percAvg = b4, oppShotsIBavg = b5, oppIn5perc_wAvg = b6, oppIn1percAvg = b7)],
            id.vars = 1), aes(x = exp(value))) + geom_histogram(binwidth = 0.01) +
  facet_wrap(~variable, scales = "free")

ggplot(melt(ms1dt[, .(id = 1:.N, int = b0, h_a = b1, shotsIBavg = b2, in5perc_wAvg = b3, in1percAvg = b4, oppShotsIBavg = b5, oppIn5perc_wAvg = b6, oppIn1percAvg = b7)],
            id.vars = 1), aes(x = id, y = exp(value))) + geom_line() +
  facet_wrap(~variable, scales = "free")


# Here, extend the gameData so it includes games played in 2021 as well
gameData_pred = rbind(gameData[, .(Team, Game, Year = 2020, shotsIB, in5perc_w, in1perc, oppShotsIB, oppIn5perc_w, oppIn1perc)],
                      gameData21[, .(Team, Game, Year = 2021, shotsIB, in5perc_w, in1perc, oppShotsIB, oppIn5perc_w, oppIn1perc)])
rounds = gameData_pred[Team == "Kalmar FF", .N]
rounds2 = gameData_pred[Team == "IF Elfsborg", .N]
rounds3 = gameData_pred[Team == "Degerfors IF", .N]

# Probably want to insert the weighted mean of last season instead of the last 8 game rolling average
gameWeight = data.table(y = dexp(seq(0, 0.5, length.out = rounds), rate = 3), x = rounds:1)
# Min-max normalization [0.75, 1]
gameWeight[, y := 0.75 + ((y - min(y)) * (1-0.75)) / (max(y) - (min(y)))]
gameWeight = gameWeight[order(x)]

gameWeight2 = data.table(y = dexp(seq(0, 0.5, length.out = rounds2), rate = 3), x = rounds2:1)
gameWeight2[, y := 0.75 + ((y - min(y)) * (1-0.75)) / (max(y) - (min(y)))]
gameWeight2 = gameWeight2[order(x)]

gameWeight3 = data.table(y = dexp(seq(0, 0.5, length.out = rounds3), rate = 1.5), x = rounds3:1)
gameWeight3[, y := 0.9 + ((y - min(y)) * (1-0.9)) / (max(y) - (min(y)))]
gameWeight3 = gameWeight3[order(x)]

featurePred = gameData_pred[(Team %in% c("Helsingborgs IF", "Falkenbergs FF", "Malmö FF", "IF Elfsborg",
                                         "Halmstads BK", "Degerfors IF")) == FALSE,
                            .(shotsIBavg = weighted.mean(shotsIB, w = gameWeight[, y]),
                              in5perc_wAvg = weighted.mean(in5perc_w, w = gameWeight[, y]),
                              in1percAvg = weighted.mean(in1perc, w = gameWeight[, y]),
                              oppShotsIBavg = weighted.mean(oppShotsIB, w = gameWeight[, y]),
                              oppIn5perc_wAvg = weighted.mean(oppIn5perc_w, w = gameWeight[, y]),
                              oppIn1percAvg = weighted.mean(oppIn1perc, w = gameWeight[, y])), Team]

featurePred2 = gameData_pred[(Team %in% c("Malmö FF", "IF Elfsborg")),
                            .(shotsIBavg = weighted.mean(shotsIB, w = gameWeight2[, y]),
                              in5perc_wAvg = weighted.mean(in5perc_w, w = gameWeight2[, y]),
                              in1percAvg = weighted.mean(in1perc, w = gameWeight2[, y]),
                              oppShotsIBavg = weighted.mean(oppShotsIB, w = gameWeight2[, y]),
                              oppIn5perc_wAvg = weighted.mean(oppIn5perc_w, w = gameWeight2[, y]),
                              oppIn1percAvg = weighted.mean(oppIn1perc, w = gameWeight2[, y])), Team]

featurePred3 = gameData_pred[(Team %in% c("Halmstads BK", "Degerfors IF")),
                             .(shotsIBavg = weighted.mean(shotsIB, w = gameWeight3[, y]),
                               in5perc_wAvg = weighted.mean(in5perc_w, w = gameWeight3[, y]),
                               in1percAvg = weighted.mean(in1perc, w = gameWeight3[, y]),
                               oppShotsIBavg = weighted.mean(oppShotsIB, w = gameWeight3[, y]),
                               oppIn5perc_wAvg = weighted.mean(oppIn5perc_w, w = gameWeight3[, y]),
                               oppIn1percAvg = weighted.mean(oppIn1perc, w = gameWeight3[, y])), Team]

featurePred = rbindlist(list(featurePred, featurePred2, featurePred3))

predict_func = function(ht, at, homeExtra = 0){
  homeTeam = ht
  awayTeam = at
  # home team data
  homeData = as.matrix(cbind(featurePred[Team == homeTeam, .(h_a = 1, shotsIBavg, in5perc_wAvg, in1percAvg)],
                             featurePred[Team == awayTeam, .(oppShotsIBavg, oppIn5perc_wAvg, oppIn1percAvg)]))
  # Away team data
  awayData = as.matrix(cbind(featurePred[Team == awayTeam, .(h_a = 0, shotsIBavg, in5perc_wAvg, in1percAvg)],
                             featurePred[Team == homeTeam, .(oppShotsIBavg, oppIn5perc_wAvg, oppIn1percAvg)]))
  # home
  pred = exp(ms1dt[ , b0] +
               homeData[, 1] * ms1dt[ , b1 + homeExtra] +
               homeData[, 2] * ms1dt[ , b2] +
               homeData[, 3] * ms1dt[ , b3] +
               homeData[, 4] * ms1dt[ , b4] + 
               homeData[, 5] * ms1dt[ , b5] + 
               homeData[, 6] * ms1dt[ , b6] + 
               homeData[, 7] * ms1dt[ , b7])
  home <- rpois(length(pred) , pred)
  # away
  pred = exp(ms1dt[ , b0] +
               awayData[, 1] * ms1dt[ , b1] +
               awayData[, 2] * ms1dt[ , b2] +
               awayData[, 3] * ms1dt[ , b3] +
               awayData[, 4] * ms1dt[ , b4] + 
               awayData[, 5] * ms1dt[ , b5] + 
               awayData[, 6] * ms1dt[ , b6] + 
               awayData[, 7] * ms1dt[ , b7])
  away <- rpois(length(pred) , pred)
  
  print(ggplot(data.table(Goals = c(home, away), Team = rep(c("Home", "Away"), each = length(home))), aes(x = Goals, fill = Team)) +
          geom_histogram(binwidth = 1, position = "identity", alpha = .5) + labs(title = paste(ht, at, sep = "- ")))
  
  
  #print(paste("p1:", round(mean(home > away), 3), "pX:", round(mean(home == away), 3), "p2:", round(mean(home < away), 3)))
  #print(paste("p1:", round(1 / mean(home > away), 3), "pX:", round(1 / mean(home == away), 3), "p2:", round(1 / mean(home < away), 3)))
  
  return( data.table(homeTeam = ht, awayTeam = at,
                     p1 = round(mean(home > away), 3), px = round(mean(home == away), 3), p2 = round(mean(home < away), 3),
                     o1 = round(1 / mean(home > away), 3), ox = round(1 / mean(home == away), 3), o2 = round(1 / mean(home < away), 3),
                     p2_5 = round(mean((home + away) > 2.5), 3), o2_5 = round(1 / mean((home + away) > 2.5), 3)))
}


next_round = data.table(homeTeam = c("IK Sirius", "Malmö FF", "Djurgårdens IF", "Kalmar FF", "IFK Göteborg", "IF Elfsborg", "Örebro SK", "Mjällby AIF"),
                        awayTeam = c("Östersunds FK", "IFK Norrköping", "Hammarby IF", "Degerfors IF", "Halmstads BK", "BK Häcken", "Varbergs BoIS", "AIK"))

next_round2 = data.table(homeTeam = c("Malmö FF", "Degerfors IF", "BK Häcken", "Östersunds FK", "IFK Norrköping", "Halmstads BK", "Hammarby IF", "AIK"),
                        awayTeam = c("Djurgårdens IF", "IK Sirius", "Mjällby AIF", "IF Elfsborg", "Örebro SK", "Kalmar FF", "Varbergs BoIS", "IFK Göteborg"))

predictions = list()
for(i in 1:next_round2[, .N]){
  predictions[[i]] = predict_func(next_round2[i, homeTeam], next_round2[i, awayTeam], homeExtra = 0)
}

predictions = rbindlist(predictions)

library(DT)

datatable(predictions)


load("Allsv21Games.rda")
load("Allsv21Heatmap.rda")
load("Allsv21PlayerData.rda")
load("Allsv21TeamData.rda")
load("GamesDate21.rda")


# Create pitch coordinates given pitch size 105x68
hmTable[, x2 := ((x-min(x)) * 105) / (max(x) - min(x))]
hmTable[, y2 := ((y-min(y)) * 68) / (max(y) - min(y))]

hmTable[, range(x2)]; hmTable[, range(y2)]

# Check if touch was in 5% zone, i.e. 
# Distance to center of goal
hmTable[, distCenter := round(sqrt((x2 - 105)^2 + (y2- 34)^2), 2)]
# Angle to goal
hmTable[, angleGoal := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 34)^2 - (7.32/2)^2))]
hmTable[angleGoal < 0, angleGoal := angleGoal + pi]
hmTable[, angleGoal := angleGoal * 180 / pi]

load("C:\\Users\\bt4967\\Desktop\\Mathematical-Modelling-of-Football\\xGmod.rda")

hmTable[, xG := predict(xg_mod, newdata = hmTable[, .(x1 = x2, y1 = y2, distCenter, angleGoal, head = FALSE)], type = "response")]

hmTable[, in5perc := ifelse(xG >= 0.05 & angleGoal > 5, 1, 0)]
hmTable[, in1perc := ifelse(xG >= 0.01, 1, 0)]

matchData = list()
for(i in 1:length(Games21)){
  teams = strsplit(Games21[[i]], split = " - ")[[1]]
  homeTeam = teams[1]
  awayTeam = teams[2]
  
  # Add data to long table and to wide table
  home_touch = hmTable[Team == homeTeam & Opponent == awayTeam, .(in5perc = sum(in5perc), in1perc = sum(in1perc),
                                                                  in5perc_xG = sum(ifelse(in5perc == 1, xG, 0)))]
  
  away_touch = hmTable[Team == awayTeam & Opponent == homeTeam, .(in5perc = sum(in5perc), in1perc = sum(in1perc),
                                                                  in5perc_xG = sum(ifelse(in5perc == 1, xG, 0)))]
  # Dividing in5perc_xG by in5Perc gives the average value/proximity to goal of the touches
  
  passes = c(sum(Allsv21[[i]]$home$players$statistics$totalPass, na.rm = TRUE),
             sum(Allsv21[[i]]$away$players$statistics$totalPass, na.rm = TRUE))
  longballs = c(sum(Allsv21[[i]]$home$players$statistics$totalLongBalls, na.rm = TRUE),
                sum(Allsv21[[i]]$away$players$statistics$totalLongBalls, na.rm = TRUE))
  
  teamData = Allsv21TeamStats[[i]]$statistics$groups[[1]][4, ]$statisticsItems[[1]]
  sib = which(teamData$name == "Shots inside box")
  if(length(sib) > 0){
    sib = as.numeric(teamData[sib, c(2,3)])
  }else{
    sib = c(0, 0)
  }
  
  matchData[[i]] = data.table(Team = c(homeTeam, awayTeam), Opponent = c(awayTeam, homeTeam), h_a = c(1, 0), 
                              sib = sib, sib_against = rev(sib),
                              in1perc = c(home_touch[, in1perc], away_touch[, in1perc]), in1perc_against = c(away_touch[, in1perc], home_touch[, in1perc]),
                              in5perc = c(home_touch[, in5perc], away_touch[, in5perc]), in5perc_against = c(away_touch[, in5perc], home_touch[, in5perc]),
                              in5perc_xG = c(home_touch[, in5perc_xG], away_touch[, in5perc_xG]), in5perc_xG_against = c(away_touch[, in5perc_xG], home_touch[, in5perc_xG]),
                              totalPass = passes, totalPass_against = rev(passes),
                              longballs = longballs, longballs_against = rev(longballs)
  )
  
}
matchData = rbindlist(matchData)
matchData[, in5perc_w := in5perc / ((longballs / totalPass)*10)]
matchData[, in5perc_w := ifelse(in5perc_w > 50, 50, in5perc_w)]

matchData[, in5perc_against_w := in5perc_against / ((longballs_against / totalPass_against)*10)]
matchData[, in5perc_against_w := ifelse(in5perc_against_w > 50, 50, in5perc_against_w)]

matchDataAvg= matchData[, .(sib = mean(sib), sib_against = mean(sib_against),
                            in1perc = mean(in1perc), in1perc_against = mean(in1perc_against),
                            in5percW = mean(in5perc_w), in5percW_against = mean(in5perc_against_w),
                            touch_xG = mean(in5perc_xG / in5perc), touch_xG_against = mean(in5perc_xG_against / in5perc_against)),
                        Team]

trendData = list()
k = 0
for(t in matchData[, unique(Team)]){
  k = k+1
  trendData[[k]] = data.table(Team = t,
                              sibTrend = lm(sib ~ x, data = matchData[Team == t, .(x = 1:.N, sib)])$coef[2],
                              in5perc_wTrend = lm(in5perc_w ~ x, data = matchData[Team == t, .(x = 1:.N, in5perc_w)])$coef[2],
                              sib_againstTrend = lm(sib_against ~ x, data = matchData[Team == t, .(x = 1:.N, sib_against)])$coef[2],
                              in5perc_against_wTrend = lm(in5perc_against_w ~ x, data = matchData[Team == t, .(x = 1:.N, in5perc_against_w)])$coef[2],
                              sibDiff = lm(V2 ~ x, data = matchData[Team == t, .(x = 1:.N, sib-sib_against)])$coef[2],
                              in5Diff = lm(V2 ~ x, data = matchData[Team == t, .(x = 1:.N, in5perc_w-in5perc_against_w)])$coef[2])
}
trendData = rbindlist(trendData)
matchDataAvg = merge(matchDataAvg, trendData, by = "Team")
matchDataAvg[, offTrend := rowSums(scale(matchDataAvg[, .(sibTrend, in5perc_wTrend)]))]
matchDataAvg[, defTrend := rowSums(scale(matchDataAvg[, .(sib_againstTrend, in5perc_against_wTrend)]))]
matchDataAvg[, genTrend := rowSums(scale(matchDataAvg[, .(sibDiff, in5Diff)]))]

ggplot(matchDataAvg, aes(x = in5percW, y = sib)) +
  geom_hline(yintercept = matchDataAvg[, mean(sib)]) +
  geom_vline(xintercept = matchDataAvg[, mean(in5percW)]) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = Team, fill = offTrend), size = 3) +
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0, limits = c(-4.1, 4))

# Defensive skill
ggplot(matchDataAvg, aes(x = in5percW_against, y = sib_against)) +
  geom_hline(yintercept = matchDataAvg[, mean(sib_against)]) +
  geom_vline(xintercept = matchDataAvg[, mean(in5percW_against)]) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = Team, fill = defTrend), size = 3) +
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0, limits = c(-3.2, 3.2))

# difference
ggplot(matchDataAvg, aes(x = in5percW - in5percW_against, y = sib - sib_against)) +
  geom_hline(yintercept = matchDataAvg[, mean(sib - sib_against)]) +
  geom_vline(xintercept = matchDataAvg[, mean(in5percW - in5percW_against)]) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = Team, fill = genTrend), size = 3, alpha = .75) +
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0, limits = c(-4.7, 4.7))

matchDataAvg[, .(Team, sib_diff = sib - sib_against, in5perc_diff = in5percW - in5percW_against)]
teamRank = data.table(matchDataAvg[, Team],
           skill = rowSums(scale(matchDataAvg[, .(sib_diff = sib - sib_against, in5perc_diff = in5percW - in5percW_against)])))[order(skill)]
teamRank[, Rank := 16:1]
# May, of course, be heavily affected by schedule and plain randomness


trendTS = melt(matchData[, .(Game = 1:.N, Opponent, sib, sib_against, sib_diff = sib - sib_against, in5perc_w, in5perc_against_w, in5perc_w_diff = in5perc_w - in5perc_against_w), Team],
               id.vars = 1:3)
trendTS = merge(trendTS, teamRank[, .(Opponent = V1, Rank)], by = "Opponent")[order(Team, Game)]
trendTS[, mean(value), variable]


ggplot(trendTS[Team %in% c("IF Elfsborg", "BK Häcken")], aes(x = Game, y = value, color = Rank)) +
  geom_line(color = "black") +
  geom_point(size = 2) + 
  geom_smooth(se = FALSE, formula = y ~ poly(x, 2), method = "lm") +
  facet_wrap(Team~variable, scales = "free_y", nrow = 2) +
  geom_hline(data = trendTS[, mean(value), variable], aes(yintercept = V1), color = "red3") +
  theme(panel.background = element_rect(fill = "white")) +
  scale_color_viridis(direction = -1)



 ## Player positioning analysis
goalKeepers <- as.character()
for(i in 1:length(Allsv21)){
  homeG = which(Allsv21[[i]]$home$players$position == "G")
  goalKeepers <- c(goalKeepers, Allsv21[[i]]$home$players$player$name[homeG])
  awayG = which(Allsv21[[i]]$away$players$position == "G")
  goalKeepers <- c(goalKeepers, Allsv21[[i]]$away$players$player$name[awayG])
}
goalKeepers <- unique(goalKeepers)

hmTablePos = hmTable[Player %in% goalKeepers == FALSE]

# Create grid 
hmTablePos[, Grid := ifelse(in5perc == 1 & in1perc == 1, 1, 0)]
hmTablePos[, Grid := ifelse(in5perc == 0 & in1perc == 1, 2, Grid)]
hmTablePos[x2 <= 35 & y2 <= 17, Grid := 3]
hmTablePos[x2 <= 35 & y2 > 17 & y2 <= 51, Grid := 4]
hmTablePos[x2 <= 35 & y2 > 51, Grid := 5]

hmTablePos[x2 > 35 & Grid == 0 & y2 > 27.2 & y2 <= 40.8, Grid := 6]
hmTablePos[x2 > 35 & Grid == 0 & y2 > 13.6 & y2 <= 27.2, Grid := 7]
hmTablePos[x2 > 35 & Grid == 0 & y2 > 40.8 & y2 <= 54.4, Grid := 8]
hmTablePos[x2 > 35 & Grid == 0 & y2 <= 13.6, Grid := 9]
hmTablePos[x2 > 35 & Grid == 0 & y2 > 54.4, Grid := 10]

hmTablePos[Grid == 2 & y2 > 17 & y2 <= 51, Grid := 11]
hmTablePos[Grid == 2 & y2 <= 17, Grid := 12]
hmTablePos[Grid == 2 & y2 > 51, Grid := 13]

hmTablePos[x2 > 100 & y2 > 20 & y2 < 48, Grid := 1]

hmTablePos[Min == 90, .N, .(Player, Team, Opponent, Grid)][, mean(N), .(Team, Grid)][order(V1)]


ggplot(hmTablePos[1:8000], aes(x = x2, y = y2, color = as.character(Grid))) +
  geom_point(size = 3) +
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68)) +
  scale_color_viridis(discrete = TRUE, option = "B")


hmTablePos[Grid == 1, .(x2 = min(x2)), .(y2 = round(y2, 1))][order(y2)][, paste(y2, collapse = ", ")]

gridData = data.table(Grid = c(rep(c(3,4,5), each = 4), rep(9, 23), rep(10, 22), rep(7, 22), rep(8, 22), rep(6, 4), rep(12, 28), rep(13, 26), rep(11, 100), rep(1, 57)),
                      x = c(rep(c(0, 0, 35, 35), 3),
                            35.1, 35.1, 103.95, 93.45, 92.4, 90.3, 88.2, 87.15, 86.1, 84, 82.95, 81.9, 81.9, 80.85, 79.8, 78.75, 78.75, 77.7, 76.65, 76.65, 75.6, 75.6, 74.55,
                            35.1, 35.1, 75.6, 75.6, 76.65, 76.65, 77.7, 78.75, 78.75, 79.8, 80.85, 81.9, 81.9, 82.95, 84, 86.1, 87.15, 88.2, 90.3, 92.4, 95.65, 99.75,
                            35.1, 35.1, 74.55, 73.5, 73.2, 72.45, 72.25, 72.05, 71.4, 71.2, 71, 70.35, 70.25, 70.15, 70.05, 69.3, 69.2, 69.1, 69, 68.9, 68.25, 68.05,
                            35.1, 35.1, 68.25, 69.3, 69.3, 69.3, 69.3, 69.3, 70.35, 70.35, 70.35, 70.35, 71.4, 71.4, 71.4, 72.45, 72.45, 72.45, 73.5, 73.5, 74.55, 74.55,
                            35.1, 35.1, 68.25, 68.25,
                            100.8, 95.55, 93.45, 91.35, 89.25, 88.2, 87.15, 85.05, 84, 82.95, 82.95, 81.9, 80.85, 79.8, 79.8, 78.75, 77.7, 77.7, 76.65, 76.65, 75.6, 75.6, 74.55, 74.55, 73.5, 73.5, 105, 105,
                            73.5, 74.55, 74.55, 75.6, 75.6, 76.65, 76.65, 77.7, 77.7, 78.75, 79.8, 79.8, 80.85, 81.9, 82.95, 82.95, 84, 85.05, 87.15, 88.2, 89.25, 91.35, 93.45, 103.95, 105, 105,
                            73.5, 72.45, 72.45, 72.45, 71.4, 71.4, 71.4, 71.4, 70.35, 70.35, 70.35, 70.35, 70.35, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3,
                            69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 69.3, 70.35, 70.35, 70.35, 70.35, 70.35, 71.4, 71.4, 71.4, 71.4, 72.45, 72.45, 72.45, 73.5, 73.5,
                            93.45, 91.35, 91.35, 90.3, 89.25, 89.25, 88.2, 86.1, 87.15, 85.05, 85.05, 85.05, 85.05, 85.05, 85.05, 84, 84, 84, 84, 82.95, 82.95, 82.95, 82.95, 82.95, 82.95, 82.95,
                            81.9, 82.95, 82.95, 81.9, 82.95, 82.95, 84, 84, 84, 84, 85.05, 85.05, 85.05, 86.1, 86.1, 86.1, 87.15, 87.15, 88.2, 89.25, 89.25, 90.3, 89.25, 92.4,
                            99.75, 96.6, 94.5, 93.45, 92.4, 91.35, 90.3, 90.3, 89.25, 88.2, 87.7, 87.15, 87, 86.8, 86.1, 85.9, 85.7, 85.05, 85.05, 85.05, 85.05, 84, 84, 84, 84, 84, 84, 84,
                            84, 84, 84, 84, 84, 84, 84.5, 85, 85.2, 85.5, 85.8, 86.1, 86.4, 86.7, 87, 87.3, 87.7, 88.2, 89.25, 90.3, 90.3, 91.35, 92.4, 94.5, 94.5, 96.6, 99.75, 105, 105),
                      y = c(17, 0, 0, 17, 51, 17, 17, 51, 68, 51, 51, 68,
                            13.6, 0, 0, 0.7, 1.4, 2, 2.7, 3.4, 4.1, 4.8, 5.4, 6.1, 6.8, 7.5, 8.2, 8.8, 9.5, 10.2, 10.9, 11.6, 12.2, 12.9, 13.6,
                            68, 54.4, 54.4, 55.8, 56.4, 57.1, 57.8, 58.5, 59.2, 59.8, 60.5, 61.2, 61.9, 62.6, 63.2, 63.9, 64.6, 65.3, 66, 66.6, 67.3, 68,
                            27.2, 13.6, 13.6, 15, 15.6, 16.3, 17, 17.7, 18.4, 19, 19.7, 20.4, 21.1, 21.8, 22.4, 23.1, 23.8, 24.5, 25.2, 25.8, 26.5, 27.2,
                            54.4, 40.8, 40.8, 42.2, 42.8, 43.5, 44.2, 44.9, 45.6, 46.2, 46.9, 47.6, 48.3, 49, 49.6, 50.3, 51, 51.7, 52.4, 53, 53.7, 54.4,
                            40.8, 27.2, 27.2, 40.8,
                            0, 0.7, 1.4, 2, 2.7, 3.4, 4.1, 4.8, 5.4, 6.1, 6.8, 7.5, 8.2, 8.8, 9.5, 10.2, 10.9, 11.6, 12.2, 12.9, 13.6, 14.3, 15, 15.6, 16.3, 17, 17, 0,
                            51, 52.4, 53, 53.7, 54.4, 55.1, 55.8, 56.4, 57.1, 57.8, 58.5, 59.2, 59.8, 60.5, 61.2, 61.9, 62.6, 63.2, 63.9, 64.6, 65.3, 66, 66.6, 67.3, 68, 51,
                            17, 18.4, 19, 19.7, 20.4, 21.1, 21.8, 22.4, 23.1, 23.8, 24.5, 25.2, 25.8, 26.5, 27.2, 27.9, 28.6, 29.2, 29.9, 30.6, 31.3, 32, 32.6, 33.3, 34, 34.7, 35.4,
                            36, 36.7, 37.4, 38.1, 38.8, 39.4, 40.1, 40.8, 41.5, 42.2, 42.8, 43.5, 44.2, 44.9, 45.6, 46.2, 46.9, 47.6, 48.3, 49, 49.6, 50.3, 51,
                            51, 50.3, 49.6, 49, 48.3, 47.6, 46.9, 46.2, 45.6, 44.9, 44.2, 43.5, 42.8, 42.2, 41.5, 40.8, 40.1, 39.4, 38.8, 38.1, 37.4, 36.7, 36, 35.4, 34.7, 34, 33.3, 32.6,
                            32, 31.3, 30.6, 29.9, 29.2, 28.6, 27.9, 27.2, 26.5, 25.8, 25.2, 24.5, 23.8, 23.1, 22.4, 21.8, 21.1, 20.4, 19.7, 19, 18.4, 17,
                            15.6, 16.3, 17, 17.7, 18.4, 19, 19.7, 20.4, 21.1, 21.8, 22.4, 23.1, 23.8, 24.5, 25.2, 25.8, 26.5, 27.2, 27.9, 28.6, 29.2, 29.9, 30.6, 31.3, 32, 32.6, 33.3, 34,
                            34.7, 35.4, 36, 36.7, 37.4, 38.1, 38.8, 39.4, 40.1, 40.8, 41.5, 42.2, 42.8, 43.5, 44.2, 44.9, 45.6, 46.2, 46.9, 47.6, 48.3, 49, 49.6, 50.3, 51, 51.7, 52.4, 52.4, 15.6))

ggplot(data = gridData) +
  geom_polygon(aes(x = x, y = y, fill = Grid, group = Grid), color = "white") + 
  scale_fill_viridis() + theme(panel.background = element_rect(fill = "black"), panel.grid = element_blank())

avgTouchGrid = hmTablePos[, .N, .(Team, Opponent = paste(Opponent, Game), Grid)][, .(avgTouches = mean(N)), Grid][order(Grid)]
gridData = merge(gridData, avgTouchGrid, by = "Grid")

ggplot(data = gridData) +
  geom_polygon(aes(x = x, y = y, fill = avgTouches, group = Grid), color = "white") + 
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0) +
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68))


avgTouchPeking =  hmTablePos[Team == "IFK Norrköping", .N, .(Team, Opponent, Grid)][, .(avgTouches = mean(N)), Grid][order(Grid)]
avgTouchPeking[, TouchDiff := avgTouches - avgTouchGrid[, avgTouches]]
gridDataPeking = merge(gridData, avgTouchPeking[, .(Grid, TouchDiff)], by = "Grid")

ggplot(data = gridDataPeking) +
  geom_polygon(aes(x = x, y = y, fill = TouchDiff, group = Grid), color = "white") + 
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0) +
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68))
  

avgTouchTeam =  hmTablePos[, .N, .(Team, Opponent = paste(Opponent, Game), Grid)][, .(avgTouches = mean(N)), .(Grid, Team)][order(Team, Grid)]
avgTouchTeam[, TouchDiff := avgTouches - avgTouchGrid[, avgTouches]]
gridDataTeam = merge(gridData, avgTouchTeam[, .(Team, Grid, TouchDiff)], by = "Grid", allow.cartesian = TRUE)
gridDataTeam[, quantile(TouchDiff, probs = c(0.05, 0.95))]

ggplot(data = gridDataTeam) +
  geom_polygon(aes(x = x, y = y, fill = TouchDiff, group = Grid), color = "white", alpha = .8) + 
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0, limits = c(-14.52163  , 13.51683 ), oob = scales::squish) +
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68)) +
  facet_wrap(~Team)

gameOrder = hmTablePos[Team == "IFK Norrköping", unique(paste(Opponent, Game))]
touchGameTeam = hmTablePos[Team == "IFK Norrköping", .(Touches = .N), .(Team, Opponent = paste(Opponent, Game), Grid)][order(Opponent, Grid)]
touchGameTeam[, TouchDiff := Touches - avgTouchGrid[, avgTouches]]
touchGameTeam = merge(gridData, touchGameTeam[, .(Opponent, Grid, TouchDiff)], by = "Grid", allow.cartesian = TRUE)
touchGameTeam[, Opponent := factor(Opponent, levels = gameOrder)]
touchGameTeam[, quantile(TouchDiff, probs = c(0.05, 0.95))]

ggplot(data = touchGameTeam) +
  geom_polygon(aes(x = x, y = y, fill = TouchDiff, group = Grid), color = "white", alpha = .8) + 
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0, limits = c(-16, 20), oob = scales::squish) +
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68)) +
  facet_wrap(~Opponent)


ggplot(hmTablePos[grepl(pattern = "Khazeni|Carl Björk|Adegbenro", x = Player)]) +
  geom_point(aes(x = x2, y = y2, color = Player, size = xG)) + 
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68)) +
  facet_wrap(~Opponent)
  
distToGoal = merge(hmTablePos[, .(distCenter = mean(distCenter), angleGoal = mean(angleGoal), Touches = .N), Player],
                   hmTablePos[, .(Min = max(Min)), .(Player, Team, Opponent)][, .(Min = sum(Min)), .(Player, Team)],
                   by = "Player")[order(distCenter)][Min >= 180]

playerLabels = rbindlist(list(distToGoal[distCenter < quantile(distCenter, 0.05) | angleGoal > quantile(angleGoal, 0.95) |
                                           (distCenter < quantile(distCenter, 0.1) & angleGoal < quantile(angleGoal, 0.9)) |
                                           (distCenter < quantile(distCenter, 0.8) & angleGoal > quantile(angleGoal, 0.9)  & distCenter > 45)],
                              distToGoal[distCenter > 50 & distCenter < 60][angleGoal > quantile(angleGoal, 0.95)],
                              distToGoal[distCenter > 50 & distCenter < 60][angleGoal < quantile(angleGoal, 0.05)])
                     )

distToGoal[, .(distCenter / angleGoal, distCenter, Player)][order(V1)][V1  < quantile(V1, 0.1)]

lm_distAngle = lm(angleGoal ~ distCenter, distToGoal[distCenter < 60])

playerLabels = distToGoal[c(which(lm_distAngle$residuals > quantile(lm_distAngle$residuals, 0.8)),
                            which(lm_distAngle$residuals < quantile(lm_distAngle$residuals, 0.1)))]



library(ggrepel)
ggplot(distToGoal, aes(x = distCenter, y = angleGoal, size = Touches)) +
  geom_point() +
  scale_x_reverse() +
  geom_vline(xintercept = distToGoal[, mean(distCenter)]) +
  geom_hline(yintercept = distToGoal[, mean(angleGoal)]) +
  scale_size(range = c(0.5, 4)) +
  geom_label_repel(data = playerLabels, aes(label = Player), size = 2)



ggplot(hmTablePos[grepl(pattern = "Carl Björk|Adegbenro|Nyman", x = Player)]) +
  geom_point(aes(x = x2, y = y2), color = "darkorange") + 
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68)) +
  facet_wrap(Player~Game > 350)

hmTablePos[grepl(pattern = "Sead|Levi|Adegbenro|Sema|Carl Björk|Bergmann J|Khazeni|Nyman", x = Player), .(Min = max(Min), in5perc = sum(in5perc)), .(Player, Opponent)][,
  .(Min = sum(Min), in5perc = sum(in5perc), in5perc90 = sum(in5perc) / (sum(Min) / 90)), Player]

hmTablePos[, .(Min = max(Min), in5perc = sum(in5perc)),
           .(Player, Opponent)][,.(Min = sum(Min), in5perc = sum(in5perc), in5perc90 = sum(in5perc) / (sum(Min) / 90)), Player][
                                  Min > 180][order(-in5perc90)][1:30]

ggplot(hmTablePos[grepl(pattern = "Sachpekidis|Adi N|Chilufya", x = Player)]) +
  geom_point(aes(x = x2, y = y2), color = "darkorange") + 
  sf_theme +   
  annotate("rect", xmin = 0, xmax = 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - (100 / 68) * 17.7, ymax = 34 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - (100 / 68) * 6.7, ymax = 34 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("line", x = c(52.5, 52.5), y = c(0, 68)) +
  facet_wrap(~Player)




