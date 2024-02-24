library(dplyr)

gameData2122 = gameData2122[order(Team, Date)]

gameData2122[, shotsIbMean := shift(cummean(shotsIB)), Team]
gameData2122[, oppShotsIbMean := shift(cummean(oppShotsIB)), Team]

gameData2122[, in5perc_wMean := shift(cummean(in5perc_w)), Team]
gameData2122[, oppIn5perc_wMean := shift(cummean(oppIn5perc_w)), Team]

gameData2122[, in1percMean := shift(cummean(in1perc)), Team]
gameData2122[, oppIn1percMean := shift(cummean(oppIn1perc)), Team]


# Games after 2021-05-17 
trainData21 = list()
trainData = gameData2122[Date > "2021-05-18"]
for(i in 1:trainData[, .N]){
  i_team = trainData[i, Team]
  i_date = trainData[i, Date]
  game = trainData[i, Game]
  opp_date = trainData[Team != i_team & Date == i_date & Game == game, .(Team, Date)]
  # Average shots, touches opponent allows
  oppStats = trainData[Team == opp_date[, Team] & Date == opp_date[, Date], .(oppShotsIbMean, oppIn5perc_wMean, oppIn1percMean, Goals)]
  
  # use these values to calculate if a team
  # takes more/less than opponent allows on average
  # allows more/less than opponent takes on average
  tempDat = gameData2122[Date < i_date, .(Team, Opponent, Date, h_a, shotsIB, oppShotsIB, 
                                       in5perc_w, oppIn5perc_w, in1perc, oppIn1perc,
                                       shotsIbMean, oppShotsIbMean, in5perc_wMean, oppIn5perc_wMean, in1percMean, oppIn1percMean)]
  tempDat = na.omit(tempDat)
  # Defensive stats opponent
  # avgAgainst = avgAgainst =tempDat[, .(avgOppShIb = mean(oppShotsIB), avgOppIn5perc_w = mean(oppIn5perc_w), avgOppIn1perc = mean(oppIn1perc)), .(Opponent = Team)]
  avgAgainst = tempDat[, .(avgOppShIb = tail(oppShotsIbMean, 1), avgOppIn5perc_w = tail(oppIn5perc_wMean, 1), avgOppIn1perc = tail(oppIn1percMean, 1)), .(Opponent = Team)]
  tempDat = merge(tempDat, avgAgainst, by = "Opponent", sort = FALSE)
  
  # Offensive stats opponent
  # avgFor = tempDat[, .(avgShIb = mean(shotsIB), avgIn5perc_w = mean(in5perc_w), avgIn1perc = mean(in1perc)), .(Opponent = Team)] # avg shots
  avgFor = tempDat[, .(avgShIb = tail(shotsIbMean, 1), avgIn5perc_w = tail(in5perc_wMean, 1), avgIn1perc = tail(in1percMean, 1)), .(Opponent = Team)]
  
  tempDat = merge(tempDat, avgFor, by = "Opponent", sort = FALSE)
  
  # Game offensive output minus opponent defensive average - Home team attacking skill
  offFeature = tempDat[Team == i_team, .(shotsIB = mean(shotsIB - avgOppShIb), In5perc_w = mean(in5perc_w - avgOppIn5perc_w), in1perc = mean(in1perc - avgOppIn1perc))]
  # Game defensive output minus opponent offensive average - Away team defensive skill
  defFeature = tempDat[Team == opp_date[, Team], .(shotsIB = mean(oppShotsIB - avgShIb), In5perc_w = mean(oppIn5perc_w - avgIn5perc_w), in1perc = mean(oppIn1perc - avgIn1perc))] 
  
  # Then put team, opponent, home/away, avg shots for, opponent avg shots against in a table
  trainData21[[i]] = 
    data.table(Team = i_team, Opponent = opp_date[, Team], h_a = trainData[i, h_a], Goals = trainData[i, Goals], GoalsAgainst = oppStats[, Goals], Sign = trainData[i, Sign], 
               shotsIB = trainData[i, shotsIB], oppShotsIB = trainData[i, oppShotsIB], 
               in5perc_w = trainData[i, in5perc_w], oppIn5perc_w = trainData[i, oppIn5perc_w], 
               shotsIbMean = trainData[i, shotsIbMean], in5perc_wMean = trainData[i, in5perc_wMean], in1percMean = trainData[i, in1percMean],
               oppShotsIbMean = oppStats[, oppShotsIbMean],
               oppIn5perc_wMean = oppStats[, oppIn5perc_wMean],  oppIn1percMean = oppStats[,  oppIn1percMean],
               offSkillShots = offFeature[, shotsIB], offSkillIn5perc = offFeature[, In5perc_w], offSkillIn1perc = offFeature[, in1perc],
               oppSkillShots = defFeature[, shotsIB], oppSkillIn5perc = defFeature[, In5perc_w], oppSkillIn1perc = defFeature[, in1perc])
  
  if((i %% 100) == 0) print(i)
}

trainData21 = rbindlist(trainData21)
trainData21 = na.omit(trainData21)

trainData21 = trainData21[Team %in% c("GIF Sundsvall", "IFK Värnamo", "Helsingborgs IF") == FALSE & 
                            Opponent %in% c("GIF Sundsvall", "IFK Värnamo", "Helsingborgs IF") == FALSE ]

ggplot(trainData21, aes(y = oppSkillIn5perc, x = as.factor(Goals))) + 
  geom_boxplot()


trainData21[h_a == 2, h_a := 0]
bayesData <- list(X= as.matrix(na.omit(trainData21[, .(h_a, shotsIbMean, in5perc_wMean, oppShotsIbMean, oppIn5perc_wMean,
                                                       offSkillShots, offSkillIn5perc, oppSkillShots, oppSkillIn5perc)])),  # predictors
                  Goals=na.omit(trainData21)[, Goals],  
                  N=na.omit(trainData21)[, .N])  # sample size

modelString = "model{
    ## Likelihood
    for(i in 1:N){
      Goals[i] ~ dpois(lambda[i])
      log(lambda[i]) <- mu[i]
      mu[i] <- b1 * X[i, 1] + b2 * X[i, 2] + b3 * X[i, 3] + b4 * X[i, 4] + b5 * X[i, 5] + b6 * X[i, 6] + b7 * X[i, 7] + b8 * X[i, 8] + b9 * X[i, 9]
    } 
    
    ## Priors
    b1 ~ dnorm(0, 0.0625)
    b2 ~ dnorm(0, 0.0625)
    b3 ~ dnorm(0, 0.0625)
    b4 ~ dnorm(0, 0.0625)
    b5 ~ dnorm(0, 0.0625)
    b6 ~ dnorm(0, 0.0625)
    b7 ~ dnorm(0, 0.0625)
    b8 ~ dnorm(0, 0.0625)
    b9 ~ dnorm(0, 0.0625)
}"

library(rjags)
jagsmodel <- jags.model(file = textConnection(modelString), data=bayesData, n.chains=5, n.adapt=60000)
## Burning some samples
update(jagsmodel, 45000)
## Generate MCMC samples
s1 <- coda.samples(jagsmodel, variable.names=c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9"), n.iter=70000, thin=5)
## Merge the MCMC chains into one matrix
ms1 <- as.matrix(s1)
ms1df <- data.frame(ms1)

exp(colMeans(ms1df))

ms1dt = data.table(ms1df)
### Save this table so I don't have to re-train the model every time
# Suppose it is fair to assume model parameters not should change that much from round to round
save(ms1dt, file = "allsv_bayesModel2_posterior.rda")

load("allsv_bayesModel2_posterior.rda")

ggplot(melt(ms1dt[, .(id = 1:.N, h_a = b1, shotsIbMean = b2, in5perc_wMean = b3, oppShotsIbMean = b4, oppIn5perc_wMean = b5,
                      offSkillShots = b6, offSkillIn5perc = b7, oppSkillShots = b8, oppSkillIn5perc = b9)],
            id.vars = 1), aes(x = exp(value))) + geom_histogram(binwidth = 0.01) +
  facet_wrap(~variable, scales = "free")

ggplot(melt(ms1dt[, .(id = 1:.N, h_a = b1, shotsIbMean = b2, in5perc_wMean = b3, oppShotsIbMean = b4, oppIn5perc_wMean = b5,
                      offSkillShots = b6, offSkillIn5perc = b7, oppSkillShots = b8, oppSkillIn5perc = b9)],
            id.vars = 1), aes(x = id, y = exp(value))) + geom_line() +
  facet_wrap(~variable, scales = "free")

# Feature data
# use these values to calculate if a team
# takes more/less than opponent allows on average
# allows more/less than opponent takes on average
trainData[h_a == 2, h_a := 0]


# Select data from latest x games when creating features

featurePrep = trainData[Date > "2021-08-09", .(Team, Opponent, Date, h_a, shotsIB, oppShotsIB, 
                            in5perc_w, oppIn5perc_w, in1perc, oppIn1perc,
                            shotsIbMean, oppShotsIbMean, in5perc_wMean, oppIn5perc_wMean, in1percMean, oppIn1percMean)]
featurePrep = na.omit(featurePrep)
# Defensive stats opponent
# avgAgainst = featurePrep[, .(avgOppShIb = mean(oppShotsIB), avgOppIn5perc_w = mean(oppIn5perc_w), avgOppIn1perc = mean(oppIn1perc)), .(Opponent = Team)]
avgAgainst = featurePrep[, .(avgOppShIb = tail(oppShotsIbMean, 1), avgOppIn5perc_w = tail(oppIn5perc_wMean, 1), avgOppIn1perc = tail(oppIn1percMean, 1)), .(Opponent = Team)]

featurePrep = merge(featurePrep, avgAgainst, by = "Opponent", sort = FALSE)

# Offensive stats opponent
# avgFor = featurePrep[, .(avgShIb = mean(shotsIB), avgIn5perc_w = mean(in5perc_w), avgIn1perc = mean(in1perc)), .(Opponent = Team)] # avg shots
avgFor = featurePrep[, .(avgShIb = tail(shotsIbMean, 1), avgIn5perc_w = tail(in5perc_wMean, 1), avgIn1perc = tail(in1percMean, 1)), .(Opponent = Team)]

featurePrep = merge(featurePrep, avgFor, by = "Opponent", sort = FALSE)

genFeatureData = function(team, opponent, H_A){
  teamStats = tail(featurePrep[Team == team], 1)
  # Average shots, touches opponent allows
  oppStats = tail(featurePrep[Team == opponent, .(oppShotsIbMean, oppIn5perc_wMean, oppIn1percMean)], 1)
  
  # Game offensive output minus opponent defensive average - Home team attacking skill (> 0 = Good)
  offFeature = featurePrep[Team == team, .(shotsIB = mean(shotsIB - avgOppShIb), In5perc_w = mean(in5perc_w - avgOppIn5perc_w), in1perc = mean(in1perc - avgOppIn1perc))]
  # Game defensive output minus opponent offensive average - Away team defensive skill (< 0 = Good)
  defFeature = featurePrep[Team == opponent, .(shotsIB = mean(oppShotsIB - avgShIb), In5perc_w = mean(oppIn5perc_w - avgIn5perc_w), in1perc = mean(oppIn1perc - avgIn1perc))] 
  
  # Then put team, opponent, home/away, avg shots for, opponent avg shots against in a table
  return(data.table(Team = team, Opponent = opponent, h_a = H_A, 
                    shotsIbMean = teamStats[, shotsIbMean], in5perc_wMean = teamStats[, in5perc_wMean], 
                    oppShotsIbMean = oppStats[, oppShotsIbMean], oppIn5perc_wMean = oppStats[, oppIn5perc_wMean],  
                    offSkillShots = offFeature[, shotsIB], offSkillIn5perc = offFeature[, In5perc_w], 
                    oppSkillShots = defFeature[, shotsIB], oppSkillIn5perc = defFeature[, In5perc_w]))
    
}

next_rounds = data.frame(slug = as.character(), customId = as.numeric(), ID = as.numeric(), Date = as.character(), 
                         Status = as.character(), homeTeam = as.character(), awayTeam = as.character())
Date = seq(as.Date("2022-08-13"), as.Date("2022-08-22"), 1)
for(i in as.character(Date)){
  x <- fromJSON(paste("https://api.sofascore.com/api/v1/sport/football/scheduled-events/", i, sep = ""))
  
  tournAllsv = which(x$events$tournament$id == 24)
  
  if(length(tournAllsv) > 0){
    for(g in 1:length(tournAllsv)){
      next_rounds = rbindlist(list(next_rounds,
                                   data.table(slug = x$events$slug[tournAllsv[g]],
                                              customId = x$events$customId[tournAllsv[g]],
                                              ID = x$events$id[tournAllsv[g]],
                                              Date = i,
                                              Status = x$events$status$type[tournAllsv[g]],
                                              x$events$homeTeam$name[tournAllsv[g]],
                                              x$events$awayTeam$name[tournAllsv[g]])))
    }
  }
  print(i)
}
next_rounds = next_rounds[Status != "canceled"]
next_rounds = next_rounds[-which(duplicated(next_rounds$ID))]


featureData = list()
k = 0
for(i in 1:next_rounds[, .N]){
  k = k+1
  featureData[[k]] = genFeatureData(next_rounds[i, homeTeam], next_rounds[i, awayTeam], 1)
  k = k+1
  featureData[[k]] = genFeatureData(next_rounds[i, awayTeam], next_rounds[i, homeTeam], 0)
  }

featureData = rbindlist(featureData)

predict_func = function(homeData, awayData, homeExtra = 0){
  # home
  homeData = data.frame(homeData)
  pred = exp(
               homeData[, 3] * ms1dt[ , b1 + homeExtra] +
               homeData[, 4] * ms1dt[ , b2] +
               homeData[, 5] * ms1dt[ , b3] +
               homeData[, 6] * ms1dt[ , b4] + 
               homeData[, 7] * ms1dt[ , b5] + 
               homeData[, 8] * ms1dt[ , b6] + 
               homeData[, 9] * ms1dt[ , b7] + 
               homeData[, 10] * ms1dt[ , b8] + 
               homeData[, 11] * ms1dt[ , b9])
  home <- rpois(length(pred) , pred)
  # away
  awayData = data.frame(awayData)
  pred = exp(
               awayData[, 3] * ms1dt[ , b1] +
               awayData[, 4] * ms1dt[ , b2] +
               awayData[, 5] * ms1dt[ , b3] +
               awayData[, 6] * ms1dt[ , b4] + 
               awayData[, 7] * ms1dt[ , b5] + 
               awayData[, 8] * ms1dt[ , b6] + 
               awayData[, 9] * ms1dt[ , b7] + 
               awayData[, 10] * ms1dt[ , b8] + 
               awayData[, 11] * ms1dt[ , b9])
  away <- rpois(length(pred) , pred)
  
  # print(ggplot(data.table(Goals = c(home, away), Team = rep(c("Home", "Away"), each = length(home))), aes(x = Goals, fill = Team)) +
  #         geom_histogram(binwidth = 1, position = "identity", alpha = .5) + labs(title = paste(homeData[, 1], awayData[, 1], sep = "- ")))
  #print(paste("p1:", round(mean(home > away), 3), "pX:", round(mean(home == away), 3), "p2:", round(mean(home < away), 3)))
  #print(paste("p1:", round(1 / mean(home > away), 3), "pX:", round(1 / mean(home == away), 3), "p2:", round(1 / mean(home < away), 3)))
  
  return( data.table(homeTeam = homeData[, 1], awayTeam = awayData[, 1],
                     p1 = round(mean(home > away), 3), px = round(mean(home == away), 3), p2 = round(mean(home < away), 3),
                     o1 = round(1 / mean(home > away), 3), ox = round(1 / mean(home == away), 3), o2 = round(1 / mean(home < away), 3),
                     p2_5 = round(mean((home + away) > 2.5), 3), o2_5 = round(1 / mean((home + away) > 2.5), 3)))
}


predictions = list()
k = 1
for(i in 1:next_rounds[, .N]){
  predictions[[i]] = predict_func(featureData[k, ], featureData[(k+1), ], homeExtra = 0)
  k = k+2
}

predictions = rbindlist(predictions)

library(DT)

datatable(predictions[1:8, ], options = list(paging = FALSE, searching = FALSE, info = FALSE,
                                      initComplete = JS(
                                        "function(settings, json) {",
                                        "$(this.api().table().header()).css({'background-color': '#c9dde7', 'color': '#353e42'});",
                                        "}")),  rownames = FALSE) %>%
  formatStyle("homeTeam",  color = '#353e42', backgroundColor = '#e8f0f5', fontWeight = 'bold') %>%
  formatStyle("awayTeam",  color = '#353e42', backgroundColor = '#eff5f8', fontWeight = 'bold') %>%
  formatStyle(c("p1", "px", "p2"),
              background = styleColorBar(range(c(0, 0.975)), '#deb3cf'), 
              backgroundColor = '#7aabc6',
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center', fontWeight = 'bold') %>%
  formatStyle(names(predictions)[6:10],  color = '#353e42', fontWeight = 'bold')

datatable(predictions[9:16, ], options = list(paging = FALSE, searching = FALSE, info = FALSE,
                                             initComplete = JS(
                                               "function(settings, json) {",
                                               "$(this.api().table().header()).css({'background-color': '#c9dde7', 'color': '#353e42'});",
                                               "}")),  rownames = FALSE) %>%
  formatStyle("homeTeam",  color = '#353e42', backgroundColor = '#e8f0f5', fontWeight = 'bold') %>%
  formatStyle("awayTeam",  color = '#353e42', backgroundColor = '#eff5f8', fontWeight = 'bold') %>%
  formatStyle(c("p1", "px", "p2"),
              background = styleColorBar(range(c(0, 0.975)), '#deb3cf'), 
              backgroundColor = '#7aabc6',
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center', fontWeight = 'bold') %>%
  formatStyle(names(predictions)[6:10],  color = '#353e42', fontWeight = 'bold')

featurePrep[, Game := 1:.N, Team]

FeatureLims = featurePrep[, .(variable = c("OffSkillshots", "OffSkillIn5perc_w", "DefSkillShots", "DefSkillIn5perc_w"),
                              Mean = c(mean(shotsIB - avgOppShIb), mean(in5perc_w - avgOppIn5perc_w),
                                       mean(oppShotsIB - avgShIb), mean(oppIn5perc_w - avgIn5perc_w)),
                              Q05 = c(quantile(shotsIB - avgOppShIb, 0.05), quantile(in5perc_w - avgOppIn5perc_w, 0.05),
                                      quantile(oppShotsIB - avgShIb, 0.05), quantile(oppIn5perc_w - avgIn5perc_w, 0.05)),
                              Q95 = c(quantile(shotsIB - avgOppShIb, 0.95), quantile(in5perc_w - avgOppIn5perc_w, 0.95),
                                      quantile(oppShotsIB - avgShIb, 0.95), quantile(oppIn5perc_w - avgIn5perc_w, 0.95)))]

ggplot(melt(featurePrep[Team %in% predictions[2, c(as.character(homeTeam), as.character(awayTeam))],
                        .(Team, Opponent, Game, h_a, OffSkillshots = shotsIB - avgOppShIb, OffSkillIn5perc_w = in5perc_w - avgOppIn5perc_w,
                          DefSkillShots = oppShotsIB - avgShIb, DefSkillIn5perc_w = oppIn5perc_w - avgIn5perc_w)], id.vars = 1:4),
       aes(x = Game, y = value)) +
  geom_line() + geom_point() +
  facet_wrap(Team~variable, scales = "free_y", ncol = 4) +
  geom_hline(data = FeatureLims, aes(yintercept = Mean)) +
  geom_hline(data = FeatureLims, aes(yintercept = Q05)) +
  geom_hline(data = FeatureLims, aes(yintercept = Q95)) +
  geom_smooth(se = FALSE, method = "lm")



library(cluster)


mround <- function(x,base){ 
  base*round(x/base) 
}

formationPlotter = function(gameNum, homeAway){
  gameNum2 = ifelse(homeAway == "home", (gameNum * 2) - 1, gameNum * 2)
  gameFormation = hmTable[Game == gameNum2 & Player %in% playSel(GameInd = gameNum, ha = homeAway)[Position != "G", Player]]
  playerOrder = merge(gameFormation[, .(median(x), median(y)), Player],
                      playSel(GameInd = gameNum, ha = homeAway)[Position != "G", .(Player, Position = factor(Position, levels = c("D", "M", "F")))],
                      by = "Player")[order(Position, V2)]
  
  gameFormation[, Player := factor(Player, levels = playerOrder[, Player])]
  
  gameFormation = gameFormation[(x == 99  & y == 99 ) == FALSE & (x == 99  & y == 0) == FALSE, .N, .(x = mround(x2, 15), y = mround(y2, 10), Player)][order(Player, -N)][N > 1]
  
  gameFormation[y >= 70, y := 67]
  gameFormation[y == 0, y := 1]
  gameFormation[x == 105, x := 103]
  gameFormation[x == 0, x := 2]
  
  gameFormation = gameFormation[, .(x2 = rep(x, N), y2 = rep(y, N)), .(Player, x, y)]
  
  clustCenters = gameFormation[, kmeans(x = as.matrix(data.table(x, y)), centers = 1)$centers, Player]
  clustCenters[, xy := rep(c("x", "y"), playerOrder[, .N])]
  clustCenters = dcast(clustCenters, Player ~ xy, value.var = "V1")
  # Could do second order clusters to add arrows...
    # But then all players must have at least two unqiue coordiantes
  uniqueCoords = gameFormation[, length(unique(paste(x ,y ))), Player]
  clustCenters_2 = gameFormation[Player %in% uniqueCoords[V1 > 1, Player],
                                 kmeans(x = as.matrix(data.table(x, y)), centers = 2, iter.max = 100, nstart = 5)$centers, Player]
  clustCenters_2[, xy :=  rep(c("x","x","y","y"), length(unique(Player)))]
  clustCenters_2[, Group := rep(c(1,2,1,2), length(unique(Player)))]
  clustCenters_2 = dcast(clustCenters_2, Player ~ xy + Group, value.var = "V1")
  if(uniqueCoords[V1 < 2, .N] > 0){
    clustCenters_2 = rbind(clustCenters_2,
                           data.table(Player = uniqueCoords[V1 < 2, Player], x_1 = NA, x_2 = NA, y_1 = NA, y_2 = NA))
    clustCenters_2 = clustCenters_2[order(Player)]
  }
  clustCenters_2[, c("x","y") := clustCenters[, .(x,y)]]
  # Calculate angle and set the arrow with that angle, but max 10 metres distance
  clustCenters_2[, angle1 := atan2(x - x_1, y - y_1)]
  clustCenters_2[,  x_1 := ifelse(sqrt((x - x_1)^2 + (y - y_1)^2) > 15, x + (15 * cos(angle1)), x_1)]
  clustCenters_2[,  y_1 := ifelse(sqrt((x - x_1)^2 + (y - y_1)^2) > 15, y + (15 * sin(angle1)), y_1)]
  clustCenters_2[, angle2 := atan2(x - x_2, y - y_2)]
  clustCenters_2[,  x_2 := ifelse(sqrt((x - x_2)^2 + (y - y_2)^2) > 15, x + (15 * cos(angle2)), x_2)]
  clustCenters_2[,  y_2 := ifelse(sqrt((x - x_2)^2 + (y - y_2)^2) > 15, y + (15 * sin(angle2)), y_2)]
  
  clustCenters[, Surname := tail(strsplit(as.character(Player), split = " ")[[1]], 1), Player]
  
  print(ggplot(clustCenters, aes(x = x, y = y)) +
          annotate("rect", xmin = 0, xmax = 16.5,
                   ymin = 34 - 17.7, ymax = 34 + 17.7, fill = "transparent", col = "black") +
          annotate("rect", xmin = 105, xmax = 105 - 16.5,
                   ymin = 34 - 17.7, ymax = 34 + 17.7, fill = "transparent", col = "black") +
          annotate("rect", xmin = 0, xmax = 5.5,
                   ymin = 34 - 6.7, ymax = 34 + 6.7, fill = "transparent", col = "black") +
          annotate("rect", xmin = 105, xmax = 105 - 5.5,
                   ymin = 34 - 6.7, ymax = 34 + 6.7, fill = "transparent", col = "black") +
          annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
          annotate("line", x = c(52.5, 52.5), y = c(0, 68)) +
          geom_point(size = 4) + 
          geom_text(aes(label = Surname), size = 2.5, vjust = 1.5, hjust = 1) +
          geom_segment(data = clustCenters_2, aes(x = x, y = y, xend = x_1, yend = y_1), alpha = .25, size = 1, arrow = arrow( length = unit(3, "mm"))) + 
          geom_segment(data = clustCenters_2, aes(x = x, y = y, xend = x_2, yend = y_2), alpha = .25, size = 1, arrow = arrow( length = unit(3, "mm"))) + 
          xlim(c(0, 105)) + ylim(c(0, 68))  + 
          sf_theme +
          labs(title = gameData21[Game == gameNum & h_a == ifelse(homeAway == "home", 1, 2), paste(Team, "vs", Opponent)]))
}

formationPlotter(gameNum = 190, homeAway = "away")

formationPlots = list()
for(i in 1:gameData22[Team == "IFK Norrköping", .N]){
  formationPlots[[i]] = formationPlotter(gameNum = gameData22[Team == "IFK Norrköping", Game][i],
                                         homeAway = ifelse(gameData22[Team == "IFK Norrköping", h_a][i] == 1, "home", "away"))
}

gridExtra::grid.arrange(grobs = formationPlots, nrow = 4)

# team analysis

trainData21 = list()
trainData = gameData2122[Date > "2021-05-18"]
for(i in 1:trainData[, .N]){
  i_team = trainData[i, Team]
  i_date = trainData[i, Date]
  game = trainData[i, Game]
  opp_date = trainData[Team != i_team & Date == i_date & Game == game, .(Team, Date)]
  # Average shots, touches opponent allows
  oppStats = trainData[Team == opp_date[, Team] & Date == opp_date[, Date], .(oppShotsIbMean, oppIn5perc_wMean, oppIn1percMean, Goals)]
  
  # use these values to calculate if a team
  # takes more/less than opponent allows on average
  # allows more/less than opponent takes on average
  tempDat = gameData2122[Date < i_date, .(Team, Opponent, Date, h_a, shotsIB, oppShotsIB, 
                                          in5perc_w, oppIn5perc_w, in1perc, oppIn1perc,
                                          shotsIbMean, oppShotsIbMean, in5perc_wMean, oppIn5perc_wMean, in1percMean, oppIn1percMean)]
  tempDat = na.omit(tempDat)
  # Defensive stats opponent
  # avgAgainst = avgAgainst =tempDat[, .(avgOppShIb = mean(oppShotsIB), avgOppIn5perc_w = mean(oppIn5perc_w), avgOppIn1perc = mean(oppIn1perc)), .(Opponent = Team)]
  avgAgainst = tempDat[, .(avgOppShIb = tail(oppShotsIbMean, 1), avgOppIn5perc_w = tail(oppIn5perc_wMean, 1), avgOppIn1perc = tail(oppIn1percMean, 1)), .(Opponent = Team)]
  tempDat = merge(tempDat, avgAgainst, by = "Opponent", sort = FALSE)
  
  # Offensive stats opponent
  # avgFor = tempDat[, .(avgShIb = mean(shotsIB), avgIn5perc_w = mean(in5perc_w), avgIn1perc = mean(in1perc)), .(Opponent = Team)] # avg shots
  avgFor = tempDat[, .(avgShIb = tail(shotsIbMean, 1), avgIn5perc_w = tail(in5perc_wMean, 1), avgIn1perc = tail(in1percMean, 1)), .(Opponent = Team)]
  
  tempDat = merge(tempDat, avgFor, by = "Opponent", sort = FALSE)
  
  # Game offensive output minus opponent defensive average - Home team attacking skill
  offFeature = tempDat[Team == i_team, .(shotsIB = mean(shotsIB - avgOppShIb), In5perc_w = mean(in5perc_w - avgOppIn5perc_w), in1perc = mean(in1perc - avgOppIn1perc))]
  # Game defensive output minus opponent offensive average - Away team defensive skill
  defFeature = tempDat[Team == opp_date[, Team], .(shotsIB = mean(oppShotsIB - avgShIb), In5perc_w = mean(oppIn5perc_w - avgIn5perc_w), in1perc = mean(oppIn1perc - avgIn1perc))] 
  
  # Then put team, opponent, home/away, avg shots for, opponent avg shots against in a table
  trainData21[[i]] = 
    data.table(Team = i_team, Opponent = opp_date[, Team], h_a = trainData[i, h_a], Goals = trainData[i, Goals], GoalsAgainst = oppStats[, Goals], Sign = trainData[i, Sign], 
               shotsIB = trainData[i, shotsIB], oppShotsIB = trainData[i, oppShotsIB], 
               in5perc_w = trainData[i, in5perc_w], oppIn5perc_w = trainData[i, oppIn5perc_w], 
               shotsIbMean = trainData[i, shotsIbMean], in5perc_wMean = trainData[i, in5perc_wMean], in1percMean = trainData[i, in1percMean],
               oppShotsIbMean = oppStats[, oppShotsIbMean],
               oppIn5perc_wMean = oppStats[, oppIn5perc_wMean],  oppIn1percMean = oppStats[,  oppIn1percMean],
               offSkillShots = offFeature[, shotsIB], offSkillIn5perc = offFeature[, In5perc_w], offSkillIn1perc = offFeature[, in1perc],
               oppSkillShots = defFeature[, shotsIB], oppSkillIn5perc = defFeature[, In5perc_w], oppSkillIn1perc = defFeature[, in1perc])
  
  if((i %% 100) == 0) print(i)
}

trainData21 = rbindlist(trainData21)
trainData21 = na.omit(trainData21)


teamShotsFor = trainData21[Team == "Djurgårdens IF"]

# Compare outcome against team's mean amd opponent's men

ggplot(melt(teamShotsFor[, .(Team, Game = 1:.N, shotsIB, shotsIbMean, oppShotsIbMean)], id.vars = 1:2),
       aes(x = Game, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point()

ggplot(melt(teamShotsFor[, .(Team, Game = 1:.N, h_a, oppShotsIbMean, vsMean = shotsIB - shotsIbMean, vsOppMean = shotsIB - oppShotsIbMean)],
            id.vars = 1:4),
       aes(x = Game, y = value, color = variable)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_point(aes(size = oppShotsIbMean)) +
  scale_size(range = c(1, 6), trans = "reverse")

ggplot(teamShotsFor[, .(Team, Game = 1:.N, h_a, shotsIB, vsOppMean = shotsIB - oppShotsIbMean)],
       aes(x = shotsIB, y = vsOppMean, color = as.factor(h_a))) +
  geom_vline(xintercept = trainData21[, mean(shotsIbMean)]) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5) +
  geom_density_2d_filled(alpha = .4, bins = 3)


library(mclust)

teams = trainData21[, unique(Team)]

# teams = teams[teams %in% c("GIF Sundsvall", "IFK Värnamo", "Helsingborgs IF") == FALSE]

teamShotsData = list()
i = 0
for(t in teams){
  i = i+1
  
  teamShotsFor = trainData21[Team == t]
  
  mb = Mclust(teamShotsFor[,.(shotsIB, vsOppMean = shotsIB - oppShotsIbMean)])
  
  mb$G
  
  mb2 = Mclust(teamShotsFor[,.(shotsIB, vsOppMean = shotsIB - oppShotsIbMean)], 2)
  
  temp = cbind(data.table(t(mb2$parameters$mean)),
               data.table(table(mb2$classification))[, .(N)])
  temp[, Team := t]
  
  teamShotsData[[i]] = temp

}


table(teamShotsFor$h_a, mb$classification)
# vs
table(teamShotsFor$h_a, mb2$classification)

summary(mb, parameters = TRUE)
summary(mb2, parameters = TRUE)


# Få fram bra och dåligt värde för varje klubb, ger en bild av varians/jämnhet
  # Antalet observationer i varje kluster bidrar också till den analysen

teamShotsData = rbindlist(teamShotsData)

library(ggrepel)
ggplot(teamShotsData, aes(x = shotsIB, y = vsOppMean, size = N)) +
  geom_vline(xintercept = trainData21[, mean(shotsIbMean)]) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_text_repel(aes(label = Team), size = 3)
  
teamShotsData[, N2 := N / sum(N), Team]

ggplot(teamShotsData[, .(shotsIB = sum(shotsIB * N2), vsOppMean = sum(vsOppMean * N2)), Team],
       aes(x = shotsIB, y = vsOppMean)) +
  geom_vline(xintercept = trainData21[, mean(shotsIbMean)]) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5) +
  geom_text_repel(aes(label = Team), size = 3)


# Samma fast för defensiven

teamShotsAgainstData = list()
i = 0
for(t in teams){
  i = i+1
  
  teamShotsAgainst = trainData21[Team == t]
  
  mb = Mclust(teamShotsAgainst[,.(oppShotsIB, vsMean = oppShotsIB - shotsIbMean)])
  
  mb$G
  
  mb2 = Mclust(teamShotsAgainst[,.(oppShotsIB, vsMean = oppShotsIB - shotsIbMean)], 2)
  
  temp = cbind(data.table(t(mb2$parameters$mean)),
               data.table(table(mb2$classification))[, .(N)])
  temp[, Team := t]
  
  teamShotsAgainstData[[i]] = temp
  
}


teamShotsAgainstData = rbindlist(teamShotsAgainstData)

library(ggrepel)
ggplot(teamShotsAgainstData, aes(x = oppShotsIB, y = vsMean, size = N)) +
  geom_vline(xintercept = trainData21[, mean(shotsIbMean)]) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_text_repel(aes(label = Team), size = 3)

teamShotsAgainstData[, N2 := N / sum(N), Team]

ggplot(teamShotsAgainstData[, .(oppShotsIB = sum(oppShotsIB * N2), vsMean = sum(vsMean * N2)), Team],
       aes(x = oppShotsIB, y = vsMean)) +
  geom_vline(xintercept = trainData21[, mean(shotsIbMean)]) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5) +
  geom_text_repel(aes(label = Team), size = 3)


# Det här kan ge en summering av offensiv resp defensiv kvalitet
# Användbar tillsammans med tidsserie för att också få  trender

teamShotsData
teamShotsAgainstData


ggplot(melt(trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                        .(Game = 1:.N, h_a, oppShotsIbMean, vsOppMean = shotsIB - oppShotsIbMean), Team],
            id.vars = 1:4),
       aes(x = Game, y = value, color = value > 0, group = 1)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_point(aes(size = oppShotsIbMean)) +
  scale_size(range = c(1, 5), trans = "reverse") +
  facet_wrap(~Team)


ggplot(melt(trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                        .(Game = 1:.N, h_a, shotsIbMean, vsMean = shotsIbMean - oppShotsIB), Team],
            id.vars = 1:4),
       aes(x = Game, y = value, color = value > 0, group = 1)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_point(aes(size = shotsIbMean)) +
  scale_size(range = c(1, 5), trans = "reverse") +
  facet_wrap(~Team)


ggplot(melt(trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                        .(Game = 1:.N, h_a,
                          vsMean = (shotsIbMean - oppShotsIB) + (shotsIB - oppShotsIbMean)), Team],
            id.vars = 1:4),
       aes(x = Game, y = vsMean, color = vsMean > 0, group = 1)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 23.5) + 
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~Team)

# Do same but for touches

ggplot(melt(trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                        .(Game = 1:.N, h_a, oppIn5perc_wMean, vsOppMean =  in5perc_w- oppIn5perc_wMean), Team],
            id.vars = 1:4),
       aes(x = Game, y = value, color = value > 0, group = 1)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_point(aes(size = oppIn5perc_wMean)) +
  scale_size(range = c(1, 5), trans = "reverse") +
  facet_wrap(~Team)


ggplot(melt(trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                        .(Game = 1:.N, h_a, vsMean = in5perc_wMean - oppIn5perc_w), Team],
            id.vars = 1:3),
       aes(x = Game, y = value, color = value > 0, group = 1)) +
  geom_hline(yintercept = 0) + 
  geom_line(size = 1) +
  geom_point(size = 4) +
  facet_wrap(~Team)


ggplot(melt(trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                        .(Game = 1:.N, h_a,
                          vsMean = (in5perc_w- oppIn5perc_wMean) + (in5perc_wMean - oppIn5perc_w)), Team],
            id.vars = 1:3),
       aes(x = Game, y = value, color = value > 0, group = 1)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 23.5) + 
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~Team)

lm(vsMeanShots ~ vsMeanTouches, 
   data = trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                      .(Game = 1:.N, h_a,
                        vsMeanTouches = (in5perc_w- oppIn5perc_wMean) + (in5perc_wMean - oppIn5perc_w),
                        vsMeanShots = (shotsIbMean - oppShotsIB) + (shotsIB - oppShotsIbMean)), Team])



ggplot(trainData21[Team %in% c("Örebro SK", "Halmstads BK", "Östersunds FK") == FALSE,
                   .(Game = .N:1, h_a,
                   vsMeanTouches = (in5perc_w- oppIn5perc_wMean) + (in5perc_wMean - oppIn5perc_w),
                   vsMeanShots = (shotsIbMean - oppShotsIB) + (shotsIB - oppShotsIbMean)), Team],
       aes(x = vsMeanTouches, y = vsMeanShots, color = Game)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)  +
  geom_point(size = 4) +
  facet_wrap(~Team) +
  geom_abline(slope = 0.1960, intercept = 0.2276) +
  scale_color_viridis()
  
  
