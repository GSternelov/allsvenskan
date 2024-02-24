library(data.table)
library(ggplot2)
library(viridis)
library(MASS)
library(gridExtra)
library(grid)
library(marginaleffects)

# xG Model - Check
# Shotmap - Check
# Shotmap team - Check
# xG trendlines - Check
# Code for state data - check
# Zone 14 touches
# Prediction model goals - Check (just same approach but simpler)
# Prediction model goals (state draw) - Check
# predictions model xG + zone 14
# predictions model xG + zone 14 (state draw)

xgDT = function(shDT, Games){
  shotMapDT = list()
  for(i in 1:length(shDT)){
    tempSM = data.table(shDT[[i]]$shotmap)
    
    tempSM = tempSM[, .(player.shortName, player.position, isHome, shotType, situation,
                        playerCoordinates.x, playerCoordinates.y, bodyPart,
                        time, timeSeconds, game = i)]
    
    tempSM[isHome == TRUE, Team := strsplit(Games[[i]], split = " - ")[[1]][1]]
    tempSM[isHome == FALSE, Team := strsplit(Games[[i]], split = " - ")[[1]][2]]
    
    tempSM[isHome == TRUE, Opponent := strsplit(Games[[i]], split = " - ")[[1]][2]]
    tempSM[isHome == FALSE, Opponent := strsplit(Games[[i]], split = " - ")[[1]][1]]
    
    shotMapDT[[i]] = tempSM
    
  }
  shotMapDT = rbindlist(shotMapDT)
  return(shotMapDT)
}

load("Allsv22Shotmaps.rda")
load("Allsv22Games.rda")
Games22 = as.list(Games22)
shDT22 = xgDT(Allsv22Shotmaps, Games22)

load("Allsv23Shotmaps.rda")
load("Allsv23Games.rda")
shDT23 = xgDT(Allsv23Shotmaps, Games23)

shotMapDT = rbind(shDT22, shDT23)

# Angle, distance, situation, bodyPart

  # Create pitch coordinates given pitch size 105x68
shotMapDT[, x2 := (playerCoordinates.x / 100) * 105 ]
shotMapDT[, y2 := 68 - ((playerCoordinates.y / 100) * 68)]

shotMapDT[, range(x2)]; shotMapDT[, range(y2)]

# Distance to center of goal
shotMapDT[, distCenter := round(sqrt((x2)^2 + (y2- 34)^2), 2)]
# Angle to goal
shotMapDT[, angleGoal := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 34)^2 - (7.32/2)^2))]
shotMapDT[angleGoal < 0, angleGoal := angleGoal + pi]
shotMapDT[, angleGoal := angleGoal * 180 / pi]

shotMapDT[, .N, round(angleGoal, 1)]
shotMapDT[, .N, round(distCenter, 0)][order(round)]
shotMapDT[, .N, situation]

shotMapDT[, situation2 := ifelse(situation %in% c("regular", "assisted"), "open play", situation)]
shotMapDT[, situation2 := ifelse(situation %in% c("corner", "set-piece", "throw-in-set-piece"), "set-piece", situation2)]
shotMapDT[, .N, situation2]

shotMapDT[, .N, bodyPart]
shotMapDT[, bodyPart2 := ifelse(bodyPart %in% c("left-foot", "right-foot"), "foot", bodyPart)]
shotMapDT[, .N, bodyPart2]

shotMapDT[, .N, shotType]
shotMapDT[, Goal := ifelse(shotType == "goal", TRUE, FALSE)]
shotMapDT[, .N, Goal]


ggplot(shotMapDT, aes(x = x2, y = y2)) +
  geom_point(size = 3, alpha = .15) +
  annotate("rect", xmin = 0, xmax = 0 + 16.5,
           ymin = 34 - 17.7, ymax = 34 + 17.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0 + 5.5,
           ymin = 34 - 6.7, ymax = 34 + 6.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0,
           ymin = 30.34, ymax = 37.66, fill = "transparent", col = "black", size = 1) +
  
  annotate("rect", xmin = 0, xmax = 52.5, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("point", x = c(94), y = c(34), col = "black") + 
  
  geom_curve(aes(y = 27.2, x = 0 + 16.5, yend = 40.8, xend = 0 + 16.5),curvature = -0.58, colour = "black")+ 
  
  facet_wrap(~Goal, ncol = 2) + xlim(c(0, 55)) + 
  
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72"),
        panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black")) +
  
  coord_flip()


# Add points where prob is 0
# from very far, for example
extraData = data.table(expand.grid(x2 = seq(0, 55, 5), y2 = seq(0, 68, 4), Goal = FALSE, 
                                   situation2 = "open play", bodyPart2 = c("foot")))

# Distance to center of goal
extraData[, distCenter := round(sqrt((x2 - 105)^2 + (y2- 34)^2), 2)]
# Angle to goal
extraData[, angleGoal := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 34)^2 - (7.32/2)^2))]
extraData[angleGoal < 0, angleGoal := angleGoal + pi]
extraData[, angleGoal := angleGoal * 180 / pi]

shotMapDT = rbind(shotMapDT, 
                 extraData,
                 fill = TRUE)

xg_mod =glm(Goal ~ distCenter + I(distCenter^2) + angleGoal + situation2 + bodyPart2,
              data = shotMapDT[situation2 != "penalty"], family = "binomial")
summary(xg_mod)

save(xg_mod, file = "xg_mod.rda")

shotMapDT[situation2 != "penalty", xG := predict(xg_mod, type = "response")]
shotMapDT[situation2 == "penalty", xG := 0.76]

shotMapDT[, sum(xG), player.shortName][order(-V1)][1:20]

# Conformal prediction
conf_xg <- data.table(predictions(xg_mod, newdata = shotMapDT[situation2 != "penalty"]))

conf_xg[, .(.N, Goals = sum(Goal), xG = sum(xG), low = sum(conf.low), high = sum(conf.high), avgUnc = (sum(conf.high) - sum(conf.low)) / .N * 100), player.shortName][order(-xG)][1:20]
conf_xg[, .(.N, xG = sum(xG), low = sum(conf.low), high = sum(conf.high), avgUnc = (sum(conf.high) - sum(conf.low)) / .N), player.shortName][N > 10][order(-avgUnc)][1:20]


ggplot(shotMapDT[Team == "IFK Norrköping"], aes(x = x2, y = y2, size = xG)) +
  geom_point(alpha = .25) +
  annotate("rect", xmin = 0, xmax = 0 + 16.5,
           ymin = 34 - 17.7, ymax = 34 + 17.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0 + 5.5,
           ymin = 34 - 6.7, ymax = 34 + 6.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0,
           ymin = 30.34, ymax = 37.66, fill = "transparent", col = "black", size = 1) +
  
  annotate("rect", xmin = 0, xmax = 52.5, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("point", x = c(94), y = c(34), col = "black") + 
  
  geom_curve(aes(y = 27.2, x = 0 + 16.5, yend = 40.8, xend = 0 + 16.5),curvature = -0.58,
             colour = "black", inherit.aes = FALSE)+ 
  
  facet_wrap(~Goal, ncol = 2) + xlim(c(0, 55)) + 
  
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72"),
        panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black")) +
  
  coord_flip()

season_xG = merge(shotMapDT[situation2 != "penalty", .(xG = sum(xG)), .(game, Team)],
                  shotMapDT[situation2 != "penalty", .(xG_Against = sum(xG)), .(game, Team = Opponent)],
                  by = c("game", "Team"))
season_xG[, round := 1:.N, Team]
season_xG = season_xG[!is.na(game)]

ggplot(melt(season_xG[Team == "IFK Norrköping"], id.vars = c(1, 2, 5)),
       aes(x = round, y = value, color = variable)) +
  geom_vline(xintercept = c(13.5, 17.5), linetype = "dashed") +
  geom_line(size = 1, alpha = .5) +
  geom_smooth(se = FALSE, linetype ="dashed", size = 1.5) +
  theme_bw() + 
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72")) +
  scale_color_manual(name = NULL, values = c("seagreen", "red3"))



ggplot(melt(season_xG, id.vars = c(1, 2, 5)),
       aes(x = round, y = value, color = variable)) +
  geom_line( alpha = .5) +
  geom_smooth(se = FALSE, linetype ="dotdash", size = 1) +
  theme_bw() + 
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72")) +
  scale_color_manual(name = NULL, values = c("seagreen", "red3")) +
  facet_wrap(~Team)

ggplot(season_xG,
       aes(x = round, y = xG - xG_Against)) +
  geom_hline(yintercept = 0) + 
  geom_line( alpha = .5) +
  geom_smooth(se = FALSE, linetype ="dotdash", size = 1) +
  theme_bw() + 
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72")) +
  facet_wrap(~Team)



# State data
  # Sign for home/away, then time at each state
state_data = list()
for(i in 1:length(Allsv22Shotmaps)){
  temp = shotMapDT[game == i][order(timeSeconds)]
  
  temp[, homeState := 0]
  temp[, awayState := 0]
  
  temp[Goal == TRUE, homeState := ifelse(isHome == TRUE, 1, -1)]
  temp[Goal == TRUE, awayState := ifelse(isHome == TRUE, -1, 1)]
  
  temp[, homeState := shift(cumsum(homeState), fill = 0)]
  temp[, awayState := shift(cumsum(awayState), fill = 0)]
  
  temp[, sequence := cumsum(c(0, abs(diff(homeState))))]
  
  tempHome = temp[, .(time = max(timeSeconds), xG = sum(ifelse(isHome == TRUE, xG, 0)),
                      xG_Against = sum(ifelse(isHome == FALSE, xG, 0))), .(sequence, state = homeState)]
  tempHome[, Team := temp[isHome == TRUE, unique(Team)]]
  tempHome[, Opponent := temp[isHome == FALSE, unique(Team)]]
  tempHome[, Home := 1]
  tempHome[, timeState := time - shift(time, fill = 0)]
  tempHome[.N, timeState := ifelse(time > (93*60), timeState + 60, (93*60) - time)]
  
  
  tempAway = temp[, .(time = max(timeSeconds), xG = sum(ifelse(isHome == FALSE, xG, 0)),
                      xG_Against = sum(ifelse(isHome == TRUE, xG, 0))), .(sequence, state = awayState)]
  tempAway[, Team := temp[isHome == FALSE, unique(Team)]]
  tempAway[, Opponent := temp[isHome == TRUE, unique(Team)]]
  tempAway[, Home := 0]
  tempAway[, timeState := time - shift(time, fill = 0)]
  tempAway[.N, timeState := ifelse(time > (93*60), timeState + 60, (93*60) - time)]
  
  state_data[[i]] = rbind(tempHome, tempAway)
}
state_data = rbindlist(state_data)
state_data[, stateClass := sign(state)]

ggplot(state_data[, sum(timeState), .(state, stateClass, Team)],
       aes(x = state, y = V1, fill = as.factor(stateClass))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Team) +
  scale_fill_manual(name = NULL, guide = 'none', values = c("red3", "black", "seagreen"))

ggplot(state_data[, sum(timeState), .(stateClass, Team)],
       aes(x = stateClass, y = V1, fill = as.factor(stateClass))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Team) +
  scale_fill_manual(name = NULL, guide = 'none', values = c("red3", "black", "seagreen"))



xG_stateDraw = state_data[stateClass == 1, .(xG = sum(xG), xG_Against = sum(xG_Against), time = sum(timeState) / 3600), Team]

ggplot(xG_stateDraw, aes(x = time, y = xG - xG_Against)) + 
  geom_hline(yintercept = 0) + 
  geom_point() +
  ggrepel::geom_label_repel(aes(label = Team)) +
  theme_bw()

ggplot(xG_stateDraw, aes(x = xG / time, y = xG_Against / time)) + 
  geom_point() +
  ggrepel::geom_label_repel(aes(label = Team)) +
  theme_bw() +
  scale_y_reverse()



state_data[, .(xG = sum(xG), xG_Against = sum(xG_Against), time = sum(timeState) / 3600), .(Team, stateClass)]
ggplot(state_data[, .(xGDiff = (sum(xG) - sum(xG_Against)) / (sum(timeState) / 3600)), .(Team, stateClass)],
       aes(x = stateClass, y = xGDiff, fill = as.factor(stateClass))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Team) +
  scale_fill_manual(name = NULL, guide = 'none', values = c("red3", "black", "seagreen")) +
  theme_bw()


# Gamma model xG - state draw

gamModelData = state_data[stateClass == 0, .(xG_Min = sum(xG) / (sum(timeState) / 60),
                                             Time = sum(timeState) / 60), .(Team, Opponent, Home)]
ggplot(gamModelData, aes(x = Time)) +
  geom_histogram(binwidth = 2.5) + theme_bw()


gamModelData[xG_Min == 0, xG_Min := xG_Min + 0.00001]
gamModelData[xG_Min > 0.5, xG_Min := 0.5]

gamModelDraw = glm(xG_Min ~ Home + Team + Opponent,
                   family = Gamma(link=log), data = gamModelData)
summary(gamModelDraw)

gamModelDraw$coefficients[2] = gamModelDraw$coefficients[2] / 3

save(gamModelDraw, file = "gamModelDraw.rda")


# Predict a game

predGame = function(ht, at, minDraw = 25, plot = TRUE){
  
  predHome = predict(gamModelDraw, data.table(Home = 1, Team = ht, Opponent = at), type = "response") * minDraw
  predAway = predict(gamModelDraw, data.table(Home = 0, Team = at, Opponent = ht), type = "response") * minDraw
  
  # Want to view the prediction as the mean and the variance to be fixed
  
  V = gamma.shape(gamModelDraw)[1]$alpha
  
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

  
  estPoints = predDT[, .(Prob = .N / predDT[, .N]), Sign][order(Sign), sum(Prob * c(0, 1, 3))]
  return(estPoints)
}


predGame(ht = "Mjällby AIF", at = "Varbergs BoIS", minDraw = 30)


coefDT = data.table(exp(gamModelDraw$coefficients), coef = names(gamModelDraw$coefficients))
coefDT[grepl(pattern = "Opponent", x = coef), variable := "Def"]
coefDT[grepl(pattern = "Team", x = coef), variable := "Off"]

coefDT[grepl(pattern = "Opponent", x = coef), Team := gsub(pattern = "Opponent", replacement = "", x = coef)]
coefDT[grepl(pattern = "Team", x = coef), Team := gsub(pattern = "Team", replacement = "", x = coef)]

defQuality = coefDT[variable == "Def"]
defQuality[, Team := factor(Team, levels = defQuality[order(V1), Team])]

offQuality = coefDT[variable == "Off"]
offQuality[, Team := factor(Team, levels = offQuality[order(V1), Team])]

ggplot(defQuality, aes(x = V1, y = Team)) +
  geom_linerange(aes(xmin = 0, xmax = V1)) + 
  geom_point() + theme_bw()

ggplot(offQuality, aes(x = V1, y = Team)) +
  geom_linerange(aes(xmin = 0, xmax = V1)) + 
  geom_point() + theme_bw()

ggplot(cbind(coefDT[variable == "Off", .(Off = V1, Team)], coefDT[variable == "Def", .(Def = V1)]),
       aes(x = Off, y = Def, label = Team)) +
  geom_label()

# Season prediction

seasonGames = data.table(expand.grid(Team = gamModelData[, unique(Team)], Opponent = gamModelData[, unique(Team)],
                                     Home = c(1, 0)))
seasonGames = seasonGames[Team != Opponent]

for(i in 1:seasonGames[,.N]){
  tempPoints = predGame(ht = seasonGames[i, Team], at = seasonGames[i, Opponent], minDraw = 30, plot = FALSE)
  
  seasonGames[i, Points := tempPoints]
}

seasonGames[, sum(Points), Team][order(-V1)]


# Predict a round


