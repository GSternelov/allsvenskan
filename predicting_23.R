load("Allsv22Shotmaps.rda")
load("Allsv22Games.rda")

load("Allsv23Shotmaps.rda")
load("Allsv23Games.rda")

load("Allsv22Heatmap.rda")
hmTable22 = hmTable
load("Allsv23Heatmap.rda")
hmTable23 = hmTable

load("xg_mod.rda")

source("modelling_prep.R")

shotMapDT22 = shotMap_w_xg(data = Allsv22Shotmaps, games = Games22)
shotMapDT22[, season := 2022]
shotMapDT23 = shotMap_w_xg(data = Allsv23Shotmaps, games = Games23)
shotMapDT23[, season := 2023]

shotMapDT = rbind(shotMapDT22, shotMapDT23)

zoneData22 = touchAttZone(hmTable22)
zoneData22[, season := 2022]

zoneData23 = touchAttZone(hmTable23)
zoneData23[, season := 2023]

# Train new model
load("GamesDate22.rda")
Games2022 = data.table(V1 = Games22)[, c("Team", "Opponent") := tstrsplit(V1, split = " - ")][
  , .(Team, Opponent, Home = 1, season = 2022, date = GamesDate)]

Games2022 = rbind(Games2022, Games2022[, .(Team = Opponent, Opponent = Team, Home = 0, season = 2022, date)])

load("GamesDate23.rda")
Games2023 = data.table(unlist(Games23))[, c("Team", "Opponent") := tstrsplit(V1, split = " - ")][
  , .(Team, Opponent, Home = 1, season = 2023, date = GamesDate)]

Games2023 = rbind(Games2023, Games2023[, .(Team = Opponent, Opponent = Team, Home = 0, season = 2023, date)])

GamesData = rbind(Games2022, Games2023)

state_data_22 = state_data_func(data = Allsv22Shotmaps, smData = shotMapDT22)
state_data_22[, season := 2022]
state_data_23 = state_data_func(data = Allsv23Shotmaps, smData = shotMapDT23)
state_data_23[, season := 2023]

state_data = rbind(state_data_22, state_data_23)

state_data = merge(state_data, GamesData, by = c("Team", "Opponent", "Home", "season"))

# state_data = state_data[Team  %in% c("IF Brommapojkarna", "Halmstads BK") == FALSE &
#                           Opponent %in% c("IF Brommapojkarna", "Halmstads BK") == FALSE]

state_data[, weight := ifelse(season == 2022, 0.7, 1)]

modelData_state1 = state_data[abs(stateClass) <= 1, .(xG_Min = sum(xG) / (sum(timeState) / 60),
                                                      Time = sum(timeState) / 60), .(Team, Opponent, Home, season, weight)]

modelData_stateDraw = state_data[abs(stateClass) == 0, .(xG_Min = sum(xG) / (sum(timeState) / 60),
                                                      Time = sum(timeState) / 60), .(Team, Opponent, Home, season, weight)]

modelData_state1[xG_Min >= quantile(xG_Min, 0.995), xG_Min := quantile(xG_Min, 0.995)]
modelData_stateDraw[xG_Min >= quantile(xG_Min, 0.995), xG_Min := quantile(xG_Min, 0.995)]

modelData_state1[xG_Min == 0, xG_Min := xG_Min + 0.00001]
modelData_stateDraw[xG_Min == 0, xG_Min := xG_Min + 0.00001]

zoneData22[, Home := rep(c(1,0), .N / 2)]
zoneData23[, Home := rep(c(1,0), .N / 2)]

modelData_state1 = merge(modelData_state1, rbind(zoneData22, zoneData23), by = c("Team", "Opponent", "Home", "season"))
modelData_stateDraw = merge(modelData_stateDraw, rbind(zoneData22, zoneData23), by = c("Team", "Opponent", "Home", "season"))


gamModelDraw = glm(xG_Min ~ Home + zone14 + inPenaltyArea + Team + Opponent,
                   family = Gamma(link=log), data = modelData_stateDraw, weights = modelData_stateDraw[, weight])
summary(gamModelDraw)

# gamModelDraw$coefficients[2] = gamModelDraw$coefficients[2] / 3

gamModelState1 = glm(xG_Min ~ Home + zone14 + inPenaltyArea + Team + Opponent,
                   family = Gamma(link=log), data = modelData_state1, weights = modelData_stateDraw[, weight])
summary(gamModelState1)


library(goalmodel)
goalsData = cbind(shotMapDT[season == 2023 & isHome == TRUE, .(GoalsHome = sum(Goal),
                                                               xgHome = sum(ifelse(situation2 == "penalty", 0, xG))),
                            .(game, homeTeam = Team, awayTeam = Opponent)],
                  shotMapDT[season == 2023 & isHome == FALSE, .(GoalsAway = sum(Goal),
                                                                xgAway = sum(ifelse(situation2 == "penalty", 0, xG))),
                            .(game, Team, Opponent)][, .(GoalsAway, xgAway)])

goalsData[, weight := ifelse(game > (max(game) - 8*8), 1, 0.2)]


pointsDT = cbind(shotMapDT[season == 2023, .(Goals = sum(Goal)), .(Team , game, isHome)][order(game, isHome)],
                 shotMapDT[season == 2023, .(GoalsAgainst = sum(Goal)), .(Opponent, game, isHome)][
                   order(game, -isHome), .(GoalsAgainst)]
                 )
pointsFunc = function(value){
  if(value == 1) points = 3
  else if (value == 0) points = 1
  else points = 0
  
  return(points)
}
pointsDT[, Points := pointsFunc(sign(Goals - GoalsAgainst)), .(Team, game)]
pointsDT[, PointsGame := shift(frollmean(Points, n = 6)), Team]

goalsData = merge(goalsData, 
                  cbind(pointsDT[isHome == TRUE, .(game, homeTeam = Team, ppgHome = PointsGame)],
                        pointsDT[isHome == FALSE, .(awayTeam = Team, ppgAway = PointsGame)]),
                  by = c("game", "homeTeam", "awayTeam"))




# Goals model whole 2023 season
goalModel = goalmodel(goals1 = goalsData[, GoalsHome], goals2 = goalsData[, GoalsAway],
                      team1 = goalsData[, homeTeam], team2 = goalsData[, awayTeam])
summary(goalModel)



# Goals model, 20 % weights on all but last 8 games
goalModelWeighted = goalmodel(goals1 = goalsData[, GoalsHome], goals2 = goalsData[, GoalsAway],
                      team1 = goalsData[, homeTeam], team2 = goalsData[, awayTeam],
                      weights = goalsData[, weight],
                      fixed_params = list(hfa = goalModel$parameters$hfa,
                                          intercept = goalModel$parameters$intercept))
summary(goalModelWeighted)

# xG model whole 2023 season
xGModel = goalmodel(goals1 = goalsData[, xgHome], goals2 = goalsData[, xgAway],
                    team1 = goalsData[, homeTeam], team2 = goalsData[, awayTeam],
                    model = "gaussian",
                    fixed_params = list(hfa = goalModel$parameters$hfa))
summary(xGModel)

# xG model, 20 % weights on all but last 8 games
xGModelWeighted = goalmodel(goals1 = goalsData[, xgHome], goals2 = goalsData[, xgAway],
                    team1 = goalsData[, homeTeam], team2 = goalsData[, awayTeam],
                    weights = goalsData[, weight],
                    model = "gaussian",
                    fixed_params = list(hfa = goalModel$parameters$hfa))
summary(xGModelWeighted)

# Goals model with ppg
ppgModel = goalmodel(goals1 = goalsData[!is.na(ppgHome), GoalsHome], goals2 = goalsData[!is.na(ppgHome), GoalsAway],
                     team1 = goalsData[!is.na(ppgHome), homeTeam], team2 = goalsData[!is.na(ppgHome), awayTeam],
                     x1 = as.matrix(goalsData[!is.na(ppgHome), .(ppgHome, ppgAway)]),
                     weights = goalsData[!is.na(ppgHome), weight])
summary(ppgModel)

#xGGoals model with ppg
xGppgModel = goalmodel(goals1 = goalsData[!is.na(ppgHome), xgHome], goals2 = goalsData[!is.na(ppgHome), xgAway],
                     team1 = goalsData[!is.na(ppgHome), homeTeam], team2 = goalsData[!is.na(ppgHome), awayTeam],
                     model = "gaussian",
                     x1 = as.matrix(goalsData[!is.na(ppgHome), .(ppgHome, ppgAway)]),
                     weights = goalsData[!is.na(ppgHome), weight])
summary(xGppgModel)


# Function to get next games
nextRound = data.frame(slug = as.character(), customId = as.numeric(), ID = as.numeric(), Date = as.character(), 
                     Status = as.character(), homeTeam = as.character(), awayTeam = as.character())
Date = seq(as.Date("2023-10-21"), as.Date("2023-10-24"), 1)
for(i in as.character(Date)){
  x <- fromJSON(paste("https://api.sofascore.com/api/v1/sport/football/scheduled-events/", i, sep = ""))
  
  tournAllsv = which(x$events$tournament$id == 24)
  
  if(length(tournAllsv) > 0){
    for(g in 1:length(tournAllsv)){
      nextRound = rbindlist(list(nextRound,
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
nextRound = nextRound[Status != "canceled"]
nextRound = nextRound[-which(duplicated(nextRound$ID))]

# Predict games
library(MASS)
library(gridExtra)

ppgNext = pointsDT[, .(ppg = frollmean(Points, n = 6)), Team][, tail(ppg, 1), Team]
nextRound = merge(nextRound, ppgNext[, .(awayTeam = Team, ppgAway = V1)], by = "awayTeam")
nextRound = merge(nextRound, ppgNext[, .(homeTeam = Team, ppgHome = V1)], by = "homeTeam")

nextRound = as.matrix(nextRound)






# Predict for each goalmodel version
gamePred = rbindlist(list(
  data.table(rbindlist(apply(nextRound, 1, function(x)
    predict_result(model_fit = goalModel, team1 = x[1], team2 = x[2], return_df = TRUE))), model = "gm"),
  data.table(rbindlist(apply(nextRound, 1, function(x)
    predict_result(model_fit = goalModelWeighted, team1 = x[1], team2 = x[2], return_df = TRUE))), model = "gm_w"),
  data.table(rbindlist(apply(nextRound, 1, function(x)
    predict_result(model_fit = xGModel, team1 = x[1], team2 = x[2], return_df = TRUE))), model = "xg"),
  data.table(rbindlist(apply(nextRound, 1, function(x)
    predict_result(model_fit = xGModelWeighted, team1 = x[1], team2 = x[2], return_df = TRUE))), model = "xg_w"),
  
  data.table(rbindlist(apply(nextRound, 1, function(x)
    predict_result(model_fit = ppgModel, team1 = x[1], team2 = x[2],
                   x1 = matrix(data = c(as.numeric(x[8]),
                                        as.numeric(x[9]))),
                   return_df = TRUE))), model = "gm_w_ppg"),
  data.table(rbindlist(apply(nextRound, 1, function(x)
    predict_result(model_fit = xGppgModel, team1 = x[1], team2 = x[2],
                   x1 = matrix(data = c(as.numeric(x[8]),
                                        as.numeric(x[9]))),
                   return_df = TRUE))), model = "xg_w_ppg")
))
gamePred[, game := .GRP, .(team1, team2)]


ggplot(melt(gamePred, measure.vars = 3:5)  , aes(x = variable, y = value, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  facet_wrap(~paste(team1, team2))

ggplot(melt(gamePred, measure.vars = 3:5)  , aes(x = variable, y = ifelse(value < 0.1, 10, 1 / value), fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  facet_wrap(~paste(team1, team2), ncol = 2) +
  geom_text(aes(label = round(1 / value, 2)), size = 3, position = position_dodge(width = .8))


# Trendlines

season_xG = state_data[season == 2023 & abs(stateClass) <= 1,
                       .(xG = sum(xG), xG_Against = sum(xG_Against), Mins = sum(time)),
           .(Team, Opponent, date)][order(Team, date)]

season_xG[, round := 1:.N, Team]

ggplot(melt(season_xG[Team == "IFK Norrköping"], id.vars = c(1:3, 6, 7)),
       aes(x = round, y = value, color = variable)) +
  geom_line(size = 1, alpha = .5) +
  geom_smooth(se = FALSE, linetype ="dashed", size = 1.5) +
  theme_bw() + 
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72")) +
  scale_color_manual(name = NULL, values = c("seagreen", "red3"))



ggplot(melt(season_xG, id.vars = c(1:3, 6, 7)),
       aes(x = round, y = value, color = variable)) +
  geom_line( alpha = .5) +
  geom_smooth(se = FALSE, linetype ="dotdash", size = 1) +
  theme_bw() + 
  theme(legend.key.width = unit(1, "cm"), legend.position = "top", strip.text = element_text(size = 12),
        legend.background = element_rect(fill = "#00000010", color = "grey72")) +
  scale_color_manual(name = NULL, values = c("seagreen", "red3")) +
  facet_wrap(~Team)

ggplot(season_xG,
       aes(x = round, y = (xG - xG_Against) / (Mins / 60))) +
  geom_hline(yintercept = 0) + 
  geom_line( alpha = .5) +
  geom_smooth(se = FALSE, linetype ="dotdash", size = 1) +
  theme_bw() + 
  theme(legend.key.width = unit(1, "cm"), legend.position = "top", strip.text = element_text(size = 12),
        legend.background = element_rect(fill = "#00000010", color = "grey72")) +
  facet_wrap(~Team)




ggplot(shotMapDT[season == 2023 & situation2 != "penalty"],
       aes(x = x2, y = y2, size = xG, fill = Goal)) +
  geom_point(alpha = .75, shape = 21) +
  annotate("rect", xmin = 0, xmax = 0 + 16.5,
           ymin = 34 - 17.7, ymax = 34 + 17.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0 + 5.5,
           ymin = 34 - 6.7, ymax = 34 + 6.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0,
           ymin = 30.34, ymax = 37.66, fill = "transparent", col = "black", size = 1) +
  
  annotate("rect", xmin = 0, xmax = 52.5, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("point", x = c(11), y = c(34), col = "black") + 
  
  geom_curve(aes(y = 27.2, x = 0 + 16.5, yend = 40.8, xend = 0 + 16.5),curvature = -0.58,
             colour = "black", inherit.aes = FALSE)+ 
  
  facet_wrap(~Team, ncol = 4) + xlim(c(0, 52.5)) + 
  
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72"),
        panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black")) +
  
  coord_flip(xlim = c(0, 40)) +
  scale_fill_manual(values = c("indianred", "seagreen"))

ggplot(shotMapDT[season == 2023 & situation2 != "penalty"],
       aes(x = x2, y = y2, size = xG, fill = Goal)) +
  geom_point(alpha = .75, shape = 21) +
  annotate("rect", xmin = 0, xmax = 0 + 16.5,
           ymin = 34 - 17.7, ymax = 34 + 17.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0 + 5.5,
           ymin = 34 - 6.7, ymax = 34 + 6.7, fill = "transparent", col = "black") +
  annotate("rect", xmin = 0, xmax = 0,
           ymin = 30.34, ymax = 37.66, fill = "transparent", col = "black", size = 1) +
  
  annotate("rect", xmin = 0, xmax = 52.5, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("point", x = c(11), y = c(34), col = "black") + 
  
  geom_curve(aes(y = 27.2, x = 0 + 16.5, yend = 40.8, xend = 0 + 16.5),curvature = -0.58,
             colour = "black", inherit.aes = FALSE)+ 
  
  facet_wrap(~Opponent, ncol = 4) + xlim(c(0, 52.5)) + 
  
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72"),
        panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black")) +
  
  coord_flip(xlim = c(0, 40)) +
  scale_fill_manual(values = c("indianred", "seagreen"))


merge(shotMapDT[season == 2023 & situation2 != "penalty", .(xG = sum(xG)), .(Team)],
      shotMapDT[season == 2023 & situation2 != "penalty", .(xG_Against = sum(xG)), .(Team = Opponent)],
      by = c("Team"))[, .(Team, xG, xG_Against, xG_Diff = xG - xG_Against)][order(-xG_Diff)]


p1 = ggplot(merge(shotMapDT[season == 2023 & situation2 != "penalty", .(xG = sum(xG)), .(Team)],
                  shotMapDT[season == 2023 & situation2 != "penalty", .(xG_Against = sum(xG)), .(Team = Opponent)],
                  by = c("Team"))[, .(Team, xG, xG_Against, xG_Diff = xG - xG_Against)][order(-xG_Diff)],
            aes(x = xG_Against, y = xG)) +
  ggrepel::geom_label_repel(aes(label = Team)) +
  geom_point() + 
  scale_x_reverse() +
  theme_bw()

p2 = ggplot(merge(shotMapDT[season == 2023 & situation2 != "penalty", .(xG = mean(xG)), .(Team)],
             shotMapDT[season == 2023 & situation2 != "penalty", .(xG_Against = mean(xG)), .(Team = Opponent)],
             by = c("Team"))[, .(Team, xG, xG_Against, xG_Diff = xG - xG_Against)][order(-xG_Diff)],
       aes(x = xG_Against, y = xG)) +
  ggrepel::geom_label_repel(aes(label = Team)) +
  geom_point() + 
  scale_x_reverse() +
  theme_bw()

grid.arrange(p1, p2, ncol = 2)  



