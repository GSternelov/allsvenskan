library(data.table)
library(ggplot2)
library(viridis)

library(V8)
library(stringr)
library(jsonlite)
library(rvest)
library(parsedate)
library(RCurl)


# load("Allsv23Games.rda")
# load("Allsv23Heatmap.rda")
# load("Allsv23PlayerData.rda")
# # load("Odds22.rda")
# load("Allsv23TeamData.rda")
# load("GamesDate23.rda")
# load("Allsv23Shotmaps.rda")

source("sofascoreFunc.R")

# Step 1, get URL to each game
urlData = data.frame(slug = as.character(), customId = as.numeric(), ID = as.numeric(), Date = as.character(), 
                     Status = as.character(), homeTeam = as.character(), awayTeam = as.character())
Date = seq(as.Date("2024-03-30"), as.Date("2024-04-22"), 1)
for(i in as.character(Date)){
  x <- fromJSON(paste("https://api.sofascore.com/api/v1/sport/football/scheduled-events/", i, sep = ""))
  
  tournAllsv = which(x$events$tournament$id == 24)
  
  if(length(tournAllsv) > 0){
    for(g in 1:length(tournAllsv)){
      urlData = rbindlist(list(urlData,
                               data.table(slug = x$events$slug[tournAllsv[g]],
                                          customId = x$events$customId[tournAllsv[g]],
                                          ID = x$events$id[tournAllsv[g]],
                                          Date = i,
                                          Status = x$events$status$type[tournAllsv[g]],
                                          homeTeam = x$events$homeTeam$name[tournAllsv[g]],
                                          awayTeam    = x$events$awayTeam$name[tournAllsv[g]])))
    }
  }
  
  print(i)
}
urlData = urlData[Status != "canceled"]
urlData = urlData[-which(duplicated(urlData$ID))]

urlData$url = paste("https://www.sofascore.com/sv/", urlData$slug, "/", urlData$customId, sep = "")

# urlData = urlData[c(2:4, 6)]

GamesDate <- c(GamesDate, parse_date(as.character(urlData$Date)))
save(GamesDate, file = "GamesDate24.rda")

k = length(Allsv24)
for(i in 1:nrow(urlData)){
  k = k+1
  x <- fromJSON(paste("https://api.sofascore.com/api/v1/event/", urlData[i, ID], "/lineups", sep = ""))
  Allsv24[[k]] <- x 
  print(i)
}

k = length(Games23)
for(i in 1:nrow(urlData)){
  k = k+1
  #x <- fromJSON(paste("http://www.sofascore.com/event/", urlData[i, ID], "/json", sep = ""))
  Games24[[k]] <- urlData[i, paste(homeTeam, "-", awayTeam)]
}

# for(i in 1:nrow(urlData)){
#   x <- fromJSON(paste("https://api.sofascore.com/api/v1/event/", urlData[i, ID], "/odds/1/all", sep = ""))
#   
#   openOdds = x$markets$choices[[1]]$initialFractionalValue
#   closeOdds = x$markets$choices[[1]]$fractionalValue
# 
#   for(o in 1:3){
#     fracs = as.numeric(strsplit(x = closeOdds[o], split = "/")[[1]])
#     closeOdds[o] = (fracs[1] / fracs[2]) + 1
#     fracs = as.numeric(strsplit(x = openOdds[o], split = "/")[[1]])
#     openOdds[o] = (fracs[1] / fracs[2]) + 1
#   }
#   openOdds = as.numeric(openOdds)
#   closeOdds = as.numeric(closeOdds)
#   
#   Odds21 <- rbind(Odds21,
#                 data.table(homeOpen = openOdds[1], drawOpen = openOdds[2], awayOpen = openOdds[3],
#                                  homeClose = closeOdds[1], drawClose = closeOdds[2], awayClose = closeOdds[3]))
#     
# }
# save(Odds21, file = "Odds21.rda")

k = length(Allsv24TeamStats)
for(i in 1:nrow(urlData)){
  k = k+1
  x <- fromJSON(paste("https://api.sofascore.com/api/v1/event/", urlData[i, ID], "/statistics", sep = ""))
  Allsv24TeamStats[[k]] <- x
}

k = length(Allsv24Shotmaps)
for(i in 1:nrow(urlData)){
  k = k+1
  x <- fromJSON(paste("https://api.sofascore.com/api/v1/event/", urlData[i, ID], "/shotmap", sep = ""))
  Allsv24Shotmaps[[k]] <- x
}

testUrl = tryCatch(
  fromJSON(url)$heatmap,
  error=function(e) e
)
inherits(testUrl, "error") == FALSE

hmList = list()
k = 0
g = length(Allsv24) - nrow(urlData)
for(i in 1:nrow(urlData)){
  g = g+1
  homeTeam = strsplit(Games24[[g]], split = " - ")[[1]][1]
  awayTeam = strsplit(Games24[[g]], split = " - ")[[1]][2]
  
  tempPlayerIDHome = data.table(Allsv24[[g]]$home$players$player[, c("name", "id")])
  minPlayed = Allsv24[[g]]$home$players$statistics$minutesPlayed
  tempPlayerIDHome = tempPlayerIDHome[which(!is.na(minPlayed) & minPlayed > 1)]
  minPlayed = minPlayed[which(!is.na(minPlayed) & minPlayed > 1)]
  for(h in 1:tempPlayerIDHome[, .N]){
    url = paste("https://api.sofascore.com/api/v1/event/", urlData[i, ID], "/player/", tempPlayerIDHome[h, id],
                "/heatmap", sep = "")
    testUrl = tryCatch(
      fromJSON(url)$heatmap,
      error=function(e) e
    )
    if(inherits(testUrl, "error") == FALSE){
      heatMap <- data.table(fromJSON(url)$heatmap)
      heatMap[, Player := tempPlayerIDHome[h, name]]
      heatMap[, Min := minPlayed[h]]
      heatMap[, Team := homeTeam]
      heatMap[, Opponent := awayTeam]
      k = k+1
      hmList[[k]] = heatMap
    }
  }
  
  tempPlayerIDAway = data.table(Allsv24[[g]]$away$players$player[, c("name", "id")])
  minPlayed = Allsv24[[g]]$away$players$statistics$minutesPlayed
  tempPlayerIDAway = tempPlayerIDAway[which(!is.na(minPlayed) & minPlayed > 1)]
  minPlayed = minPlayed[which(!is.na(minPlayed) & minPlayed > 1)]
  for(h in 1:tempPlayerIDAway[, .N]){
    url = paste("https://api.sofascore.com/api/v1/event/", urlData[i, ID], "/player/", tempPlayerIDAway[h, id],
                "/heatmap", sep = "")
    testUrl = tryCatch(
      fromJSON(url)$heatmap,
      error=function(e) e
    )
    if(inherits(testUrl, "error") == FALSE){
      heatMap <- data.table(fromJSON(url)$heatmap)
      heatMap[, Player := tempPlayerIDAway[h, name]]
      heatMap[, Min := minPlayed[h]]
      heatMap[, Team := awayTeam]
      heatMap[, Opponent := homeTeam]
      k = k+1
      hmList[[k]] = heatMap
    }
  }
  print(i)
}
hmTableTemp <-  rbindlist(hmList)

hmTable = rbindlist(list(hmTable, hmTableTemp), fill = TRUE)
hmTable[, range(x)]; hmTable[, range(y)]

save(hmTable, file = "Allsv24Heatmap.rda")

save(Games24, file = "Allsv24Games.rda")
save(Allsv24, file = "Allsv24PlayerData.rda")
save(Allsv24TeamStats, file = "Allsv24TeamData.rda")
save(Allsv24Shotmaps, file = "Allsv24Shotmaps.rda")


gameHeatmap(ind = 16, allsvData = Allsv22, Games = Games22)

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

ggplot(hmTable[Team == "IFK Norrköping"], aes(x = x2, y = y2, color = in5perc)) +
  geom_point() +
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
  annotate("line", x = c(52.5, 52.5), y = c(0, 68))+
  facet_wrap(~Opponent)
  
hmTable[in5perc == 1, sum(xG), Team][order(V1)]
hmTable[in1perc == 1, sum(xG), Team][order(V1)]

  # For this calculation, have to weight the number by  ((totalLongBalls / totalPass)*10)



# Make a statsbomb-ish map over the pitch, showing differences toward mean
  # Both for Players and team

# Have to put each row in hmTable to a grid ID
  # Size: 6x5
x_bound <- seq(0, 100, length.out = 7)
y_bound <- seq(0, 100, length.out = 6)

hmTable[x < x_bound[2], c("xGrid", "xMin", "xMax") := data.table(1, x_bound[1], x_bound[2])]
hmTable[x >= x_bound[2] & x < x_bound[3], c("xGrid", "xMin", "xMax") := data.table(2, x_bound[2], x_bound[3])]
hmTable[x >= x_bound[3] & x < x_bound[4], c("xGrid", "xMin", "xMax") := data.table(3, x_bound[3], x_bound[4])]
hmTable[x >= x_bound[4] & x < x_bound[5], c("xGrid", "xMin", "xMax") := data.table(4, x_bound[4], x_bound[5])]
hmTable[x >= x_bound[5] & x < x_bound[6], c("xGrid", "xMin", "xMax") := data.table(5, x_bound[5], x_bound[6])]
hmTable[x >= x_bound[6], c("xGrid", "xMin", "xMax") := data.table(6, x_bound[6], x_bound[7])]

hmTable[y < y_bound[2], c("yGrid", "yMin", "yMax") := data.table(1, y_bound[1], y_bound[2])]
hmTable[y >= y_bound[2] & y < y_bound[3], c("yGrid", "yMin", "yMax") := data.table(2, y_bound[2], y_bound[3])]
hmTable[y >= y_bound[3] & y < y_bound[4], c("yGrid", "yMin", "yMax") := data.table(3, y_bound[3], y_bound[4])]
hmTable[y >= y_bound[4] & y < y_bound[5], c("yGrid", "yMin", "yMax") := data.table(4, y_bound[4], y_bound[5])]
hmTable[y >= y_bound[5], c("yGrid", "yMin", "yMax") := data.table(5, y_bound[5], y_bound[6])]

hmTable[, Grid := .GRP, .(xGrid, yGrid)]

hmTable[, inPenaltyArea := ifelse(x >= 100 - (100 / 105) * 16.5 & y >= 50 - (100 / 68) * 17.7 & y <= 50 + (100 / 68) * 17.7, 1, 0)]
hmTable[, finalThird := ifelse(x >= 66.67, 1, 0)]

# Remove  goalkeepers
goalKeepers <- as.character()
for(i in 1:length(Allsv20)){
  homeG = which(Allsv20[[i]]$home$players$position == "G")
  goalKeepers <- c(goalKeepers, Allsv20[[i]]$home$players$player$name[homeG])
  awayG = which(Allsv20[[i]]$away$players$position == "G")
  goalKeepers <- c(goalKeepers, Allsv20[[i]]$away$players$player$name[awayG])
}
goalKeepers <- unique(goalKeepers)

GridMean = hmTable[Player %in% goalKeepers == FALSE & Min == 90,
                   .(count = .N, xmin = min(xMin), xmax = max(xMax), ymin = min(yMin), ymax = max(yMax) ),
                   .(Grid, Player, Opponent)]
GridMean = GridMean[, .(count = mean(count)), .(Grid, xmin, xmax, ymin, ymax)]

quantMinMaxPlayer <- GridMean[, quantile(count, c(0.025, 0.975))]
GridMean[, count2 := count]
GridMean[count < quantMinMaxPlayer[1], count2 :=  quantMinMaxPlayer[1]]
GridMean[count > quantMinMaxPlayer[2], count2 :=  quantMinMaxPlayer[2]]

ggplot(GridMean,
       aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = count)) + geom_rect() +  
  annotate("rect", xmin = 0, xmax = (100 / 105) * 16.5,
           ymin = 50 - (100 / 68) * 17.7, ymax = 50 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 100, xmax = 100 - (100 / 105) * 16.5,
           ymin = 50 - (100 / 68) * 17.7, ymax = 50 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = (100 / 105) * 5.5,
           ymin = 50 - (100 / 68) * 6.7, ymax = 50 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 100, xmax = 100 - (100 / 105) * 5.5,
           ymin = 50 - (100 / 68) * 6.7, ymax = 50 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "transparent", col = "black") +
  annotate("line", x = c(50, 50), y = c(0, 100)) +
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = GridMean[, mean(count)])

playerGrid <- hmTable[Player %in% goalKeepers == FALSE,
                      .(count = .N, xmin = min(xMin), xmax = max(xMax), ymin = min(yMin), ymax = max(yMax) ),
                      .(Grid, Player, Team, Opponent)]
playerGrid = merge(GridMean,
                   playerGrid,
                   by = c("Grid", "xmin", "xmax", "ymin", "ymax"), allow.cartesian = TRUE)

playerGrid[is.na(count.y), count.y := 0]
playerGrid[, countDiff := count.y - count.x]

quantMinMaxPlayer <- playerGrid[, quantile(countDiff, c(0.025, 0.975))]
playerGrid[, countDiff2 := countDiff]
playerGrid[countDiff < quantMinMaxPlayer[1], countDiff2 :=  quantMinMaxPlayer[1]]
playerGrid[countDiff > quantMinMaxPlayer[2], countDiff2 :=  quantMinMaxPlayer[2]]

playerHeatmap("Ouma")

playerGridmap(Name = "Ortmark", quantLim = quantMinMaxPlayer)

# Next step could be to calculate the mean grid given position

# Team touch charts
TeamGrid <- hmTable[Player %in% goalKeepers == FALSE,
                      .(count = .N, xmin = min(xMin), xmax = max(xMax), ymin = min(yMin), ymax = max(yMax) ),
                      .(Grid, Team)]
TeamGrid = merge(GridMean,
                   TeamGrid,
                   by = c("Grid", "xmin", "xmax", "ymin", "ymax"), allow.cartesian = TRUE)

TeamGrid[is.na(count.y), count.y := 0]
TeamGrid[, avgGrid := mean(count.y), Grid]
TeamGrid[, countDiff := count.y - avgGrid]

quantMinMaxTeam <- TeamGrid[, quantile(countDiff, c(0.01, 0.99))]
TeamGrid[, countDiff2 := countDiff]
TeamGrid[countDiff < quantMinMaxTeam[1], countDiff2 :=  quantMinMaxTeam[1]]
TeamGrid[countDiff > quantMinMaxTeam[2], countDiff2 :=  quantMinMaxTeam[2]]

gameGridmap(team1 = "IK Sirius", team2 = "IFK Norrköping", quantLim = quantMinMaxTeam)

TeamGridmap(team = "IFK Norrköping", quantLim = quantMinMaxTeam)

ggplot(TeamGrid[grepl("", x = Team)],
       aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = countDiff)) + geom_rect() +  
  annotate("rect", xmin = 0, xmax = (100 / 105) * 16.5,
           ymin = 50 - (100 / 68) * 17.7, ymax = 50 + (100 / 68) * 17.7, fill = "transparent", col = "black") +  
  annotate("rect", xmin = 100, xmax = 100 - (100 / 105) * 16.5,
           ymin = 50 - (100 / 68) * 17.7, ymax = 50 + (100 / 68) * 17.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = (100 / 105) * 5.5,
           ymin = 50 - (100 / 68) * 6.7, ymax = 50 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 100, xmax = 100 - (100 / 105) * 5.5,
           ymin = 50 - (100 / 68) * 6.7, ymax = 50 + (100 / 68) * 6.7, fill = "transparent", col = "black") + 
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "transparent", col = "black") +
  annotate("line", x = c(50, 50), y = c(0, 100)) +
  annotate("path", x=50+9.15*cos(seq(0,2*pi,length.out=100)),
            y=50+9.15*sin(seq(0,2*pi,length.out=100))) +
  annotate("point", x = c(11, 50, 89), y = c(50, 50 ,50), size = 1) + 
  facet_wrap(~Team, nrow = 4) +
  scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = TeamGrid[, quantile(countDiff, 0.5)], 
                       limits = quantMinMaxTeam, breaks = TeamGrid[, quantile(countDiff, c(0.1, 0.9))], name = NULL,
                       labels = c("Less than\naverage", "More than\naverage")) +
  labs(x = NULL, y = NULL, title = "Touch map", subtitle = "Allsvenskan 2020") +
  theme(legend.position = "bottom", legend.key.width = unit(5, "cm"), 
        panel.background = element_rect(fill = "white"), axis.text = element_blank(),
        axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black"))


gameStat = list()
k = 0
for(i in 1:length(Allsv20)){
  k = k+1
  temp = data.table(Allsv20[[i]]$home$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  temp[, bigChances := as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 2])]
  temp[, shotsInsideBox := as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 2])]
  temp[, shotsOutsideBox := as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][5, 2])]
  temp[, Team := strsplit(Games[[i]], split = " - ")[[1]][1]]
  temp[, Opponent := strsplit(Games[[i]], split = " - ")[[1]][2]]
  gameStat[[k]] <- temp
  k = k+1
  temp = data.table(Allsv20[[i]]$away$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  temp[, bigChances := as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 3])]
  temp[, shotsInsideBox := as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 3])]
  temp[, shotsOutsideBox := as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][5, 3])]
  temp[, Team := strsplit(Games[[i]], split = " - ")[[1]][2]]
  temp[, Opponent := strsplit(Games[[i]], split = " - ")[[1]][1]]
  gameStat[[k]] <- temp
}
gameStat = rbindlist(gameStat, fill = TRUE)
wrongInd = gameStat[, which(is.na(shotsInsideBox))]
gameStat[wrongInd, shotsInsideBox := bigChances]
gameStat[wrongInd, bigChances := 0]

setnafill(x = gameStat, fill = 0, cols = which(names(gameStat) %in% c("Team", "Opponent") == FALSE))


offStat = gameStat[, .(
  Goals = sum(goals),
  shotOffTarget = sum(shotOffTarget), 
  onTargetScoringAttempt = sum(onTargetScoringAttempt),
  blockedScoringAttempt = sum(blockedScoringAttempt),
  bigChances = sum(bigChances),
  shotsInsideBox = sum(shotsInsideBox),
  shotsOutsideBox = sum(shotsOutsideBox),
  keyPass = sum(keyPass), 
  Shots = sum(shotOffTarget + onTargetScoringAttempt + blockedScoringAttempt),
  Passes = sum(totalPass), 
  Touches = sum(touches)),
  Team]

offTouches = hmTable[, .(penaltyArea = sum(inPenaltyArea), finalThird = sum(finalThird)), Team]
offStat = merge(offStat, offTouches, by = "Team")
offStat[, touchesPerPenaltyArea := round(Touches / penaltyArea, 2)]
offStat[, touchesPerfinalThird := round(Touches / finalThird, 2)]


offStatRank = melt(offStat[, .(ShotsRank = frankv(Shots, order = -1),
                               shotsInsideBoxRank = frankv(shotsInsideBox, order = -1) * 0.5,
                               bigChancesRank = frankv(bigChances, order = -1),
                               penaltyAreaRank = frankv(penaltyArea, order = -1) * 0.5,
                               finalThirdRank = frankv(finalThird, order = -1),
                               Team)],
                   id.vars = "Team")
offStatRank[, Team := factor(Team, levels = offStatRank[, sum(value), Team][order(V1), Team]) ]

ggplot(offStatRank,
       aes(x = variable, y = value)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~Team) +
  coord_flip()




# Player stats

playerStat = list()
k = 0
for(i in 1:length(Allsv20)){
  k = k+1
  temp = data.table(Allsv20[[i]]$home$players$statistics)
  temp[, player := Allsv20[[i]]$home$players$player$name]
  temp[, position := Allsv20[[i]]$home$players$player$position]
  temp[, Team := strsplit(Games[[i]], split = " - ")[[1]][1]]
  temp[, Opponent := strsplit(Games[[i]], split = " - ")[[1]][2]]
  temp[, Date := GamesDate[i]]
  playerStat[[k]] <- temp
  
  k = k+1
  temp = data.table(Allsv20[[i]]$away$players$statistics)
  temp[, player := Allsv20[[i]]$away$players$player$name]
  temp[, position := Allsv20[[i]]$away$players$player$position]
  temp[, Team := strsplit(Games[[i]], split = " - ")[[1]][2]]
  temp[, Opponent := strsplit(Games[[i]], split = " - ")[[1]][1]]
  temp[, Date := GamesDate[i]]
  playerStat[[k]] <- temp
}
playerStat = rbindlist(playerStat, fill = TRUE)

playerInBox = hmTable[, .(touchesInBox = sum(inPenaltyArea)), .(player = Player, Team, Opponent)]

playerStat = merge(playerStat, playerInBox, by = c("Team", "Opponent", "player"), all.x = TRUE)
setnafill(x = playerStat, fill = 0, cols = which(names(playerStat) %in% c("Team", "Opponent", "Date", "position", "player") == FALSE))


library(zoo)

playerTrendAll = 
  playerStat[, .(touches = rollsum(touches, k = 3), touchesInBox = rollsum(touchesInBox, k = 3),
                 possessionLostCtrl = rollsum(possessionLostCtrl, k = 3),
                 bigChanceCreated = rollsum(bigChanceCreated, k = 3),
                 keyPass = rollsum(keyPass, k = 3), dispossessed = rollsum(dispossessed, k = 3),
                 totalCross = rollsum(totalCross, k = 3), wasFouled = rollsum(wasFouled, k = 3),
                 shots = rollsum(shotOffTarget + onTargetScoringAttempt + blockedScoringAttempt, k = 3),
                 dribbles = rollsum(totalContest, k = 3), dribblesSucc = rollsum(wonContest, k = 3),
                 minutesPlayed = rollsum(minutesPlayed, k = 3) / 90), player]
playerTrendAll = melt(playerTrendAll, id.vars = c("player", "minutesPlayed"))
playerTrendAll[, value90 := value / minutesPlayed]

playerTrend = playerStat[grepl(pattern = "Jesper Karlss|Sead|Vecchia", x = player), .(touches = rollsum(touches, k = 3), touchesInBox = rollsum(touchesInBox, k = 3),
                                                                           possessionLostCtrl = rollsum(possessionLostCtrl, k = 3),
                                                                           bigChanceCreated = rollsum(bigChanceCreated, k = 3),
                                                                           keyPass = rollsum(keyPass, k = 3), dispossessed = rollsum(dispossessed, k = 3),
                                                                           totalCross = rollsum(totalCross, k = 3), wasFouled = rollsum(wasFouled, k = 3),
                                                                           shots = rollsum(shotOffTarget + onTargetScoringAttempt + blockedScoringAttempt, k = 3),
                                                                           dribbles = rollsum(totalContest, k = 3), dribblesSucc = rollsum(wonContest, k = 3),
                                                                           minutesPlayed = rollsum(minutesPlayed, k = 3) / 90), player]
playerTrend = melt(playerTrend, id.vars = c("player", "minutesPlayed"))
playerTrend[, value90 := value / minutesPlayed]
playerTrend[, N := 1:.N, .(variable, player)]


quantLimits = playerTrendAll[variable %in% c("touchesInBox", "keyPass", "shots", "dribblesSucc", "bigChanceCreated", "wasFouled"),
                             quantile(value, probs = c(0.025, 0.975)), variable]

ggplot(playerTrend[variable %in% c("touchesInBox", "keyPass", "shots", "dribblesSucc", "bigChanceCreated", "wasFouled")], aes(x = N, y = value90, col = player)) +
  geom_hline(data = quantLimits, aes(yintercept = V1)) + geom_line(size = 1) +
  facet_wrap(~variable, scales = "free") +
  scale_color_viridis(discrete = TRUE, end = 0.9)


# Train prediction model
library(goalmodel)
library(implied)

load("Allsv18PlayerData.rda")
load("Allsv18TeamData.rda")
load("Allsv18Games.rda")
load("GamesDate18.rda")
load("Allsv18Heatmap.rda")

hmTable18[, inPenaltyArea := ifelse(x >= 100 - (100 / 105) * 16.5 & y >= 50 - (100 / 68) * 17.7 & y <= 50 + (100 / 68) * 17.7, 1, 0)]
# Must fix a unique ID for each game
hmTable18[, TeamLag := shift(Team, n = 1)]
hmTable18[, OpponentLag := shift(Opponent, n = 1)]

hmTable18[, Game := ifelse(Team == TeamLag & Opponent == OpponentLag, 0, 1)]
hmTable18[1, Game := 0]
hmTable18[, Game := cumsum(Game) + 1]

Games18 = Games

matchStat18 = list()
g = 1
for(i in 1:length(Games18)){
  tempDT = data.table(homeTeam = strsplit(Games18[[i]], split = " - ")[[1]][1],
                      awayTeam = strsplit(Games18[[i]], split = " - ")[[1]][2],
                      homeGoals = sum(Allsv18[[i]]$home$players$statistics$goals, na.rm = TRUE) + sum(Allsv18[[i]]$away$players$statistics$ownGoals, na.rm = TRUE),
                      awayGoals = sum(Allsv18[[i]]$away$players$statistics$goals, na.rm = TRUE) + sum(Allsv18[[i]]$home$players$statistics$ownGoals, na.rm = TRUE),
                      homeShotsInBox = as.numeric(Allsv18TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 2]),
                      awayShotsInBox = as.numeric(Allsv18TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 3]),
                      homeBigChances = as.numeric(Allsv18TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 2]),
                      awayBigChances = as.numeric(Allsv18TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 3]),
                      homeTouchesInBox = hmTable18[Game == g & inPenaltyArea == 1, .N],
                      awayTouchesInBox = hmTable18[Game == (g+1) & inPenaltyArea == 1, .N]
  )
  g = g+2
  matchStat18[[i]] = tempDT
}
matchStat18 = rbindlist(matchStat18)
matchStat18[, Date := GamesDate[-13]]
wrongInd = matchStat18[, which(is.na(homeShotsInBox))]
matchStat18[wrongInd, homeShotsInBox := homeBigChances]
matchStat18[wrongInd, homeBigChances := 0]
matchStat18[wrongInd, awayShotsInBox := awayBigChances]
matchStat18[wrongInd, awayBigChances := 0]


# dslm <- days_since_last_match(matchStat19$homeTeam, matchStat19$awayTeam, matchStat19$Date)

# matchStat19[, homeDaysSinceLast := dslm[, 1]]
# matchStat19[, awayDaysSinceLast := dslm[, 2]]
matchStat18 = matchStat18[1:240]

matchStat18Mod = na.omit(matchStat18)


load("Allsv19PlayerData.rda")
load("Allsv19TeamData.rda")
load("Allsv19Games.rda")
load("GamesDate19.rda")
load("Allsv19Heatmap.rda")

hmTable19[, inPenaltyArea := ifelse(x >= 100 - (100 / 105) * 16.5 & y >= 50 - (100 / 68) * 17.7 & y <= 50 + (100 / 68) * 17.7, 1, 0)]
# Must fix a unique ID for each game
hmTable19[, TeamLag := shift(Team, n = 1)]
hmTable19[, OpponentLag := shift(Opponent, n = 1)]

hmTable19[, Game := ifelse(Team == TeamLag & Opponent == OpponentLag, 0, 1)]
hmTable19[1, Game := 0]
hmTable19[, Game := cumsum(Game) + 1]

Games19 = Games

matchStat19 = list()
g = 1
for(i in 1:length(Games19)){
  tempDT = data.table(homeTeam = strsplit(Games19[[i]], split = " - ")[[1]][1],
                      awayTeam = strsplit(Games19[[i]], split = " - ")[[1]][2],
                      homeGoals = sum(Allsv19[[i]]$home$players$statistics$goals, na.rm = TRUE) + sum(Allsv19[[i]]$away$players$statistics$ownGoals, na.rm = TRUE),
                      awayGoals = sum(Allsv19[[i]]$away$players$statistics$goals, na.rm = TRUE) + sum(Allsv19[[i]]$home$players$statistics$ownGoals, na.rm = TRUE),
                      homeShotsInBox = as.numeric(Allsv19TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 2]),
                      awayShotsInBox = as.numeric(Allsv19TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 3]),
                      homeBigChances = as.numeric(Allsv19TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 2]),
                      awayBigChances = as.numeric(Allsv19TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 3]),
                      homeTouchesInBox = hmTable19[Game == g & inPenaltyArea == 1, .N],
                      awayTouchesInBox = hmTable19[Game == (g+1) & inPenaltyArea == 1, .N]
  )
  g = g+2
  matchStat19[[i]] = tempDT
}
matchStat19 = rbindlist(matchStat19)
matchStat19[, Date := GamesDate]

wrongInd = matchStat19[, which(is.na(homeShotsInBox))]
matchStat19[wrongInd, homeShotsInBox := homeBigChances]
matchStat19[wrongInd, homeBigChances := 0]
matchStat19[wrongInd, awayShotsInBox := awayBigChances]
matchStat19[wrongInd, awayBigChances := 0]


# dslm <- days_since_last_match(matchStat19$homeTeam, matchStat19$awayTeam, matchStat19$Date)

# matchStat19[, homeDaysSinceLast := dslm[, 1]]
# matchStat19[, awayDaysSinceLast := dslm[, 2]]
matchStat19 = matchStat19[1:240]

matchStat19Mod = na.omit(matchStat19)


allsv1819 <- rbind(matchStat18Mod, matchStat19Mod)

game_weights <- weights_dc(allsv1819$Date, xi=0.003)

ggplot(data.table(Date = allsv1819$Date, Weights = game_weights), aes(x = Date, y = Weights)) + geom_line()

gm19 = goalmodel(goals1 = allsv1819$homeGoals, goals2 = allsv1819$awayGoals,
                 team1 = allsv1819$homeTeam, team2 = allsv1819$awayTeam,
                 x1 = as.matrix(allsv1819[, .(homeShotsInBox, homeBigChances, homeTouchesInBox)]),
                 x2 = as.matrix(allsv1819[, .(awayShotsInBox, awayBigChances, awayTouchesInBox)]),
                 fixed_params = list("intercept" = 0), model = "poisson", weights = game_weights)
summary(gm19)

gm19 = goalmodel(goals1 = allsv1819$homeGoals, goals2 = allsv1819$awayGoals,
                 team1 = allsv1819$homeTeam, team2 = allsv1819$awayTeam,
                 x1 = as.matrix(allsv1819[, .(homeShotsInBox, homeBigChances, homeTouchesInBox)]),
                 x2 = as.matrix(allsv1819[, .(awayShotsInBox, awayBigChances, awayTouchesInBox)]),
                 fixed_params = gm19$parameters, model = "poisson", dc = TRUE)
summary(gm19)

gm19$parameters$hfa = 0.2

matchStatLong = melt(allsv1819, measure.vars = 1:2)

matchStatLong[, Weight := weights_dc(matchStatLong$Date, xi=0.003)]
# How good is the model?
matchUpPred <- function(Team1, Team2){
  team1home = matchStatLong[value == Team1 & variable == "homeTeam",
                            .(ShotsInBox = sum(homeShotsInBox * Weight), BigChances = sum(homeBigChances * Weight), TouchesInBox = sum(homeTouchesInBox * Weight))]
  team1away = matchStatLong[value == Team1 & variable == "awayTeam",
                            .(ShotsInBox = sum(awayShotsInBox * Weight), BigChances = sum(awayBigChances * Weight), TouchesInBox = sum(awayTouchesInBox * Weight))]
  team1_data = rbind(team1home, team1away)
  team2home = matchStatLong[value == Team2 & variable == "homeTeam",
                            .(ShotsInBox = sum(homeShotsInBox * Weight), BigChances = sum(homeBigChances * Weight), TouchesInBox = sum(homeTouchesInBox * Weight))]
  team2away = matchStatLong[value == Team2 & variable == "awayTeam",
                            .(ShotsInBox = sum(awayShotsInBox * Weight), BigChances = sum(awayBigChances * Weight), TouchesInBox = sum(awayTouchesInBox * Weight))]
  team2_data = rbind(team2home, team2away)
  
  return(predict_result(model_fit = gm19, team1 = Team1, team2 = Team2,
                        x1 = as.matrix(team1_data[, .(mean(ShotsInBox), mean(BigChances), mean(TouchesInBox))]),
                        x2 = as.matrix(team2_data[, .(mean(ShotsInBox), mean(BigChances), mean(TouchesInBox))])))

}

load("Allsv20Games.rda")
load("Allsv20PlayerData.rda")
VarMaif = 0
for(i in 1:length(Games)){
  if(grepl(pattern = "Varberg", x = paste(strsplit(x = Games[[i]], split = " - ")[[1]], collapse = " ")) & i <= 80){
    VarMaif[i] = 1
  }else if(grepl(pattern = "Mjällby", x = paste(strsplit(x = Games[[i]], split = " - ")[[1]], collapse = " "))  & i <= 80){
    VarMaif[i] = 1
  }else{
    VarMaif[i] = 0
  }
  
}

GamesToPred = Games[-which(VarMaif == 1)]
pred = matrix(NA, nrow = length(GamesToPred), ncol = 3)
outcome = matrix(0, nrow = length(GamesToPred), ncol = 3)
Allsv20 = Allsv20[-which(VarMaif == 1)]

for(i in 1:length(GamesToPred)){
  # pred[i, ] = 
  #   matchUpPred(Team1 = strsplit(GamesToPred[[i]], split = " - ")[[1]][1],
  #               Team2 = strsplit(GamesToPred[[i]], split = " - ")[[1]][2])
  temp = data.table(Allsv20[[i]]$home$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  homescore = temp$goals; if(is.null(homescore)) homescore = 0
  homeOwnGoals = temp$ownGoals; if(is.null(homeOwnGoals)) homeOwnGoals = 0
  
  temp = data.table(Allsv20[[i]]$away$players$statistics)
  temp = temp[, lapply(.SD, sum, na.rm=TRUE), .SDcols=names(temp)]
  awayscore = temp$goals ; if(is.null(awayscore)) awayscore = 0
  awayOwnGoals = temp$ownGoals; if(is.null(awayOwnGoals)) awayOwnGoals = 0
  
  homescore = homescore + awayOwnGoals
  awayscore = awayscore + homeOwnGoals
  
  Res = sign(homescore - awayscore)
  if(Res == 0) outcome[i, 2] = 1
  if(Res == -1) outcome[i, 3] = 1
  if(Res == 1) outcome[i, 1] = 1
}

# Score calculations
ignScores = as.numeric()
for(i in 1:nrow(pred)){
  ignScores[i] = -log(pred[i, which(outcome[i,] == 1)], base = 2)
}
# Want to compare the ignorance of the model with the ignorance of the bookmakers
load("Odds20.rda")

OddsToPred = Odds[-which(VarMaif == 1)]

OddsToPred[, c("homeOpen", "drawOpen", "awayOpen") := 
             data.table(implied_probabilities(odds = OddsToPred[, .(homeOpen, drawOpen, awayOpen)], method = "shin")$probabilities)]

OddsToPred[, c("homeClose", "drawClose", "awayClose") := 
             data.table(implied_probabilities(odds = OddsToPred[, .(homeClose, drawClose, awayClose)], method = "shin")$probabilities)]

ignScoresOpen = as.numeric()
ignScoresClose = as.numeric()

for(i in 1:OddsToPred[, .N]){
  Ind = which(outcome[i,] == 1)
  Ind2 = which(outcome[i,] == 1) + 3
  ignScoresOpen[i] = as.numeric(-log(OddsToPred[i, ..Ind], base = 2))
  ignScoresClose[i] = as.numeric(-log(OddsToPred[i, ..Ind2], base = 2))
}

mean(ignScores)
mean(ignScoresOpen)
mean(ignScoresClose)

modelRes = list()
for(i in 1:length(GamesToPred)){
  Game = GamesToPred[[i]]
  modelRes[[i]] = data.table(Game, t(pred[i, ]), (OddsToPred[i, .(homeOpen, drawOpen, awayOpen)]),
                             ignScores = ignScores[i], ignScoresOpen = ignScoresOpen[i], ignScoresClose = ignScoresClose[i])
  
}
modelRes = rbindlist(modelRes)
modelRes[, Edge := ignScores < ignScoresOpen]

GameBet = data.table(Game = as.numeric(), Sign = as.numeric(), Outcome = as.numeric(), Odds = as.numeric(), OddsP = as.numeric(), Pred = as.numeric())
Odds2 = Odds[-which(VarMaif == 1)]
for(i in 1:modelRes[, .N]){
  if(modelRes[i, sum(c(V1, V2, V3) > c(homeOpen, drawOpen, awayOpen))] > 0){
    GameBet = rbind(GameBet,
                    data.table(Game = i, Sign = modelRes[i, which(c(V1, V2, V3) > c(homeOpen, drawOpen, awayOpen))],
                               Outcome = which(outcome[i, ] == 1),
                               Odds = as.numeric(Odds2[i, modelRes[i, which(c(V1, V2, V3) > c(homeOpen, drawOpen, awayOpen))], with = FALSE]),
                               OddsP = as.numeric(modelRes[i, modelRes[i, which(c(V1, V2, V3) > c(homeOpen, drawOpen, awayOpen))] + 4, with = FALSE]),
                               Pred = pred[i, modelRes[i, which(c(V1, V2, V3) > c(homeOpen, drawOpen, awayOpen))]]))
  }
}
GameBet[, Value := Pred - OddsP]
# Add prediction to GameBet

GameBet[, truePred := Sign == Outcome]
GameBet[, Stake := 50]
GameBet[truePred == FALSE, Res := -Stake]
GameBet[truePred == TRUE, Res := (Stake * Odds) - Stake]
GameBet[, (sum(Res) + sum(Stake)) / sum(Stake)] # Flat ROI

GameBet[, Stake2 := 150 * Pred]
GameBet[truePred == FALSE, Res2 := - Stake2]
GameBet[truePred == TRUE, Res2 := (Stake2 * Odds) - Stake2]
GameBet[, (sum(Res2) + sum(Stake2)) / sum(Stake2)] # Weighted ROI

GameBet[, Stake3 := 50]
GameBet[truePred == FALSE & Value > 0.03 , Res3 := -Stake]
GameBet[truePred == TRUE & Value > 0.03, Res3 := (Stake * Odds) - Stake]
GameBet[ Value <= 0.03, Res3 := 0]
GameBet[ Value <= 0.03, Stake3 := 0]
GameBet[, (sum(Res3) + sum(Stake3)) / sum(Stake3)] # Flat ROI

GameBet[, .(sum(Res), sum(Res2), sum(Res3))]
GameBet[, .(sum(Stake), sum(Stake2), sum(Stake3))]

ggplot(melt(GameBet[, .(Res = cumsum(Res), Res2 = cumsum(Res2), Res3 = cumsum(Res3), 1:.N)], id.vars = "V4"),
       aes(x = V4, y = value, col = variable)) + geom_line() +
  geom_smooth(se = FALSE)



# Looking quite promising actually

# Need a larger dataset
# Effect of always using the latest data (should improve, right?)
# How fast does the open odds change?


## Effect of using latest data (one week before game)

  # Write a function that takes all data sources with dates
    # Have parameters to decide which matches to use for training and which to predict

# Add 2020 data
load("Allsv20PlayerData.rda")
load("Allsv20TeamData.rda")
load("Allsv20Games.rda")
load("GamesDate20.rda")
load("Allsv20Heatmap.rda")

hmTable[, inPenaltyArea := ifelse(x >= 100 - (100 / 105) * 16.5 & y >= 50 - (100 / 68) * 17.7 & y <= 50 + (100 / 68) * 17.7, 1, 0)]
# Must fix a unique ID for each game
hmTable[, TeamLag := shift(Team, n = 1)]
hmTable[, OpponentLag := shift(Opponent, n = 1)]

hmTable[, Game := ifelse(Team == TeamLag & Opponent == OpponentLag, 0, 1)]
hmTable[1, Game := 0]
hmTable[, Game := cumsum(Game) + 1]

Games20 = Games

matchStat20 = list()
g = 1
for(i in 1:length(Games20)){
  tempDT = data.table(homeTeam = strsplit(Games20[[i]], split = " - ")[[1]][1],
                      awayTeam = strsplit(Games20[[i]], split = " - ")[[1]][2],
                      homeGoals = sum(Allsv20[[i]]$home$players$statistics$goals, na.rm = TRUE) + sum(Allsv20[[i]]$away$players$statistics$ownGoals, na.rm = TRUE),
                      awayGoals = sum(Allsv20[[i]]$away$players$statistics$goals, na.rm = TRUE) + sum(Allsv20[[i]]$home$players$statistics$ownGoals, na.rm = TRUE),
                      homeShotsInBox = as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 2]),
                      awayShotsInBox = as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][4, 3]),
                      homeBigChances = as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 2]),
                      awayBigChances = as.numeric(Allsv20TeamStats[[i]]$statistics$groups[[1]]$statisticsItems[[4]][1, 3]),
                      homeTouchesInBox = hmTable[Game == g & inPenaltyArea == 1, .N],
                      awayTouchesInBox = hmTable[Game == (g+1) & inPenaltyArea == 1, .N]
  )
  g = g+2
  matchStat20[[i]] = tempDT
}
matchStat20 = rbindlist(matchStat20)
matchStat20[, Date := GamesDate]

wrongInd = matchStat20[, which(is.na(homeShotsInBox))]
matchStat20[wrongInd, homeShotsInBox := homeBigChances]
matchStat20[wrongInd, homeBigChances := 0]
matchStat20[wrongInd, awayShotsInBox := awayBigChances]
matchStat20[wrongInd, awayBigChances := 0]

           
# dslm <- days_since_last_match(matchStat20$homeTeam, matchStat20$awayTeam, matchStat20$Date)

# matchStat20[, homeDaysSinceLast := dslm[, 1]]
# matchStat20[, awayDaysSinceLast := dslm[, 2]]
# matchStat20 = matchStat20[1:240]

matchStat20Mod = na.omit(matchStat20)
matchStat20Mod[, DateDiff := c(0, diff.Date(Date)) /  86400]
matchStat20Mod[, Round := cumsum(ifelse(DateDiff > 1, 1, 0)) + 1]

allsv181920 = rbind(allsv1819, matchStat20Mod, fill = TRUE)


# Does my 2020 result improve if I add all games from previous round to continously update the model?
  # Sofa score open odds are gathered when the last game of the previous round has been played (I think)

game_weights <- weights_dc(allsv181920$Date, xi=0.003)

ggplot(data.table(Date = allsv181920$Date, Weights = game_weights), aes(x = Date, y = Weights)) + geom_line()

gm20 = goalmodel(goals1 = allsv181920$homeGoals, goals2 = allsv181920$awayGoals,
                 team1 = allsv181920$homeTeam, team2 = allsv181920$awayTeam,
                 x1 = as.matrix(allsv181920[, .(homeShotsInBox, homeBigChances, homeTouchesInBox)]),
                 x2 = as.matrix(allsv181920[, .(awayShotsInBox, awayBigChances, awayTouchesInBox)]),
                 fixed_params = list("intercept" = 0), model = "poisson", weights = game_weights)
summary(gm20)

gm20 = goalmodel(goals1 = allsv181920$homeGoals, goals2 = allsv181920$awayGoals,
                 team1 = allsv181920$homeTeam, team2 = allsv181920$awayTeam,
                 x1 = as.matrix(allsv181920[, .(homeShotsInBox, homeBigChances, homeTouchesInBox)]),
                 x2 = as.matrix(allsv181920[, .(awayShotsInBox, awayBigChances, awayTouchesInBox)]),
                 fixed_params = gm20$parameters, model = "poisson", dc = TRUE)
summary(gm20)

gm20$parameters$hfa = 0.2

matchStatLong = melt(allsv181920, measure.vars = 1:2)
matchUpPred <- function(Team1, Team2){
  team1home = matchStatLong[value == Team1 & variable == "homeTeam",
                            .(ShotsInBox = sum(homeShotsInBox * Weight), BigChances = sum(homeBigChances * Weight), TouchesInBox = sum(homeTouchesInBox * Weight))]
  team1away = matchStatLong[value == Team1 & variable == "awayTeam",
                            .(ShotsInBox = sum(awayShotsInBox * Weight), BigChances = sum(awayBigChances * Weight), TouchesInBox = sum(awayTouchesInBox * Weight))]
  team1_data = rbind(team1home, team1away)
  team2home = matchStatLong[value == Team2 & variable == "homeTeam",
                            .(ShotsInBox = sum(homeShotsInBox * Weight), BigChances = sum(homeBigChances * Weight), TouchesInBox = sum(homeTouchesInBox * Weight))]
  team2away = matchStatLong[value == Team2 & variable == "awayTeam",
                            .(ShotsInBox = sum(awayShotsInBox * Weight), BigChances = sum(awayBigChances * Weight), TouchesInBox = sum(awayTouchesInBox * Weight))]
  team2_data = rbind(team2home, team2away)
  
  return(predict_result(model_fit = gm20, team1 = Team1, team2 = Team2,
                        x1 = as.matrix(team1_data[, .(mean(ShotsInBox), mean(BigChances), mean(TouchesInBox))]),
                        x2 = as.matrix(team2_data[, .(mean(ShotsInBox), mean(BigChances), mean(TouchesInBox))])))
  
}

# Iterating through all rounds 
Odds[, Round := allsv181920[!is.na(Round), Round]]

Rounds = as.vector(allsv181920[, na.omit(unique(Round))])
Rounds = Rounds[Rounds > 1]
Predictions = list()
HFA = as.numeric()
AttMat = matrix(data = 0, ncol = max(Rounds)-1, nrow = allsv181920[, length(unique(homeTeam))])
DefMat = matrix(data = 0, ncol = max(Rounds)-1, nrow = allsv181920[, length(unique(homeTeam))])

Weights = as.numeric()
WeightsTeam = as.numeric()

for(r in Rounds){
  tempData = allsv181920[is.na(Round) | Round < (r)]
  game_weights <- weights_dc(tempData$Date, xi=0.003)
  round_date = tempData[!is.na(Round) & Round > 1, max(Date, na.rm = TRUE), Round][, V1]
  
  gm20 = goalmodel(goals1 = tempData$homeGoals, goals2 = tempData$awayGoals,
                   team1 = tempData$homeTeam, team2 = tempData$awayTeam,
                   x1 = as.matrix(tempData[, .(homeShotsInBox, homeBigChances, homeTouchesInBox)]),
                   x2 = as.matrix(tempData[, .(awayShotsInBox, awayBigChances, awayTouchesInBox)]),
                   fixed_params = list("intercept" = 0), model = "poisson", weights = game_weights)

  gm20 = goalmodel(goals1 = tempData$homeGoals, goals2 = tempData$awayGoals,
                   team1 = tempData$homeTeam, team2 = tempData$awayTeam,
                   x1 = as.matrix(tempData[, .(homeShotsInBox, homeBigChances, homeTouchesInBox)]),
                   x2 = as.matrix(tempData[, .(awayShotsInBox, awayBigChances, awayTouchesInBox)]),
                   fixed_params = gm20$parameters, model = "poisson", dc = TRUE)
  
  Names =  gm20$all_teams
  HFA = c(HFA, gm20$parameters$hfa)
  AttMat[, (r-1)] = gm20$parameters$attack
  DefMat[, (r-1)] = gm20$parameters$defense
  
  if(r > 2){
    Weights = c(Weights[1], weights_dc(round_date, 0.003))
    WeightsTeam = c(WeightsTeam[1], weights_dc(round_date, 0.003))
    gm20$parameters$attack = apply(X = AttMat[, 1:(r-1)], MARGIN = 1, FUN = weighted.mean, w = WeightsTeam)
    names(gm20$parameters$attack) = Names
    gm20$parameters$defense = apply(X = DefMat[, 1:(r-1)], MARGIN = 1, FUN = weighted.mean, w = WeightsTeam)
    names(gm20$parameters$defense) = Names
  }else{
    Weights = c(Weights, 0.35)
    WeightsTeam = c(WeightsTeam, 0.50)
  }
  print(weighted.mean(HFA, Weights))
  gm20$parameters$hfa = weighted.mean(HFA, Weights)

  gm20$parameters$hfa = 0.15
  # print(gm20$parameters$hfa)
  
  matchStatLong = melt(tempData, measure.vars = 1:2)
  matchStatLong[, Weight := weights_dc(matchStatLong$Date, xi=0.003)]
  
  if(r <= 10){
    gamesToPred = 
      allsv181920[Round == r & homeTeam %in% c("Varbergs BoIS", "Mjällby AIF") == FALSE & awayTeam %in% c("Varbergs BoIS", "Mjällby AIF") == FALSE]
  }else{
    gamesToPred = allsv181920[Round == r]
  }
  

  for(g in 1:gamesToPred[, .N]){
    gamesToPred[g, c("homeP", "drawP", "awayP") := data.table(matchUpPred(Team1 = gamesToPred[g, homeTeam], Team2 = gamesToPred[g, awayTeam]))]    
  }
  
  if(r <= 10){
    oddsRM = allsv181920[Round == r][, which(homeTeam %in% c("Varbergs BoIS", "Mjällby AIF") | awayTeam %in% c("Varbergs BoIS", "Mjällby AIF"))]
    gamesToPred = cbind(gamesToPred, Odds[Round == r][-oddsRM, 1:6])
  }else{
    gamesToPred = cbind(gamesToPred, Odds[Round == r][, 1:6])
  }
  
  Predictions[[r]] = gamesToPred
  
}
Predictions = rbindlist(Predictions)

Predictions[, c("homeOpen", "drawOpen", "awayOpen") := 
             data.table(implied_probabilities(odds = Predictions[, .(homeOpen, drawOpen, awayOpen)], method = "shin")$probabilities)]

Predictions[, c("homeClose", "drawClose", "awayClose") := 
             data.table(implied_probabilities(odds = Predictions[, .(homeClose, drawClose, awayClose)], method = "shin")$probabilities)]

outcome = outcome[-(1:6),]
# Score calculations
ignScores = as.numeric()
ignScoresOpen = as.numeric()
ignScoresClose = as.numeric()

for(i in 1:Predictions[, .N]){
  Ind = which(outcome[i,] == 1) + 16
  Ind2 = which(outcome[i,] == 1) + 3 + 16
  Ind3 = which(outcome[i,] == 1) + 13
  ignScoresOpen[i] = as.numeric(-log(Predictions[i, ..Ind], base = 2))
  ignScoresClose[i] = as.numeric(-log(Predictions[i, ..Ind2], base = 2))
  ignScores[i] = as.numeric(-log(Predictions[i, ..Ind3], base = 2))
}

mean(ignScores)
mean(ignScoresOpen)
mean(ignScoresClose)

Odds2 = Odds[-which(VarMaif == 1)]
Odds2 = Odds2[-(1:6)]

modelRes = list()
for(i in 1:Predictions[, .N]){
  Game = GamesToPred[[(i + 6)]]
  modelRes[[i]] = data.table(Game, (Predictions[i, .(homeP, drawP, awayP)]), (Predictions[i, .(homeOpen, drawOpen, awayOpen)]),
                             ignScores = ignScores[i], ignScoresOpen = ignScoresOpen[i], ignScoresClose = ignScoresClose[i])
  
}
modelRes = rbindlist(modelRes)
modelRes[, Edge := ignScores < ignScoresOpen]

GameBet = data.table(Game = as.numeric(), Sign = as.numeric(), Outcome = as.numeric(), Odds = as.numeric(), OddsP = as.numeric(), Pred = as.numeric())
for(i in 1:modelRes[, .N]){
  if(modelRes[i, sum(c(homeP, drawP, awayP) > c(homeOpen, drawOpen, awayOpen))] > 0){
    GameBet = rbind(GameBet,
                    data.table(Game = i, Sign = modelRes[i, which(c(homeP, drawP, awayP) > c(homeOpen, drawOpen, awayOpen))],
                               Outcome = which(outcome[i, ] == 1),
                               Odds = as.numeric(Odds2[i, modelRes[i, which(c(homeP, drawP, awayP) > c(homeOpen, drawOpen, awayOpen))], with = FALSE]),
                               OddsP = as.numeric(modelRes[i, modelRes[i, which(c(homeP, drawP, awayP) > c(homeOpen, drawOpen, awayOpen))] + 4, with = FALSE]),
                               Pred = as.numeric(Predictions[i, modelRes[i, which(c(homeP, drawP, awayP) > c(homeOpen, drawOpen, awayOpen))] + 13, with = FALSE])
                               ))
  }
}

GameBet[, Value := Pred - OddsP]
# Add prediction to GameBet

GameBet[, truePred := Sign == Outcome]
GameBet[, Stake := 50]
GameBet[truePred == FALSE, Res := -Stake]
GameBet[truePred == TRUE, Res := (Stake * Odds) - Stake]
GameBet[, (sum(Res) + sum(Stake)) / sum(Stake)] # Flat ROI

GameBet[, Stake2 := 150 * Pred]
GameBet[truePred == FALSE, Res2 := - Stake2]
GameBet[truePred == TRUE, Res2 := (Stake2 * Odds) - Stake2]
GameBet[, (sum(Res2) + sum(Stake2)) / sum(Stake2)] # Weighted ROI

GameBet[, Stake3 := 50]
GameBet[truePred == FALSE & Value > 0.02 , Res3 := -Stake]
GameBet[truePred == TRUE & Value > 0.02, Res3 := (Stake * Odds) - Stake]
GameBet[ Value <= 0.02, Res3 := 0]
GameBet[ Value <= 0.02, Stake3 := 0]
GameBet[, (sum(Res3) + sum(Stake3)) / sum(Stake3)] # Flat ROI

GameBet[, .(sum(Res), sum(Res2), sum(Res3))]
GameBet[, .(sum(Stake), sum(Stake2), sum(Stake3))]

ggplot(melt(GameBet[Game > 8, .(Res = cumsum(Res), Res2 = cumsum(Res2), Res3 = cumsum(Res3), 1:.N)], id.vars = "V4"),
       aes(x = V4, y = value, col = variable)) + geom_line() +
  geom_smooth(se = FALSE)

