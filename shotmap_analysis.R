library(data.table)
library(ggplot2)
library(viridis)
library(gridExtra)
library(ggridges)


load("Allsv23Shotmaps.rda")
load("Allsv23Games.rda")
load("Allsv23PlayerData.rda")

load("Allsv23Heatmap.rda")
hmTable23 = hmTable

load("xg_mod.rda")

squadAge_Theme = theme(panel.background = element_rect(fill = "grey30", color = "lemonchiffon3"),
                       plot.background = element_rect(fill = "grey30", color = "grey30"),
                       text = element_text(color = "lemonchiffon3"),
                       axis.text.y = element_text(color = "lemonchiffon3", size = 12), axis.ticks.y = element_blank(), 
                       axis.text.x = element_text(color = "lemonchiffon3", size = 12), panel.grid = element_blank(),
                       legend.position = "bottom", legend.background = element_rect(fill = "grey30"), legend.text = element_text(size = 12),
                       legend.key = element_rect(fill = "grey30"),
                       plot.title = element_text(size = 20), plot.subtitle = element_text(size = 16),
                       strip.background = element_rect(fill = "grey30"), strip.text = element_text(size = 14, color = "lemonchiffon3"))

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
    
    homeGK = data.table(dataPlayer[[i]]$home$players)[player.position == "G" & !is.na(statistics.minutesPlayed)]
    awayGK = data.table(dataPlayer[[i]]$away$players)[player.position == "G" & !is.na(statistics.minutesPlayed)]
    
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

shotMapDT23 = shotMap_w_xg(data = Allsv23Shotmaps, dataPlayer = Allsv23, games = Games23)

shotMapDT23 = shotMapDT23[(goalType == "own") == FALSE | is.na(goalType) ]

shotMapDT23[player.shortName == 'A. Carlen', player.shortName := "A. Carlén"]
shotMapDT23[player.shortName == 'T. Ali', player.shortName := "T. A. Ali"]
shotMapDT23[player.shortName == 'E. Júnior', player.shortName := "E. Junior"]
shotMapDT23[player.shortName == 'N. Dahlstrom', player.shortName := "N. Dahlström"]
shotMapDT23[player.shortName == 'R. Sjostedt', player.shortName := "R. Sjöstedt"]

shotMapDT23[, playerTeam := tail(Team, 1), player.shortName]


shots_summary = shotMapDT23[situation2 != "penalty", .(.N, Goals = sum(Goal), xG = sum(xG),
                                                       distCenter = sum(distCenter), angleGoal = sum(angleGoal)),
                            .(player.shortName, player.position, playerTeam)][order(player.shortName, -N)]
shots_summary[, position := head(player.position, 1), player.shortName]
shots_summary[, position := zoo::na.locf.default(position, na.rm = FALSE)]

shots_summary = shots_summary[N > 2, .(N = sum(N), Goals = sum(Goals), xG = sum(xG),
                                       distCenter = sum(distCenter) / sum(N), angleGoal = sum(angleGoal) / sum(N)),
                              .(player.shortName, position, playerTeam)][N > 15]

shots_type = shotMapDT23[player.shortName %in% shots_summary[, player.shortName] & bodyPart2 != "other",
                         .N, .(player.shortName, playerTeam, bodyPart2)]
shots_type[, P := N / sum(N), player.shortName]

shots_summary = merge(shots_summary, 
                      dcast(shots_type, player.shortName + playerTeam ~ bodyPart2, value.var = "P", fill = 0),
                      by = c("player.shortName", "playerTeam"))


# Plot: Type of shot
plot_type = melt(shots_summary[position == "F"], measure.vars = c("foot", "head"))
ggplot(plot_type, aes(x = variable, y = value)) +
  stat_summary(geom = "bar", fun = "mean", fill = "grey74") +
  theme_bw() + 
  ylim(c(0,1)) +
  geom_bar(data = plot_type[player.shortName == "C. Nyman"],
           stat = "identity", alpha = .5, color = "grey22", size = 1, fill = "indianred3")

mround = function(x, base){
  res = base * round(x/base)
  return(res)
}

# Plot: Dist to goal
ggplot(shots_summary[position == "F",.(distCenter = mround(distCenter, .75))][, .(ymin = (.N / 2) * -1, ymax = .N / 2), distCenter],
       aes(x = distCenter, xend = distCenter, y = ymin, yend = ymax)) +
  geom_segment(size = 7.5, color = "grey64") +
  theme_bw() + 
  geom_point(data = shots_summary[player.shortName == "C. Nyman"],
             aes(x = distCenter, y = 0), size = 10, shape = 21, stroke = 2, alpha = .75, fill = "red3", inherit.aes = FALSE)

# Plot: Angle to goal
ggplot(shots_summary[position == "F",.(angleGoal = mround(angleGoal, .025))][, .(ymin = (.N / 2) * -1, ymax = .N / 2), angleGoal],
       aes(x = angleGoal, xend = angleGoal, y = ymin, yend = ymax)) +
  geom_segment(size = 5.5, color = "grey64") +
  theme_bw() + 
  geom_point(data = shots_summary[player.shortName == "C. Nyman"],
             aes(x = angleGoal, y = 0), size = 10, shape = 21, stroke = 2, alpha = .75, fill = "red3", inherit.aes = FALSE)


# Plot: Shot map

ggplot(shotMapDT23[situation2 != "penalty", .N, .(x2 = mround(x2, 1.75), y2 = mround(y2, 1.7))][x2 > 0],
       aes(x = 105 - x2, y = 68 - y2, fill = N)) +
  geom_tile() + 
  annotate("rect", xmin = 105, xmax = 105 - 16.5,
           ymin = 34 - 17.7, ymax = 34 + 17.7, fill = "transparent", col = "black", size = 1) +
  annotate("rect", xmin = 105, xmax = 105 - 5.5,
           ymin = 34 - 6.7, ymax = 34 + 6.7, fill = "transparent", col = "black", size = 1) +
  annotate("rect", xmin = 105, xmax = 105,
           ymin = 30.34, ymax = 37.66, fill = "transparent", col = "black", size = 1.5) +
  annotate("rect", xmin = 105, xmax = 52.5, ymin = 0, ymax = 68, fill = "transparent", col = "black") +
  annotate("point", x = c(94), y = c(34), col = "black", size = 2) + 
  geom_curve(aes(y = 27.2, x = 105 - 16.5, yend = 40.8, xend = 105 - 16.5),curvature = 0.58, size = 1,
             colour = "black", inherit.aes = FALSE)+ 
  
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72"),
        panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black")) +
  
  coord_flip(xlim = c(75, 105), ylim = c(15, 53)) +
  
  scale_fill_gradient(low = "grey84", high = "grey22") +

  geom_point(data = shotMapDT23[situation2 != "penalty" & player.shortName == "S. Nanasi"],
             alpha = .75, size = 4, shape = 21, stroke = 2,
             fill = "indianred3")

# Plot: Shots per 90

load("minPlayer.rda")

minPlayer[, totMin := sum(Min), .(Player, Age)]

shots_summary = merge(shots_summary, minPlayer[, .(player.shortName = Player ,playerTeam = Team, totMin)], by = c("player.shortName", "playerTeam"))

shots_summary[, shots90 := round(N / (totMin / 90), 3)]
shots_summary[, xGshot := xG / N]


ggplot(shots_summary[position == "F",.(shots90 = mround(shots90, .25))][, .(ymin = (.N / 2) * -1, ymax = .N / 2), shots90],
       aes(x = shots90, xend = shots90, y = ymin, yend = ymax)) +
  geom_segment(size = 10, color = "grey64") +
  theme_bw() + 
  geom_point(data = shots_summary[player.shortName == "C. Nyman"],
             aes(x = shots90, y = 0), size = 10, shape = 21, stroke = 2, alpha = .75, fill = "red3", inherit.aes = FALSE)


# 
p1 = ggplot(shots_summary[position == "F"],
       aes(x = shots90, y = 0)) +
  geom_density_ridges(fill = "lemonchiffon3") +
  squadAge_Theme + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  geom_point(data = shots_summary[player.shortName == "C. Nyman"],
             aes(x = shots90, y = 0), size = 10, shape = 21, stroke = 2, alpha = .75, fill = "red3", inherit.aes = FALSE) +
  labs(title = "Shots per 90", x = "Count")

p2 = ggplot(shots_summary[position == "F"],
       aes(x = distCenter, y = 0)) +
  geom_density_ridges(fill = "lemonchiffon3") +
  squadAge_Theme + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  geom_point(data = shots_summary[player.shortName == "C. Nyman"],
             aes(x = distCenter, y = 0), size = 10, shape = 21, stroke = 2, alpha = .75, fill = "red3", inherit.aes = FALSE) +
  labs(title = "Distance to Goal", x = "Metres")

plot_type = melt(shots_summary[position == "F"], measure.vars = c("foot", "head"), )
p3 = ggplot(plot_type, aes(x = variable, y = value * 100)) +
  stat_summary(geom = "bar", fun = "mean", fill = "grey74") +
  squadAge_Theme + 
  coord_flip(ylim = c(0,100), xlim = c(1, 2)) +
  scale_x_discrete(breaks = c("head", "foot"), labels = c("Head", "Foot")) + 
  geom_bar(data = plot_type[player.shortName == "C. Nyman"],
           stat = "identity", alpha = .75, color = "grey22", size = 1, fill = "indianred3") +
  labs(title = "Type of Shot", y = "%", x = NULL)

grid.arrange(p1, p2, p3, ncol = 1)


gk_summary23 = shotMapDT23[situation2 != "penalty", .(.N, Goals = sum(Goal), xG = sum(xG),
                                                       distCenter = mean(distCenter), angleGoal = mean(angleGoal)),
                            .(Goalkeeper, Team = Opponent)]

gk_data23 = shotMapDT23[situation2 != "penalty", .(Goalkeeper, game, Team = Opponent, x2, y2, distCenter, angleGoal, situation2,
                bodyPart2, xG, Goal)]

gk_data23[, Goalkeeper2 := factor(Goalkeeper, levels = 
                                    gk_data23[Goal == TRUE & situation2 != "set-piece",
                                         .(distCenter = median(distCenter), .N), Goalkeeper][order(distCenter), Goalkeeper]
                                 )]

ggplot(gk_data23[Goal == TRUE & Goalkeeper %in% gk_summary[Goals > 10, Goalkeeper] & situation2 != "set-piece"],
       aes(x = distCenter, y = Goalkeeper2, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1) +
  scale_fill_viridis(direction = -1, guide = "none")  +
  squadAge_Theme +
  xlim(c(0, 40)) 


gk_data23[, Goalkeeper2 := factor(Goalkeeper, levels = 
                                    gk_data23[Goal == TRUE & situation2 != "set-piece",
                                          .(xG = median(xG), .N), Goalkeeper][order(xG), Goalkeeper]
)]

ggplot(gk_data23[Goal == TRUE & Goalkeeper %in% gk_summary[Goals > 10, Goalkeeper] & situation2 != "set-piece"],
       aes(x = xG, y = Goalkeeper2, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1) +
  scale_fill_viridis(direction = -1, guide = "none")  +
  squadAge_Theme 





save(shots_summary, file = "shots_summary.rda")
save(shotMapDT23, file = "shotMapDT23.rda")
save(gk_data23, file = "gk_data23.rda")
save(gk_summary23, file = "gk_summary23.rda")



