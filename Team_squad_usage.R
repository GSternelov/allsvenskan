library(viridis)
library(ggridges)
library(data.table)
library(ggplot2)

### Sofa score data ###
  # Team squad usage analysis

load("Allsv23PlayerData.rda")
load("Allsv23Games.rda")
load("GamesDate23.rda")

squadData = list()
k = 0
g = 1
for(i in 1:length(Games23)){
  k = k+1
  
  tempDT = data.table()
  # Name
  tempDT[, Player := Allsv23[[i]]$home$players$player$shortName]
  # Position
  tempDT[, Position := Allsv23[[i]]$home$players$player$position]
  # Date of birth
  tempDT[, dob := as.POSIXct(Allsv23[[i]]$home$players$player$dateOfBirthTimestamp, origin = "1970-01-01", tz = "GMT")]
  # Minutes played
  tempDT[, Min := Allsv23[[i]]$home$players$statistics$minutesPlayed]
  # Goals
  tempDT[, Goals := Allsv23[[i]]$home$players$statistics$goals]
  # Assists
  tempDT[, Assists := Allsv23[[i]]$home$players$statistics$goalAssist]
  # Key passes
  tempDT[, keypass := Allsv23[[i]]$home$players$statistics$keyPass]
  # Created big chances
  tempDT[, createdBigChance := Allsv23[[i]]$home$players$statistics$bigChanceCreated]
  # Team
  tempDT[, Team := strsplit(Games23[[i]], split = " - ")[[1]][1]]
  # Game
  tempDT[, Game := i]
  # Date
  tempDT[, Date := GamesDate[i]]
  
  squadData[[k]] = tempDT
  
  k = k+1
  
  tempDT = data.table()
  # Name
  tempDT[, Player := Allsv23[[i]]$away$players$player$shortName]
  # Position
  tempDT[, Position := Allsv23[[i]]$away$players$player$position]
  # Date of birth
  tempDT[, dob := as.POSIXct(Allsv23[[i]]$away$players$player$dateOfBirthTimestamp, origin = "1970-01-01", tz = "GMT")]
  # Minutes played
  tempDT[, Min := Allsv23[[i]]$away$players$statistics$minutesPlayed]
  # Goals
  tempDT[, Goals := Allsv23[[i]]$away$players$statistics$goals]
  # Assists
  tempDT[, Assists := Allsv23[[i]]$away$players$statistics$goalAssist]
  # Key passes
  tempDT[, keypass := Allsv23[[i]]$away$players$statistics$keyPass]
  # Created big chances
  tempDT[, createdBigChance := Allsv23[[i]]$away$players$statistics$bigChanceCreated]
  # Team
  tempDT[, Team := strsplit(Games23[[i]], split = " - ")[[1]][2]]
  # Game
  tempDT[, Game := g]
  # Date
  tempDT[, Date := GamesDate[i]]
  
  squadData[[k]] = tempDT
  
  if((i %% 8) == 0){g = g+1}
}

squadData = rbindlist(squadData, fill = TRUE)
squadData[is.na(Min), Min := 0]
squadData[is.na(keypass), keypass := 0]
squadData[is.na(createdBigChance), createdBigChance := 0]
squadData[is.na(Goals), Goals := 0]
squadData[is.na(Assists), Assists := 0]

squadData[, Age := as.numeric(floor(difftime(as.POSIXct(Date), dob, units = "weeks") / 52))]

squadData[Player == 'A. Carlen', Player := "A. Carlén"]
squadData[Player == 'T. Ali', Player := "T. A. Ali"]
squadData[Player == 'E. Júnior', Player := "E. Junior"]
squadData[Player == 'N. Dahlstrom', Player := "N. Dahlström"]
squadData[Player == 'R. Sjostedt', Player := "R. Sjöstedt"]

squadData[Player == 'O. Pettersson', Age := 24]
squadData[Player == 'V. Tyrén', Age := 18]
squadData[Player == 'V. Granath', Age := 29]

squadData[Age < 23, Group := "< 23"]
squadData[Age >= 23 & Age < 26, Group := ">= 23 & < 26"]
squadData[Age >= 26 & Age < 30, Group := ">= 26 & < 30"]
squadData[Age >= 30, Group := ">= 30"]

peakAge = squadData[!is.na(Age), .(Min = sum(Min)), .(Team, Group)]
peakAge = peakAge[order(Team)]
peakAge[, MinPerc := Min / squadData[order(Team)][!is.na(Age), sum(Min), Team][, rep(V1, each = 4)] ]
peakAge[, Team := factor(Team, levels = peakAge[Group == "< 23"][order(MinPerc), Team])]
peakAge[, Group := factor(Group, levels = c(">= 30", ">= 26 & < 30", ">= 23 & < 26", "< 23"))]


squadAge_Theme = theme(panel.background = element_rect(fill = "grey30", color = "lemonchiffon3"),
                       plot.background = element_rect(fill = "grey30", color = "grey30"),
                       text = element_text(color = "lemonchiffon3"),
                       axis.text.y = element_text(color = "lemonchiffon3", size = 12), axis.ticks.y = element_blank(), 
                       axis.text.x = element_text(color = "lemonchiffon3", size = 12), panel.grid = element_blank(),
                       legend.position = "bottom", legend.background = element_rect(fill = "grey30"), legend.text = element_text(size = 12),
                       legend.key = element_rect(fill = "grey30"),
                       plot.title = element_text(size = 20), plot.subtitle = element_text(size = 16),
                       strip.background = element_rect(fill = "grey30"), strip.text = element_text(size = 14, color = "lemonchiffon3"))

ggplot(peakAge, aes(x = Team, y = MinPerc, fill = Group)) + 
  geom_bar(stat = "identity", position = "stack", color = "grey40") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), name = NULL) +
  scale_fill_viridis(discrete = TRUE, begin = .25, end = .99, name = "Age group", guide = guide_legend(reverse = TRUE)) + 
  squadAge_Theme + 
  labs(x = NULL, title = "Share of minutes played by age group", subtitle = "Allsvenskan 2023, round 1-30")

peakAge[, Group := factor(Group, levels = rev(c(">= 30", ">= 26 & < 30", ">= 23 & < 26", "< 23")))]

peakAge[, Team := factor(Team, levels = sort(levels(Team)))]

# ggplot(peakAge, aes(x = Team, y = MinPerc, fill = Group)) + 
#   geom_bar(stat = "identity", position=position_dodge(1)) + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.8), name = NULL) +
#   scale_fill_viridis(discrete = TRUE, begin = .4, end = .99, name = NULL, option = "D",
#                      guide = guide_legend(reverse = FALSE)) +
#   labs(x = NULL, title = "Share of minutes played by age group",
#        subtitle = "Allsvenskan 2023, round 1-30, players age was recorded for each round") +
#   facet_wrap(~Team, ncol = 4, scales = "free_x") + 
#   squadAge_Theme

peakAgePos = squadData[!is.na(Age), .(Min = sum(Min)), .(Team, Group, Position)]
peakAgePos[, totMin := sum(Min), .(Team, Position)]
peakAgePos[, MinPerc := Min /  totMin]
peakAgePos[, Group := factor(Group, levels = (c(">= 30", ">= 26 & < 30", ">= 23 & < 26", "< 23")))]
peakAgePos[, Position := factor(Position, levels = c("G", "D", "M", "F"))]

peakAgePos = merge(peakAgePos, 
                   expand.grid(Team = peakAgePos[, unique(Team)], 
                               Group = peakAgePos[, unique(Group)], 
                               Position = peakAgePos[, unique(Position)]),
                   by = c("Team", "Group", "Position"), all = TRUE)
peakAgePos[is.na(MinPerc), MinPerc := 0]
peakAgePos[, Team := factor(Team, levels = sort(levels(Team)))]

ggplot(peakAgePos[Position != "G"], aes(x = 1, y = MinPerc, fill = Group)) + 
  geom_bar(stat = "identity", position = "stack", color = "grey40") + 
  coord_flip() + 
  facet_grid(Team~Position, shrink = FALSE) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), name = NULL) +
  scale_fill_viridis(discrete = TRUE, begin = .25, end = .99, name = NULL, option = "D",
                     guide = guide_legend(reverse = TRUE)) +
  labs(x = NULL, title = "Share of minutes played by age group",
       subtitle = "Allsvenskan 2023, round 1-30, players age was recorded for each round") +
  squadAge_Theme +
  theme(strip.text.y = element_text(angle = 0), axis.text.y = element_blank())


squadData[, AgeLast := tail(Age, 1), .(Player, Team)]

minPlayer = squadData[, .(Min = sum(Min)), .(Player, Team, Age = AgeLast)]
minPlayer[, MinPerc := Min / (length(Games23) / 8 * 90)]

ggplot(minPlayer[Min > 0 & Team == "Mjällby AIF"], aes(x = Age, y = MinPerc)) +
  geom_point(size = 3) +
  annotate("rect", xmin = 23.75, xmax = 29.25, ymin = 0, ymax = 1, alpha = .25, fill = "royalblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), name = "Share of minutes played") +
  squadAge_Theme + 
  theme(panel.grid.major = element_line(color = "grey48"), panel.background = element_rect(color = "grey30")) +
  # xlim(c(16, 40)) +
  ggrepel::geom_text_repel(aes(label = Player), size = 3.5, color = "lemonchiffon3") 

ggplot(minPlayer[Min > 0], aes(x = Age, y = MinPerc)) +
  geom_point(size = 3) +
  annotate("rect", xmin = 23.75, xmax = 29.25, ymin = 0, ymax = 1, alpha = .25, fill = "royalblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), name = "Share of minutes played") +
  squadAge_Theme + 
  theme(panel.grid.major = element_line(color = "grey48"), panel.background = element_rect(color = "grey30")) +
  # xlim(c(16, 40)) +
  ggrepel::geom_text_repel(data = minPlayer[Min > 360],
                           aes(label = Player), size = 2, color = "lemonchiffon3")   +
  facet_wrap(~Team)

minPlayer[, sum(MinPerc > 0.8), Team][order(V1)]
minPlayer[Age > 30, sum(MinPerc > 0.5), Team][order(V1)]



distAge = squadData[Min > 0, .(Min = (sum(Min) / (11*90)) * 100), .(Age, Team, Game)][
  , .(rep(Age, Min)), .(Age, Team, Game)]
teamOrder = distAge[, quantile(Age, na.rm = TRUE, 0.25), Team][order(V1), Team]
distAge[, Team := factor(Team, levels = teamOrder)]

ggplot(distAge, aes(x = V1, y = Team, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1.1) +
  scale_fill_viridis(direction = -1) +
  geom_vline(xintercept = c(23.5, 29.5), linetype = "dashed")

distAgePos = squadData[Min > 0 & Position != "G", .(Min = (sum(Min) / (11*90)) * 100), .(Age, Team, Game, Position)][
  , .(rep(Age, Min)), .(Age, Team, Game, Position)]
distAgePos[, Position := factor(Position, levels = c("G", "D", "M", "F"))]
distAgePos[, Team := factor(Team, levels = sort(unique(Team), decreasing = TRUE))]

ggplot(distAgePos, aes(x = V1, y = Team, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1, rel_min_height = .001, size = 1) +
  scale_fill_viridis(direction = 1, guide = "none") +
  geom_vline(xintercept = c(23.5, 29.5), linetype = "dashed")  + 
  facet_grid(.~Position, shrink = FALSE) +
  squadAge_Theme

save(peakAge, file = "peagAge.rda")
save(peakAgePos, file = "peakAgePos.rda")
save(minPlayer, file = "minPlayer.rda")
save(distAge, file = "distAge.rda")
save(distAgePos, file = "distAgePos.rda")







