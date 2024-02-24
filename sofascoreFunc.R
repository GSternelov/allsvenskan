# Functions and themes to plot sofa score data

sf_theme = theme(legend.position = "bottom", legend.key.width = unit(5, "cm"), 
                  panel.background = element_rect(fill = "#567d46"), axis.text = element_blank(),
                  axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
                  strip.text = element_text(color = "black"), axis.title = element_blank(), panel.grid = element_blank())

sf_theme2 = theme(legend.position = "bottom", legend.key.width = unit(5, "cm"), 
                 panel.background = element_rect(fill = "white"), axis.text = element_blank(),
                 axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
                 strip.text = element_text(color = "black"), axis.title = element_blank(), panel.grid = element_blank())

  # Select starting eleven / 11 players that played maority of the game

playSel <- function(GameInd, ha, allsvData, Games){
  if(ha == "home"){
    gameInfo = data.table(Position = allsvData[[GameInd]]$home$players$position,
                          Sub = allsvData[[GameInd]]$home$players$substitute,
                          Min = allsvData[[GameInd]]$home$players$statistics$minutesPlayed,
                          Player = allsvData[[GameInd]]$home$players$player$name,
                          Team = strsplit(Games[[GameInd]], split = " - ")[[1]][1],
                          Opponent = strsplit(Games[[GameInd]], split = " - ")[[1]][2])
  }else{
    gameInfo = data.table(Position = allsvData[[GameInd]]$away$players$position,
                          Sub = allsvData[[GameInd]]$away$players$substitute,
                          Min = allsvData[[GameInd]]$away$players$statistics$minutesPlayed,
                          Player = allsvData[[GameInd]]$away$players$player$name,
                          Team = strsplit(Games[[GameInd]], split = " - ")[[1]][2],
                          Opponent = strsplit(Games[[GameInd]], split = " - ")[[1]][1])
  }

  gameInfo = gameInfo[Min >= 45]
  if(gameInfo[, .N] > 11){
    gameInfo = gameInfo[Sub == FALSE]
  }
  
  return(gameInfo)
}


# Test that url works
url_works <- function(url){
  tryCatch(html_table(read_html(url)),
           error = function(e) {NA})
}


# Plot heatmaps for one game
gameHeatmap <- function(ind, allsvData, Games){
  gameInfo = playSel(ind, "home", allsvData = allsvData, Games = Games)
  
  gameHome = merge(gameInfo, hmTable, by = c("Player", "Team", "Opponent", "Min"))
  gameHome[, Position := factor(Position, levels = c("G", "D", "M", "F"))]
  gameHome[, Player := factor(Player, levels = 
                                gameHome[, .(Position = unique(Position), y = mean(y)), Player][order(Position, y), Player])]
  
  gameInfo = playSel(ind, "away", allsvData = allsvData, Games = Games)
  
  gameAway = merge(gameInfo, hmTable, by = c("Player", "Team", "Opponent", "Min"))
  gameAway[, Position := factor(Position, levels = c("G", "D", "M", "F"))]
  gameAway[, Player := factor(Player, levels = 
                                gameAway[, .(Position = unique(Position), y = mean(y)), Player][order(Position, y), Player])]
  
  P1 = ggplot(gameHome[Position != "G"], aes(x = x, y = y)) +   
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
    stat_density_2d(geom = "polygon", aes( fill = ..level.., alpha = ..level..), bins = 15) +
    scale_fill_viridis(option = "B", end = 0.98, begin = 0.1,
                       na.value = "yellow2", guide = FALSE, limits = c(0, 0.0006)) +
    xlim(c(0, 100)) + ylim(c(0, 100)) + scale_alpha(guide = FALSE, range = c(0.4, 1)) + facet_wrap(~Player, nrow = 2) +
    sf_theme
  
  P2 = ggplot(gameAway[Position != "G"], aes(x = x, y = y)) +  
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
    stat_density_2d(geom = "polygon", aes( fill = ..level.., alpha = ..level..), bins = 15) +
    scale_fill_viridis(option = "B", end = 0.98, begin = 0.1,
                       na.value = "yellow2", guide = FALSE, limits = c(0, 0.0006)) +
    xlim(c(0, 100)) + ylim(c(0, 100)) + scale_alpha(guide = FALSE, range = c(0.4, 1)) + facet_wrap(~Player, nrow = 2) +
    sf_theme
  
  gridExtra::grid.arrange(P1, P2)
}


# Plot grid maps for one game
gameGridmap <- function(team1, team2, quantLim){
  HomeGrid <- hmTable[Player %in% goalKeepers == FALSE & Team == team1 & Opponent == team2,
                      .(count = .N, xmin = min(xMin), xmax = max(xMax), ymin = min(yMin), ymax = max(yMax) ),
                      .(Grid, Team)]
  HomeGrid = merge(GridMean,
                   HomeGrid,
                   by = c("Grid", "xmin", "xmax", "ymin", "ymax"), allow.cartesian = TRUE)
  
  HomeGrid[is.na(count.y), count.y := 0]
  HomeGrid[, Prop.Team := count.y / sum(count.y)]
  HomeGrid[, PropDiff := Prop.Team - Prop]
  
  AwayGrid <- hmTable[Player %in% goalKeepers == FALSE & Team == team2 & Opponent == team1,
                      .(count = .N, xmin = min(xMin), xmax = max(xMax), ymin = min(yMin), ymax = max(yMax) ),
                      .(Grid, Team)]
  AwayGrid = merge(GridMean,
                   AwayGrid,
                   by = c("Grid", "xmin", "xmax", "ymin", "ymax"), allow.cartesian = TRUE)
  
  AwayGrid[is.na(count.y), count.y := 0]
  AwayGrid[, Prop.Team := count.y / sum(count.y)]
  AwayGrid[, PropDiff := Prop.Team - Prop]
  
  TeamGrid = rbind(HomeGrid, AwayGrid)
  
  TeamGrid[, PropDiff2 := PropDiff]
  TeamGrid[PropDiff < quantLim[1], PropDiff2 :=  quantLim[1]]
  TeamGrid[PropDiff > quantLim[2], PropDiff2 :=  quantLim[2]]
  
  ggplot(TeamGrid,
         aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = PropDiff2)) + geom_rect() +  
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
    annotate("path", x=50+5.15*cos(seq(0,2*pi,length.out=100)),
             y=50+9.15*sin(seq(0,2*pi,length.out=100))) +
    annotate("point", x = c(11, 50, 89), y = c(50, 50 ,50), size = 1) + 
    scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0, 
                         limits = quantLim, breaks = c(-0.01, 0.01), name = NULL,
                         labels = c("Less than\naverage", "More than\naverage")) +
    facet_wrap(~Team, nrow = 2) +
    sf_theme2 + theme(legend.key.width = unit(2.5, "cm"))
  
}

TeamGridmap <- function(team, quantLim){
  GamesGrid <- hmTable[Player %in% goalKeepers == FALSE & Team == team,
                      .(count = .N, xmin = min(xMin), xmax = max(xMax), ymin = min(yMin), ymax = max(yMax) ),
                      .(Grid, Opponent)]
  GamesGrid = merge(GridMean,
                    GamesGrid,
                    by = c("Grid", "xmin", "xmax", "ymin", "ymax"), allow.cartesian = TRUE)
  
  GamesGrid[is.na(count.y), count.y := 0]
  GamesGrid[, Prop.Game := count.y / sum(count.y), Opponent]
  GamesGrid[, PropDiff := Prop.Game - Prop]
  
  GamesGrid[, PropDiff2 := PropDiff]
  GamesGrid[PropDiff < quantLim[1], PropDiff2 :=  quantLim[1]]
  GamesGrid[PropDiff > quantLim[2], PropDiff2 :=  quantLim[2]]
  
  ggplot(GamesGrid,
         aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = PropDiff2)) + geom_rect() +  
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
    annotate("path", x=50+5.15*cos(seq(0,2*pi,length.out=100)),
             y=50+9.15*sin(seq(0,2*pi,length.out=100))) +
    annotate("point", x = c(11, 50, 89), y = c(50, 50 ,50), size = 1) + 
    scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0) +
    facet_wrap(~Opponent, nrow = 2) +
    sf_theme2 + theme(legend.key.width = unit(2.5, "cm"))
  
}



# Plot heatmaps for one player
playerHeatmap <- function(Name){

  ggplot(hmTable[grepl(Name, x = Player)], aes(x = x, y = y)) +  
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
    stat_density_2d(geom = "polygon", aes( fill = ..level.., alpha = ..level..), bins = 15) +
    scale_fill_viridis(option = "B", end = 0.98, begin = 0.1, limits = c(0, 0.00028), na.value = "yellow2", guide = FALSE) +
    scale_alpha(guide = FALSE) + xlim(c(0, 100)) + ylim(c(0, 100)) +
    sf_theme +
    facet_wrap(~Opponent)
  
}


# Plot grid maps for one player
playerGridmap <- function(Name, quantLim){
  playerGrid <- hmTable[grepl(Name, x = Player),
                        .(count = .N ),
                        .(Grid, Player, Team, Opponent)]
  
  # Must do the join operation for each game separately
  tempList = list()
  k = 0
  for(i in playerGrid[, unique(Opponent)]){
    k = k+1
    temp = data.table(dplyr::full_join(GridMean, playerGrid[Opponent == i], by = "Grid"))
    temp[is.na(count.y), count.y := 0]
    temp[is.na(Player), Player := playerGrid[, unique(Player)]]
    temp[is.na(Team), Team := playerGrid[, unique(Team)]]
    temp[is.na(Opponent), Opponent := i]
    tempList[[k]] = temp
  }
  playerGrid = rbindlist(tempList)
  
  playerGrid[, countDiff := count.y - count.x]
  
  playerGrid[, countDiff2 := countDiff]
  playerGrid[countDiff < quantLim[1], countDiff2 :=  quantLim[1]]
  playerGrid[countDiff > quantLim[2], countDiff2 :=  quantLim[2]]
  
  playerGrid[, Opponent := factor(Opponent, levels = unique(Opponent))]
  
  ggplot(playerGrid,
         aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = countDiff2)) + geom_rect() +  
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
    scale_fill_gradient2(low = "grey32", mid = "grey86", high = "red3", midpoint = 0, 
                         limits = quantLim) +
    facet_wrap(~Opponent)
}




days_since_last_match <- function(team1, team2, dates, first_val = NA){
  
  stopifnot(length(team1) == length(team2),
            length(team2) == length(dates),
            length(team1) >= 1,
            length(first_val) == 1)
  
  dates <- as.Date(dates)
  ngames <- length(team1)
  all_teams <- sort(unique(c(unique(team1), unique(team2))), decreasing = FALSE)
  
  res <- matrix(NA, ncol=2, nrow=ngames)
  
  for (ii in 1:length(all_teams)){
    
    team_idx1 <- team1 == all_teams[ii]
    team_idx2 <- team2 == all_teams[ii]
    
    tidx <- as.numeric(team_idx1)
    tidx[team_idx2] <- 2
    
    team_idx <- which(team_idx1 | team_idx2)
    
    tdates <- dates[team_idx]
    
    for (tt in 1:length(tdates)){
      if (tdates[tt] == min(tdates)){
        tmp_val <- first_val
      } else {
        tmp_val <- tdates[tt] - max(tdates[tdates < tdates[tt]])
      }
      
      res[team_idx[tt], tidx[team_idx[tt]]] <- tmp_val
      
    }
  }
  
  return(res)
  
}







