# helper functions for shiny app

sm_aes = function(data, smPlot, playerName, sColor, sShape, sSize){
  # Number of aes to set
  conut_aes = sum(c(sColor != "", sShape != "", sSize != "No"))
  
  if(sColor != "") colorVar = sColor
  if(sShape != "") shapeVar = sShape
  
  if(conut_aes == 0){
    p = smPlot + 
      geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                 alpha = .75, size = 5, shape = 21, stroke = 2, fill = "indianred3")
  }else if(conut_aes == 1){
    if(sColor != ""){
      p = smPlot + 
        geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                   alpha = .75, size = 5, aes(x = 105 - x2, y = 68 - y2, color = .data[[sColor]]), inherit.aes = FALSE) +
        scale_color_viridis(discrete = TRUE, end = .9)
    }else if(sShape != ""){
      p = smPlot + 
        geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                   alpha = .75, size = 5, fill = "indianred3", stroke = 2,
                   aes(x = 105 - x2, y = 68 - y2, shape = .data[[sShape]]), inherit.aes = FALSE) 
      }else{
        p = smPlot + 
          geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                     alpha = .75, shape = 21, stroke = 2, fill = "indianred3",
                     aes(x = 105 - x2, y = 68 - y2, size = xG), inherit.aes = FALSE)+
          scale_size(range = c(0.5, 10), limits = c(0,1))
    }
  }else if(conut_aes == 2){
    if(sColor == ""){
      p = smPlot +
        geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                   alpha = .75, stroke = 2, fill = "indianred3",
                   aes(x = 105 - x2, y = 68 - y2, shape = .data[[sShape]], size = xG), inherit.aes = FALSE) +
        scale_size(range = c(0.5, 10), limits = c(0,1))
      }else if(sShape == ""){
        p = smPlot + 
          geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                     alpha = .75, stroke = 2, shape = 21, 
                     aes(x = 105 - x2, y = 68 - y2, color = .data[[sColor]], size = xG), inherit.aes = FALSE) +
          scale_size(range = c(0.5, 10), limits = c(0,1)) +
          scale_color_viridis(discrete = TRUE, end = .9)
    }else{
      p = smPlot + 
        geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                   alpha = .75, stroke = 2, size = 5, 
                   aes(x = 105 - x2, y = 68 - y2, color = .data[[sColor]], shape = .data[[sShape]]), inherit.aes = FALSE) +
        scale_color_viridis(discrete = TRUE, end = .9)
    }
  }else if(conut_aes == 3){
    p = smPlot + 
      geom_point(data = data[situation2 != "penalty" & player.shortName == playerName],
                 alpha = .75, stroke = 2, 
                 aes(x = 105 - x2, y = 68 - y2, color = .data[[sColor]], shape = .data[[sShape]], size = xG), inherit.aes = FALSE) +
      scale_size(range = c(0.5, 10), limits = c(0,1)) +
      scale_color_viridis(discrete = TRUE, end = .9)
  }
  
  return(p)
}
