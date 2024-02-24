library(shiny)
library(data.table)
library(ggplot2)
library(viridis)
library(gridExtra)
library(ggridges)
library(DT)
library(gridExtra)

mround = function(x, base){
  res = base * round(x/base)
  return(res)
}

source("app_helpers.R")

squadAge_Theme = theme(panel.background = element_rect(fill = "grey30", color = "lemonchiffon3"),
                       plot.background = element_rect(fill = "grey30", color = "grey30"),
                       text = element_text(color = "lemonchiffon3"),
                       axis.text.y = element_text(color = "lemonchiffon3", size = 12), axis.ticks.y = element_blank(), 
                       axis.text.x = element_text(color = "lemonchiffon3", size = 12), panel.grid = element_blank(),
                       legend.position = "bottom", legend.background = element_rect(fill = "grey30"), legend.text = element_text(size = 12),
                       legend.key = element_rect(fill = "grey30"),
                       plot.title = element_text(size = 20), plot.subtitle = element_text(size = 16),
                       strip.background = element_rect(fill = "grey30"), strip.text = element_text(size = 14, color = "lemonchiffon3"))

load("peagAge.rda")
load("peakAgePos.rda")
load("minPlayer.rda")
load("distAge.rda")
load("distAgePos.rda")

load("shots_summary.rda")
load("shotMapDT23.rda")

sm_cat = c("", "situation2", "bodyPart2", "xG", "Goal")

ui <- fluidPage(

    titlePanel("Allsvenskan 2023"),
    
    navlistPanel(widths = c(3, 9),
      tabPanel("Shot Maps",
               fluidRow(column(2,
                               selectInput("selPlayer", label = "Select Player",
                                           choices = shots_summary[, paste(player.shortName, ' (', playerTeam, ')', sep = "")])    
                               ),
                        column(2,
                               selectInput("selColor", label = "Color By",
                                           choices = sm_cat[-4])  
                        ),
                        column(2,
                               selectInput("selShape", label = "Set Shape By",
                                           choices = sm_cat[-4])    
                        ),
                        column(2,
                               radioButtons('selSize', label = "Set Size by xG?", choices = c("Yes", "No"), 
                                            selected = "No", inline = TRUE) 
                        )
                        ),
               fluidRow(column(2,
                               selectInput("selCompare", label = "Compare Against",
                                           choices = c("", shots_summary[, paste(player.shortName, ' (', playerTeam, ')', sep = "")]) ))
                        ),
               fluidRow(column(9,
                               plotOutput('shotMap', height = 450)
                               ),
                        column(3,
                               plotOutput('shotMetrics', height = 450)
                               )
                        ),
               fluidRow(column(9, 
                               DT::dataTableOutput("shotTable")
                               )
                        ),
               br(),
               hr(),
               fluidRow(column(9, 
                               DT::dataTableOutput("shotTable_all")
                               )
                        )
               ),
      tabPanel("Squad Age")
    )


)

server <- function(input, output) {
  
  output$shotMap <- renderPlot({
    player = strsplit(input$selPlayer, split = ' (', fixed = TRUE)[[1]][1]
    
    if(input$selColor != "") colorAes = input$selColor
    if(input$selShape != "") shapeAes = input$selShape
    if(input$selSize != "") sizeAes = input$selSize
    
    p = ggplot(shotMapDT23[situation2 != "penalty", .N, .(x2 = mround(x2, 1.75), y2 = mround(y2, 1.7))][x2 > 0],
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
      
      coord_flip(xlim = c(75, 105), ylim = c(14, 54)) +
      
      scale_fill_gradient(low = "grey84", high = "grey22", guide = "none")
    
    if(input$selCompare == ""){
      sm_aes(data = shotMapDT23, smPlot = p, playerName = player, input$selColor, input$selShape, input$selSize)
    }else{
      p1 = sm_aes(data = shotMapDT23, smPlot = p, playerName = player, input$selColor, input$selShape, input$selSize) + labs(title = player)
      player2 = strsplit(input$selCompare, split = ' (', fixed = TRUE)[[1]][1]
      p2 = sm_aes(data = shotMapDT23, smPlot = p, playerName = player2, input$selColor, input$selShape, input$selSize) + labs(title = player2)
      grid.arrange(p1, p2, nrow = 1)
    }
    
  })
  
  output$shotMetrics <- renderPlot({
    player = strsplit(input$selPlayer, split = ' (', fixed = TRUE)[[1]][1]
    
    posPlayer = shots_summary[player.shortName == player, head(position  , 1)]
    p1 = ggplot(shots_summary[position == posPlayer],
                aes(x = shots90, y = 0)) +
      geom_density_ridges(fill = "lemonchiffon3") +
      squadAge_Theme + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
      labs(title = "Shots per 90", x = "Count")
    
    p2 = ggplot(shots_summary[position == posPlayer],
                aes(x = distCenter, y = 0)) +
      geom_density_ridges(fill = "lemonchiffon3") +
      squadAge_Theme + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
      labs(title = "Distance to Goal", x = "Metres")
    
    plot_type = melt(shots_summary[position == posPlayer], measure.vars = c("foot", "head"), )
    p3 = ggplot(plot_type, aes(x = variable, y = value * 100)) +
      stat_summary(geom = "bar", fun = "mean", fill = "grey74", color = "grey54") +
      squadAge_Theme + 
      coord_flip(ylim = c(0,100), xlim = c(1, 2)) +
      scale_x_discrete(breaks = c("head", "foot"), labels = c("Head", "Foot")) +
      labs(title = "Type of Shot", y = "%", x = NULL)
    
    if(input$selCompare == ""){
      p1 = p1 +
        geom_point(data = shots_summary[player.shortName == player],
                   aes(x = shots90, y = 0), size = 10, shape = 21, stroke = 2, alpha = .75, fill = "red3", inherit.aes = FALSE)
      p2 = p2  +
        geom_point(data = shots_summary[player.shortName == player],
                   aes(x = distCenter, y = 0), size = 10, shape = 21, stroke = 2, alpha = .75, fill = "red3", inherit.aes = FALSE)
      p3 = p3  + 
        geom_bar(data = plot_type[player.shortName == player],
                 stat = "identity", alpha = .75, color = "grey22", linewidth = 1, fill = "indianred3")
      
      grid.arrange(p1, p2, p3, ncol = 1)
    }else{
      player2 = strsplit(input$selCompare, split = ' (', fixed = TRUE)[[1]][1]
      p1 = p1 +
        geom_point(data = shots_summary[player.shortName %in% c(player, player2)],
                   aes(x = shots90, y = 0, fill = player.shortName), size = 10, shape = 21, stroke = 2, alpha = .75, inherit.aes = FALSE) +
        scale_fill_manual(values = c("red3", "skyblue3"), name = NULL) +
        theme(legend.position = "top")
      
      p2 = p2 +
        geom_point(data = shots_summary[player.shortName %in% c(player, player2)],
                   aes(x = distCenter, y = 0, fill = player.shortName), size = 10, shape = 21, stroke = 2, alpha = .75, inherit.aes = FALSE) +
        scale_fill_manual(values = c("red3", "skyblue3"), guide = "none")
      
      p3 = p3 +
        geom_bar(data = plot_type[player.shortName %in% c(player, player2)],
                 aes(fill = player.shortName, group = player.shortName),
                 stat = "identity", position = "dodge", alpha = .75, color = "grey22", linewidth = 1) +
        scale_fill_manual(values = c("indianred3", "skyblue3"), guide = "none")
      
      grid.arrange(p1, p2, p3, ncol = 1,
                   layout_matrix = cbind(c(1,1,1,2,2,3,3), c(1,1,1,2,2,3,3)))
    }
  })
  
  output$shotTable <- renderDataTable({
    player = strsplit(input$selPlayer, split = ' (', fixed = TRUE)[[1]][1]
    player2 = strsplit(input$selCompare, split = ' (', fixed = TRUE)[[1]][1]
    datatable(
      shots_summary[player.shortName %in% c(player, player2), .(Player = player.shortName, "Minutes Played" = totMin, Goals, xG = round(xG, 2),
                                                  Shots = N, "Shots per 90" = round(shots90, 1),
                                                  "Distance to Goal" = round(distCenter, 1),
                                                  "xG per shot %" = round(xGshot * 100, 1))]
    )
  })
  
  output$shotTable_all <- renderDataTable({
    datatable(
      shots_summary[, .(Player = player.shortName, "Minutes Played" = totMin, Goals, xG = round(xG, 2),
                        Shots = N, "Shots per 90" = round(shots90, 1),
                        "Distance to Goal" = round(distCenter, 1),
                        "xG per shot %" = round(xGshot * 100, 1))]
    )
  })
  
}

shinyApp(ui = ui, server = server)

