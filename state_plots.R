state_data = state_data[order(Team, date)]
state_data[, Game := ifelse(Team == shift(Team, type = "lag") &
                              Opponent == shift(Opponent, type = "lag"), 0, 1)]
state_data[1, Game := 1]
state_data[, Game := cumsum(Game), .(Team, season)]

state_data[, stateGroup := ifelse(state == 0, "Draw", "+-1")]
state_data[, stateGroup := ifelse(abs(state) > 1, "> 1", stateGroup)]
state_data[, stateGroup := factor(stateGroup, levels = c("Draw", "+-1", "> 1"))]

state_data[Team %in% c("IFK Göteborg", "IFK Värnamo"), Game := Game + 1]


ggplot(state_data[season == 2023],
       aes(x = time - timeState, xend = time, group = sequence, color = stateGroup,
           y = xG - xG_Against, yend = xG - xG_Against)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_label(data = state_data[season == 2023, .(Opponent = unique(Opponent)), .(Team, Game)], 
             aes(label = Opponent, x = 0, y = 2.65), inherit.aes = FALSE, hjust = 0, size = 3) + 
  geom_segment(size = 1.5) +
  facet_grid(Game~Team) +
  theme_bw() + theme(legend.position = "top", strip.text.y = element_text(angle = 0)) + 
  ylim(state_data[, range(xG - xG_Against)]) 



state_data[season == 2023 & stateGroup == "Draw",
           .(xG_Diff = sum(xG - xG_Against),
             xG_Diff_Time = ((sum(xG) - sum(xG_Against)) / sum(timeState)) * (60 * 60),
             timeState = sum(timeState)),
           Team][order(-xG_Diff_Time)]


state_data[season == 2023 & abs(state) <= 1,
           .(xG_Diff = sum(xG - xG_Against),
             xG_Diff_Time = ((sum(xG) - sum(xG_Against)) / sum(timeState)) * (60 * 60),
             timeState = sum(timeState)),
           Team][order(-xG_Diff_Time)]




# Measure expected xG, and compare to acutal, include how tough the oppositin was expected to be






ggplot(shotMapDT[(Team == "IFK Norrköping" | Opponent == "IFK Norrköping") & season == 2023],
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
  
  facet_wrap(~Team == "IFK Norrköping", ncol = 2) + xlim(c(0, 52.5)) + 
  
  theme(legend.key.width = unit(1, "cm"), legend.position = "top",
        legend.background = element_rect(fill = "#00000010", color = "grey72"),
        panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black")) +
  
  coord_flip(xlim = c(0, 40)) +
  scale_fill_manual(values = c("indianred", "seagreen"))





xG_stateDraw = state_data_23[abs(stateClass) <= 1, .(xG = sum(xG), xG_Against = sum(xG_Against), time = sum(timeState) / 3600), Team]

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



state_data_23[, .(xG = sum(xG), xG_Against = sum(xG_Against), time = sum(timeState) / 3600), .(Team, stateClass)]
ggplot(state_data_23[abs(state) <= 1, .(xGDiff = (sum(xG) - sum(xG_Against)) / (sum(timeState) / 3600)), .(Team, stateClass)],
       aes(x = stateClass, y = xGDiff, fill = xGDiff > 0)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Team) +
  scale_fill_manual(name = NULL, guide = 'none', values = c("red3","seagreen")) +
  # scale_fill_manual(name = NULL, guide = 'none', values = c("red3", "black", "seagreen")) +
  theme_bw()


ggplot(state_data_23[abs(state) <= 1, .(xGDiff = (sum(xG) - sum(xG_Against)) / (sum(timeState) / 3600)), .(Team)],
       aes(x = Team, y = xGDiff, fill = xGDiff > 0)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Team, scales = "free_x") +
  scale_fill_manual(name = NULL, guide = 'none', values = c("red3","seagreen")) +
  # scale_fill_manual(name = NULL, guide = 'none', values = c("red3", "black", "seagreen")) +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
