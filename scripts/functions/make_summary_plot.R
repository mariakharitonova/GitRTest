make_summary_plot <- function(dataset, name, n){
  plot <- ggplot(data = dataset, aes(y=percent, x = module, fill = category)) +
    geom_bar(stat = "identity", width=.8) +
    ggtitle(paste0(name, ", Summary Plot (N = ", n, ")")) + 
    labs(y = "Percent") + 
    theme(axis.title.y = element_blank(), text = element_text(size=12),axis.title.x = element_blank(),
          legend.title = element_blank(),
          panel.background = element_blank()) + 
    scale_fill_brewer(palette="Greens") +
    #scale_fill_manual(values=c("steelblue4", "darkolivegreen3", "lightgoldenrod2","tomato3")) +
    geom_text(data=subset(dataset, percent>2),
              aes(label = paste0(round(percent,0),"%")), position =
                position_stack(vjust = 0.5, reverse=F), size = 4)
  
  # print plots to screen
  #print(plot)
  ggplotly(plot) %>% config(displayModeBar = F)
  #position = position_stack(reverse=F),
  #coord_flip() +
}