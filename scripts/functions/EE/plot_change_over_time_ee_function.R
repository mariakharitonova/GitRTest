plot_change_over_time_ee <- function(dataset, name, n){
  library(RColorBrewer)
  
  dataset$module <- factor(dataset$module, levels = c("Overall Score", "Emotion Recognition", "Social Perspective-Taking","Social Problem-Solving", "Self Control"))
  
  levels(dataset$module) <- gsub(" ", "\n", levels(dataset$module))
  
  #dataset$category <- factor(dataset$category, levels = c("Above average (>=115)","At or around average (90-114)", "Below average (70-89)", 
  #                                                        "Well below average (<=69)"))
  
  plot <- ggplot(data = dataset, aes(y = percent, x = Timepoint, fill = category)) +
    geom_bar(stat = "identity") +
    facet_grid(~ module) +  
    labs(y = "Percent") + 
    ggtitle(paste0(name, ", Fall-Spring Comparison: ", n, " common students", "\n")) +
    theme(legend.title = element_blank(), axis.title.x = element_blank(), 
          text = element_text(size=12), panel.background = element_blank(), 
          strip.background =element_rect(fill="white"),  # legend.position="bottom",
          legend.text=element_text(size=10),
          strip.text = element_text(size=10), plot.title = element_text(hjust = 0.5, size=15),
          plot.margin = unit(c(15, 10, 10, 10), "mm")) + 
    scale_fill_brewer(palette="Greens") +
    #scale_fill_manual(values = c("#006D2C", "#41AB5D", "#A1D99B", "#E5F5E0")) + 
    geom_text(data=subset(dataset, percent>0),
              aes(label = paste0(round(percent,0),"%")), position =
                position_stack(vjust = 0.5), size = 4)

  ggplotly(plot) %>% config(displayModeBar = F) %>% layout(margin = list(l = 75), 
                                                                        xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  
}