make_summary_plot_pdf <- function(dataset, name, n){
 # library(ggplot2)
#  library(extrafont)
  loadfonts(quiet = TRUE)
  
  dataset$module <- factor(dataset$module, 
                           levels = c("Overall Score", "Emotion Recognition", "Social Perspective-Taking","Social Problem-Solving", "Self Control"))
  
  # dataset$category <- factor(dataset$category, levels = c("Above average  ", "At or around average  ", "Below average  ", 
  #                                                         "Well below average  "))
  
  levels(dataset$module) <- gsub(" ", "\n", levels(dataset$module))
  
  plot <- ggplot(data = dataset, aes(y=percent, x = module, fill = category)) +
    geom_bar(stat = "identity", width=.7) +
    #ggtitle(name, " District, Timepoint 1: ", n, " students", "\n") + 
    ggtitle(paste0(name, " (N = ", n, ")")) + 
    labs(y = "Percent") + 
    theme(axis.title.y = element_blank(), text = element_text(size=12, family = "Nunito"), 
          axis.title.x = element_blank(), 
          legend.title = element_blank(),
          panel.background = element_blank(), legend.position="bottom",
          axis.text.x = element_text(size = 9),
          legend.text=element_text(size=8),
          plot.title = element_text(hjust = 0.5, size=10)) +
          #plot.title = element_text(hjust = 0, size=11, face="bold")) + 
    scale_fill_brewer(palette="Greens") +
    #scale_fill_manual(values=c("steelblue4", "darkolivegreen3", "lightgoldenrod2","tomato3")) +
    geom_text(data=subset(dataset, percent>0),
              aes(label = paste0(round(percent,0),"%")), position =
                position_stack(vjust = 0.5, reverse=F), size = 3)
  
  # print plots to screen
  print(plot)
  
}