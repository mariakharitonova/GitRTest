make_summary_plot <- function(dataset, name, n){
  
  plot <- ggplot(data = dataset, aes(x=module, y = percent, fill = category))  +
    geom_bar(stat = "identity", width=.7) +
    ggtitle(paste0(name, ", Summary Plot (N = ", n, ")")) + 
    labs(y = "Percent") + 
    theme(legend.title = element_blank(), axis.title.x = element_blank(), 
          text = element_text(size=12), panel.background = element_blank()) + 
   #scale_fill_brewer(palette="Greens") +
   scale_fill_manual(values = c("#E5F5E0", "#A1D99B","#41AB5D","#006D2C")) +  #light top
   #scale_fill_manual(values = c("#006D2C", "#41AB5D", "#A1D99B", "#E5F5E0")) +  #dark top
    #scale_fill_manual(values = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D")) + 
    geom_text(data=subset(dataset, percent>0),
              aes(label = paste0(round(percent,0),"%")), 
              position = position_stack(vjust = 0.5), size = 4)
  
  ggplotly(plot) %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  
  
}


#display Hex codes:
#brewer.pal(n = 8, name = "Greens")
#display.brewer.pal(5, "Greens")
