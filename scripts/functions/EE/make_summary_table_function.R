make_summary_table <- function(dataset, name){
  # colors here: http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  datatable(
    dataset, 
    caption = paste0(name, ", Summary Table"),
    options = list(
      order = list(list(0, 'asc')),
      dom = 'ftipr', 
      columnDefs = list(list(width = '120px', className = 'dt-center', targets=c(1:(ncol(dataset)-1)))), #, targets = c(1:7), "_all"
      pageLength = 10), 
    style = 'bootstrap', class = c('cell-border', 'stripe','hover','order-column', 'no-wrap'), 
    escape = FALSE, rownames = FALSE) %>%
    formatStyle(
      c("Overall Score","Emotion Recognition", "Social Perspective-Taking", "Social Problem-Solving","Self Control"),
      #backgroundColor = styleInterval(c(70, 90, 115), c('#006D2C', '#41AB5D','#A1D99B', '#d3f5c9#')) #E5F5E0'
      backgroundColor = styleInterval(c(70, 90, 115), color_palette_dark_light) 
    )
}

#'#45874C', '#86C17D', '#C0E4B8', '#EFF7EA',