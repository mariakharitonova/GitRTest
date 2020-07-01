make_summary_table_le <- function(dataset, name){
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
    escape = FALSE, rownames = FALSE
  ) %>%
    formatStyle(
      c("Overall SEL", "Understanding Others", "Social Problem-Solving", "Self Control"),
      backgroundColor = styleInterval(c(70, 90, 115), c('#45874C', '#86C17D', '#C0E4B8', '#EFF7EA'))
    )
}