make_summary_table_summary_row <- function(dataset, name){
  # colors here: http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  
  # keep averages separate
  avgs <- apply(dataset[,-1], # don't average the school name column
                2,
                mean)
  # round the averages here instead if wanted
  avgs <- round(avgs, 0)
  avgs <- c("Average", as.character(avgs)) # build table footer as text values

  # avgs <- as.list(avgs)
  # avgs$School <- "Average"
  #dataset <- rbind(dataset, avgs)
  
  # manually set the table header and footer
  sketch <- htmltools::withTags(table(
    tableHeader(dataset),  # this automatically pulls column names from the dataset
    tableFooter(avgs)  # the row we created
  ))
  
  
  datatable(
    dataset, 
    caption = paste0(name, ", Summary Table"),
    options = list(
      order = list(list(0, 'asc')),  # leaving this as is will sort the Average row like others; alt options below
      dom = 'ftipr', 
      columnDefs = list(list(width = '120px', className = 'dt-center', 
                             targets=c(1:(ncol(dataset)-1)))),
      pageLength = 10), 
    style = 'bootstrap', 
    class = c('cell-border', 'stripe','hover','order-column', 'no-wrap'), 
    escape = FALSE, rownames = FALSE,
    container = sketch
  ) %>%
    formatStyle(
      columns=3:ncol(dataset),  # by index instead of name, but you could switch back
      backgroundColor = styleInterval(c(70, 90, 115), c('#45874C', '#86C17D', '#C0E4B8', '#EFF7EA'))
    )  %>%  # ADD BELOW TO ROUND COLUMN VALUES
    formatRound(columns=2:ncol(dataset),
                digits=0) %>%
    # ADD BELOW TO BOLD AVERAGE ROW - STILL WORKS
    formatStyle('School',  # Because column is called School
                target = 'row',
                fontWeight = styleEqual('Average', 'bold'))
  
  
  
}

#'#45874C', '#86C17D', '#C0E4B8', '#EFF7EA',