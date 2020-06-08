make_student_level_table <- function(dataset){
  datatable(
    dataset,
    filter = 'top',
    caption = paste0(district_name, ", Student-level data"),
    options = list(columnDefs = list(list(width = '90px', className = 'dt-center', targets = c(0:ncol(dataset)-1))), pageLength = 15), 
    style = 'bootstrap', class = c('cell-border', 'stripe','hover','order-column', 'no-wrap'), 
    escape = FALSE, rownames = FALSE
  ) %>%
    formatStyle(
      c("Overall SEL","Understanding Others", "Social Problem-Solving", "Self Control"),
      backgroundColor = styleInterval(c(70, 90, 115), c('#45874C', '#86C17D', '#C0E4B8', '#EFF7EA'))
    ) %>%
    formatStyle(1:6, 'vertical-align'='top')
}
