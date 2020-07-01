make_student_level_table_links_le <- function(dataset){
  datatable(
    dataset,
    filter = 'top',
    caption = paste0(params$school_name, ", Student-level data"),
    options = list(columnDefs = list(list(width = '90px', className = 'dt-center', targets = c(0:ncol(dataset)-1))), pageLength = 15), 
    style = 'bootstrap', class = c('cell-border', 'stripe','hover','order-column', 'no-wrap'), 
    escape = FALSE, rownames = FALSE) %>%
    formatStyle(
      c("Overall SEL", "Understanding Others", "Social Problem-Solving", "Self Control"),
      #backgroundColor = styleInterval(c(70, 90, 115), c('#006D2C', '#41AB5D', '#A1D99B', '#d3f5c9'))
      backgroundColor = styleInterval(c(70, 90, 115), color_palette_dark_light)
    ) %>%
   formatStyle(1:6, 'vertical-align'='top') 
}

