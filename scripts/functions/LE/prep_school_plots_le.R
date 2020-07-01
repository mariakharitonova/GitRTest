prep_school_plots_le <- function(dataset){
  
  #step 1: melt
  school_melted <- melt(dataset, id.vars = c("School"), measure.vars = 
                          c("SEL.SS.cat", "UO.SS.cat", "SPS.SS.cat", "SC.SS.cat"),na.rm=T)
  
  #step 2: summarize
  school_plot <- dcast(school_melted, formula = School + value + variable ~ ..., fun = length)
  
  #rename header
  names(school_plot) <- c("School", "category", "module","count")
  
  
  if("above" %in% school_plot$category == FALSE){
    school_plot <- rbind(school_plot, data.frame(School = params$school_name, category = "above", module = "SEL.SS.cat", count = 0))
  }
  
  if("meets" %in% school_plot$category == FALSE){
    school_plot <- rbind(school_plot, data.frame(School = params$school_name, category = "meets", module = "SEL.SS.cat", count = 0))
  }
  
  if("below" %in% school_plot$category == FALSE){
    school_plot <- rbind(school_plot, data.frame(School = params$school_name, category = "below", module = "SEL.SS.cat", count = 0))
  }
  
  if("well-below" %in% school_plot$category == FALSE){
    school_plot <- rbind(school_plot, data.frame(School = params$school_name, category = "well-below", module = "SEL.SS.cat", count = 0))
  }
  
  #add percent field
  school_plot <- school_plot %>%
    group_by(School, module, category) %>%
    summarise (count = sum(count, na.rm = T)) %>%
    mutate(percent = round((count / sum(count, na.rm = T)*100),2))
  
  #reorder categories:
  # school_plot$module <- factor(school_plot$module, levels = c("SC.SS.cat","SPS.SS.cat","UO.SS.cat", "SEL.SS.cat"))
  # 
  #give full descriptions to each module
  school_plot$module <- recode(school_plot$module, "SC.SS.cat"="Self Control",
                               "SPS.SS.cat" = "Social Problem-Solving",
                               "UO.SS.cat" = "Understanding Others", "SEL.SS.cat"="Overall SEL")
  
  #levels(school_plot_byTeacher$module) <- gsub("  ", "\n",levels(school_plot_byTeacher$module))
  
  #give full descriptions to each category
  school_plot$category <- factor(school_plot$category, 
                                 levels = c("above","meets","below","well-below"), 
                                 labels = c("Above average","At or around average", "Below average", "Well below average"))
  
  return(school_plot)
  
}