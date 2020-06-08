transform_for_plotting <- function(datafile){
  
  #step 1: melt
  district_melted <- melt(datafile, id.vars = c("District"), measure.vars = 
                            c("overall.cat", "er.cat", "spt.cat", "sps.cat", "sc.cat"),na.rm=T)
  
  #step 2: summarize
  district_plot <- dcast(district_melted, formula = District + value + variable ~ ..., fun = length)
  
  #rename header
  names(district_plot) <- c("District", "category", "module", "count")
  
  if("above" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "above", module = "overall.cat", count = 0))
  }
  
  if("meets" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "meets", module = "overall.cat", count = 0))
  }
  
  if("below" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "below", module = "overall.cat", count = 0))
  }
  
  if("well-below" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "well-below", module = "overall.cat", count = 0))
  }
  
  
  #add percent field
  district_plot <- district_plot %>% 
    group_by(District, module, category) %>%
    summarise (count = sum(count, na.rm = T)) %>%
    mutate(percent = round((count / sum(count, na.rm = T)*100),0))
  
  #reorder categories:
  #district_plot$module <- factor(district_plot$module, levels = c("sc.cat","sps.cat","spt.cat", "er.cat", "overall.cat"))
  
  
  
  #give full descriptions to each module
  district_plot$module <- recode(district_plot$module, "sc.cat"="Self Control", 
                                 "sps.cat" = "Social Problem-Solving", 
                                 "spt.cat" = "Social Perspective-Taking", "er.cat"="Emotion Recognition", "overall.cat"="Overall Score")
  
  #levels(district_plot_bySchool$module) <- gsub("  ", "\n",levels(district_plot_bySchool$module))
  
  #give full descriptions to each category 
  district_plot$category <- factor(district_plot$category, levels = c("above","meets","below","well-below"), 
                                   labels = c("Above average","At or around average", "Below average ", "Well below average"))
  
  return(district_plot)
  
}