#transform the data for summary plots
transform_for_plotting_le <- function(datafile){
  
  #step 1: melt
  district_melted <- melt(datafile, id.vars = c("District"), measure.vars = 
                            c("SEL.SS.cat", "UO.SS.cat", "SPS.SS.cat", "SC.SS.cat"),na.rm=T)
  
  #step 2: summarize
  district_plot <- dcast(district_melted, formula = District + value + variable ~ ..., fun = length)
  
  #rename header
  names(district_plot) <- c("District", "category", "module", "count")
  
  if("above" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "above", module = "SEL.SS.cat", count = 0))
  }
  
  if("meets" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "meets", module = "SEL.SS.cat", count = 0))
  }
  
  if("below" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "below", module = "SEL.SS.cat", count = 0))
  }
  
  if("well-below" %in% district_plot$category == FALSE){
    district_plot <- rbind(district_plot, data.frame(District = district_name, category = "well-below", module = "SEL.SS.cat", count = 0))
  }
  
  
  #add percent  
  district_plot <- district_plot %>% 
    group_by(District, module, category) %>%
    summarise (count = sum(count, na.rm = T)) %>%
    mutate(percent = round((count / sum(count, na.rm = T)*100),2))
  
  #reorder categories:
  # district_plot$module <- factor(district_plot$module, levels = c("SC.SS.cat","SPS.SS.cat","UO.SS.cat", "SEL.SS.cat"))
  # 
  
  #give full descriptions to each module
  district_plot$module <- recode(district_plot$module, "SC.SS.cat"="Self Control", 
                                 "SPS.SS.cat" = "Social Problem-Solving", 
                                 "UO.SS.cat" = "Understanding Others", "SEL.SS.cat"="Overall SEL")
  
  #levels(district_plot_bySchool$module) <- gsub("  ", "\n",levels(district_plot_bySchool$module))
  
  #give full descriptions to each category 
  district_plot$category <- factor(district_plot$category, levels = c("above","meets","below","well-below"), labels = c("Above (>=115)","Meets (90-114)", "Below (70-89)", "Well Below Expectations (<=69)"))
  
  return(district_plot)
  
}