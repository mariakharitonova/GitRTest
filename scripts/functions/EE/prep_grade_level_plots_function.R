prep_grade_level_plots <- function(dataset){    
  #for the by grade plot:
  
  #step 1: melt
  school_grade_melted <- melt(dataset, id.vars = c("School", "Grade"), measure.vars = 
                                c("overall.cat", "er.cat", "spt.cat", "sps.cat", "sc.cat"),na.rm=T)
  
  #step 2: summarize
  school_grade_plot <- dcast(school_grade_melted, formula = School + Grade + value + variable ~ ..., fun = length)
  
  
  #rename header
  names(school_grade_plot) <- c("School", "Grade", "category", "module","count")
  
  for(school in unique(school_grade_plot$School)){
    for(grade in unique(school_grade_plot$Grade)){
      for(mod in unique(school_grade_plot$module)){
        
        if("above" %in% school_grade_plot[school_grade_plot$School==school & school_grade_plot$Grade==grade & school_grade_plot$module==mod,]$category == FALSE){
          school_grade_plot <- rbind(school_grade_plot, data.frame(School = params$school_name, Grade = grade, category = "above", module = mod, count = 0))
        }
        
        if("meets" %in% school_grade_plot[school_grade_plot$School==school & school_grade_plot$Grade==grade & school_grade_plot$module==mod,]$category == FALSE){
          school_grade_plot <- rbind(school_grade_plot, data.frame(School = params$school_name, Grade = grade, category = "meets", module = mod, count = 0))
        }
        
        if("below" %in% school_grade_plot[school_grade_plot$School==school & school_grade_plot$Grade==grade & school_grade_plot$module==mod,]$category == FALSE){
          school_grade_plot <- rbind(school_grade_plot, data.frame(School = params$school_name, Grade = grade, category = "below", module = mod, count = 0))
        }
        
        if("well-below" %in% school_grade_plot[school_grade_plot$School==school & school_grade_plot$Grade==grade & school_grade_plot$module==mod,]$category == FALSE){
          school_grade_plot <- rbind(school_grade_plot, data.frame(School = params$school_name, Grade = grade, category = "well-below", module = mod, count = 0))
        }
      }
    }
  }
  
  #add percent field
  school_grade_plot <- school_grade_plot %>% 
    group_by(School, Grade, module, category) %>%
    summarise (count = sum(count, na.rm = T)) %>%
    mutate(percent = round((count / sum(count, na.rm = T)*100),2))
  
  #reorder categories:
  school_grade_plot$module <- factor(school_grade_plot$module, levels = c("overall.cat", "er.cat", "spt.cat", "sps.cat","sc.cat"))
  
  
  #give full descriptions to each module
  school_grade_plot$module <- recode(school_grade_plot$module, "sc.cat"="Self Control", 
                                     "sps.cat" = "Social Problem-Solving", 
                                     "spt.cat" = "Social Perspective-Taking", "er.cat"="Emotion Recognition", "overall.cat"="Overall Score")
  
  #levels(school_plot_byTeacher$module) <- gsub("  ", "\n",levels(school_plot_byTeacher$module))
  
  
  #give full descriptions to each category 
  school_grade_plot$category <- factor(school_grade_plot$category, levels = c("above","meets","below","well-below"), 
                                       labels = c("Above average","At or around average", "Below average", "Well below average"))
  
  return(school_grade_plot)
}