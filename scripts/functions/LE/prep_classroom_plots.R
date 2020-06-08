prep_classroom_plots <- function(dataset){
  #step 1: melt
  school_melted <- melt(dataset, id.vars = c("Teacher"), measure.vars = 
                          c("SEL.SS.cat", "UO.SS.cat", "SPS.SS.cat", "SC.SS.cat"),na.rm=T)
  
  #step 2: summarize
  school_plot_byTeacher <- dcast(school_melted, formula = Teacher + value + variable ~ ..., fun = length)
  
  #rename header
  names(school_plot_byTeacher) <- c("Teacher", "category","module","count")
  
  #check if all 4 categories (above, meets, below and well-below) are represented, if not add a row w/0 in count
  
  if("above" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "above", module = "SEL.SS.cat", count =0))
  }
  
  if("meets" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "meets", module = "SEL.SS.cat", count =0))
  }
  
  if("below" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "below", module = "SEL.SS.cat", count =0))
  }
  
  if("well-below" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "well-below", module = "SEL.SS.cat", count =0))
  }
  
  #add percent field
  school_plot_byTeacher <- school_plot_byTeacher %>% 
    group_by(Teacher, module, category) %>%
    summarise (count = sum(count, na.rm = T)) %>%
    mutate(percent = round((count / sum(count, na.rm = T)*100),2))
  
  #reorder categories:
  # school_plot_byTeacher$module <- factor(school_plot_byTeacher$module, levels = c("SC.SS.cat","SPS.SS.cat","UO.SS.cat", "SEL.SS.cat"))
  # 
  # 
  
  #give full descriptions to each module
  school_plot_byTeacher$module <- recode(school_plot_byTeacher$module, "SC.SS.cat"="Self Control",
                                         "SPS.SS.cat" = "Social Problem-Solving",
                                         "UO.SS.cat" = "Understanding Others", "SEL.SS.cat"="Overall SEL")
  
  #give full descriptions to each category 
  school_plot_byTeacher$category <- factor(school_plot_byTeacher$category, levels = c("above","meets","below","well-below"), labels = c("Above (>=115)","Meets (90-114)", "Below (70-89)", "Well Below Expectations (<=69)"))
  
  return(school_plot_byTeacher)
  
}