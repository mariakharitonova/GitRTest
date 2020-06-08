prep_classroom_plots <- function(dataset){
  #step 1: melt
  school_melted <- melt(dataset, id.vars = c("TeacherLast"), measure.vars = 
                          c("overall.cat", "er.cat", "spt.cat", "sps.cat", "sc.cat"),na.rm=T)
  
  #step 2: summarize
  school_plot_byTeacher <- dcast(school_melted, formula = TeacherLast + value + variable ~ ..., fun = length)
  
  #rename header
  names(school_plot_byTeacher) <- c("Teacher", "category","module","count")
  
  #check if all 4 categories (above, meets, below and well-below) are represented, if not add a row w/0 in count
  
  if("above" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "above", module = "overall.cat", count =0))
  }
  
  if("meets" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "meets", module = "overall.cat", count =0))
  }
  
  if("below" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "below", module = "overall.cat", count =0))
  }
  
  if("well-below" %in% school_plot_byTeacher$category == FALSE){
    school_plot_byTeacher <- rbind(school_plot_byTeacher, data.frame(Teacher = params$teacher_name, category = "well-below", module = "overall.cat", count =0))
  }
  
  #add percent field
  school_plot_byTeacher <- school_plot_byTeacher %>% 
    group_by(Teacher, module, category) %>%
    summarise (count = sum(count, na.rm = T)) %>%
    mutate(percent = round((count / sum(count, na.rm = T)*100),2))
  
  #reorder categories:
  #school_plot_byTeacher$module <- factor(school_plot_byTeacher$module, levels = c("sc.cat","sps.cat","spt.cat", "er.cat", "overall.cat"))
  
  
  
  #give full descriptions to each module
  school_plot_byTeacher$module <- recode(school_plot_byTeacher$module,"sc.cat"="Self Control", 
                                         "sps.cat" = "Social Problem-Solving", 
                                         "spt.cat" = "Social Perspective-Taking", "er.cat"="Emotion Recognition", "overall.cat"="Overall Score")
  
  #give full descriptions to each category 
  school_plot_byTeacher$category <- factor(school_plot_byTeacher$category, levels = c("above","meets","below","well-below"), 
                                           labels = c("Above average","At or around average", "Below average", "Well below average"))
                                           #labels = c("Above average (>=115)","At or around average (90-114)", "Below average (70-89)", "Well below average (<=69)"))
  
  return(school_plot_byTeacher)
  
}