make_teacher_links <- function(dataset, pagenum){
  
  school_byClassroom <- dataset %>% 
    group_by(School, Teacher, Grade) %>%
    summarise (count_kids =  n(),
               mean_SEL = round(mean(SEL.SS, na.rm=T),0),
               mean_UO = round(mean(UO.SS, na.rm=T),0),
               mean_SPS = round(mean(SPS.SS, na.rm=T),0),
               mean_SC = round(mean(SC.SS, na.rm=T),0))
  
  names(school_byClassroom) <- c("School", "Teacher", "Grade", "Number of children","Overall SEL", 
                                 "Understanding Others",
                                 "Social Problem-Solving",
                                 "Self Control")
  
  
  if(pagenum == 1){
    teacher_links <- school_byClassroom %>% ungroup %>% 
      mutate(Teacher = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "_index.html>", Teacher, "</a>"))
  }
  else if(pagenum == 2){
    teacher_links <- school_byClassroom %>% ungroup %>% 
      mutate(Teacher = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "_index.html#timepoint-2>", Teacher, "</a>"))
  }
  
  
  return(teacher_links)
  
}