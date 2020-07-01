make_teacher_links_le <- function(dataset, pagenum){
  
  school_byClassroom <- dataset %>% 
    group_by(schoolID, classID, School, TeacherLast, Grade) %>%
    summarise (count_kids =  n(),
               mean_SEL = round(mean(SEL.SS, na.rm=T),0),
               mean_UO = round(mean(UO.SS, na.rm=T),0),
               mean_SPS = round(mean(SPS.SS, na.rm=T),0),
               mean_SC = round(mean(SC.SS, na.rm=T),0))
  
  names(school_byClassroom) <- c("schoolID", "classID", "School", "Teacher", "Grade", "Number of children", "Overall SEL", 
                                 "Understanding Others",
                                 "Social Problem-Solving",
                                 "Self Control")
  
  
  if(pagenum == 1){
    teacher_links <- school_byClassroom %>% ungroup %>% 
      mutate(Teacher = paste0("<a href = classrooms/", classID, "/", 
                              tolower(gsub(" ", "_", Teacher)), "_index.html>", Teacher, "</a>")) %>% 
      ungroup(schoolID, classID) %>% 
      select(-1, -2)
  }
  else if(pagenum == 2){
    teacher_links <- school_byClassroom %>% ungroup %>%
      mutate(Teacher = paste0("<a href = classrooms/", classID, "/", 
                              tolower(gsub(" ", "_", Teacher)), "_index.html#timepoint-2>", Teacher, "</a>")) %>% 
      ungroup(schoolID, classID) %>% 
      select(-1, -2)
  }
  
  
  return(teacher_links)
  
}