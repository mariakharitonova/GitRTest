make_teacher_links <- function(dataset, pagenum){
  
  school_byClassroom <- dataset %>% 
    group_by(schoolID, classID, School, TeacherLast, Grade) %>%
    summarise (count_kids =  n(),
               mean_overall = round(mean(OverallScore, na.rm=T),0),
               mean_er = round(mean(ERScore, na.rm=T),0),
               mean_spt = round(mean(SPTScore, na.rm=T),0),
               mean_sps = round(mean(SPSScore, na.rm=T),0),
               mean_sc = round(mean(SCScore, na.rm=T),0))
  
  names(school_byClassroom) <- c("schoolID", "classID", "School", "Teacher", "Grade", "Number of children","Overall Score", 
                                 "Emotion Recognition",
                                 "Social Perspective-Taking", "Social Problem-Solving", "Self Control")
  
  if(pagenum == 1){
    teacher_links <- school_byClassroom %>% ungroup %>% 
      #mutate(Teacher = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "_index.html>", Teacher, "</a>"))
      mutate(Teacher = paste0("<a href = classrooms/", classID, "/", 
                              tolower(gsub(" ", "_", Teacher)), "_index.html>", Teacher, "</a>")) %>% 
     ungroup(schoolID, classID) %>% 
      select(-1, -2)
  }
  
  else if(pagenum == 2){
    teacher_links <- school_byClassroom %>% ungroup %>%
      #mutate(Teacher = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "_index.html#timepoint-2>", Teacher, "</a>"))
      mutate(Teacher = paste0("<a href = classrooms/", classID, "/", 
                              tolower(gsub(" ", "_", Teacher)), "_index.html#timepoint-2>", Teacher, "</a>")) %>% 
      ungroup(schoolID, classID) %>% 
      select(-1, -2)
  }
  
  
  return(teacher_links)
  
}
