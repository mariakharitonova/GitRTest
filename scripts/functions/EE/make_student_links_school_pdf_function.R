make_student_links_school_pdf <- function(dataset, pagenum){
  
  district_byStudent <- dataset %>% 
    group_by(classID, School, TeacherLast, Grade, StudentID) %>%
    select(classID, School, TeacherLast, Grade, StudentID, OverallScore, ERScore, SPTScore, SPSScore, SCScore)
    
  names(district_byStudent) <- c("classID", "School", "Teacher", "Grade", "StudentID","Overall Score", 
                                 "Emotion Recognition",
                                 "Social Perspective-Taking", "Social Problem-Solving", "Self Control")
  
  if(pagenum == 1){
    student_links <- district_byStudent %>% ungroup %>% 
      #mutate(StudentID = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),".pdf>", StudentID, "</a>"))
      mutate(StudentID = paste0("<a href=classrooms/", classID, "/students/", StudentID,".pdf>", StudentID, "</a>")) %>% 
      ungroup(classID) %>% 
      select(-1)
    
  }
  else if(pagenum == 2){
    student_links <- district_byStudent %>% ungroup %>% 
      #mutate(StudentID = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),".pdf#timepoint-2>", StudentID, "</a>"))
      mutate(StudentID = paste0("<a href=classrooms/", classID, "/students/", StudentID,".pdf>", StudentID, "</a>")) %>% 
      ungroup(classID) %>% 
      select(-1)
  }
  
  #tolower(gsub(" ", "_", School)), "/", 
  return(student_links)
  
}

##timepoint2