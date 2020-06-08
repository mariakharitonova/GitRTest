make_student_links_school <- function(dataset, pagenum){
  
  district_byStudent <- dataset %>% 
    group_by(School, TeacherLast, Grade, StudentID) %>%
    select(School, TeacherLast, Grade, StudentID, OverallScore, ERScore, SPTScore, SPSScore, SCScore)
    
  names(district_byStudent) <- c("School", "Teacher", "Grade", "StudentID","Overall Score", 
                                 "Emotion Recognition",
                                 "Social Perspective-Taking", "Social Problem-Solving", "Self Control")
  
  if(pagenum == 1){
    student_links <- district_byStudent %>% ungroup %>% 
      mutate(StudentID = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),"_index.html>", StudentID, "</a>"))
    
  }
  else if(pagenum == 2){
    student_links <- district_byStudent %>% ungroup %>% 
      mutate(StudentID = paste0("<a href=", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),"_index.html#timepoint-2>", StudentID, "</a>"))
  }
  
  #tolower(gsub(" ", "_", School)), "/", 
  return(student_links)
  
}

##timepoint2