make_student_links_district_pdf <- function(dataset, pagenum){
  
  district_byStudent <- dataset %>% 
    group_by(schoolID, classID, School, TeacherLast, Grade, StudentID) %>%
    select(schoolID, classID, StudentID, Timepoint, Language, Grade, LastName, School, TeacherLast, OverallScore, ERScore, SPTScore, SPSScore, SCScore)
  
  names(district_byStudent) <- c("schoolID", "classID", "StudentID", "Timepoint", "Language", "Grade", "Last Name", "School", "Teacher",
                                 "Overall Score", "Emotion Recognition",
                                 "Social Perspective-Taking", "Social Problem-Solving", "Self Control")
  
  if(pagenum == 1){
    student_links <- district_byStudent %>% ungroup %>% 
      #mutate(StudentID = paste0("<a href=schools/", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),".pdf>", StudentID, "</a>"))
      mutate(StudentID = paste0("<a href=schools/", schoolID, "/classrooms/", classID, "/students/", StudentID,".pdf>", StudentID, "</a>")) %>% 
      ungroup(schoolID, classID) %>% 
      select(-1, -2)
    
    
  }
  else if(pagenum == 2){
    student_links <- district_byStudent %>% ungroup %>% 
      #mutate(StudentID = paste0("<a href=schools/", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),".pdf#timepoint-2>", StudentID, "</a>"))
      mutate(StudentID = paste0("<a href=schools/", schoolID, "/classrooms/", classID, "/students/", StudentID,".pdf>", StudentID, "</a>")) %>% 
      ungroup(schoolID, classID) %>% 
      select(-1, -2)
  }
  
  #tolower(gsub(" ", "_", School)), "/", 
  return(student_links)
  
}
