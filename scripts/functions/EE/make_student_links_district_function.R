make_student_links_district <- function(dataset, pagenum){
  
  district_byStudent <- dataset %>% 
    #group_by(School, TeacherLast, Grade, StudentID) %>%
    select(StudentID, Timepoint, Language, Grade, LastName, School, TeacherLast, OverallScore, ERScore, SPTScore, SPSScore, SCScore)
  
  names(district_byStudent) <- c("StudentID", "Timepoint", "Language", "Grade", "Last Name", "School", "Teacher",
                                 "Overall Score", "Emotion Recognition",
                                 "Social Perspective-Taking", "Social Problem-Solving", "Self Control")
  
  if(pagenum == 1){
    student_links <- district_byStudent %>% ungroup %>% 
      mutate(StudentID = paste0("<a href=schools/", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),"_index.html>", StudentID, "</a>"))
    
  }
  else if(pagenum == 2){
    student_links <- district_byStudent %>% ungroup %>% 
      mutate(StudentID = paste0("<a href=schools/", tolower(gsub(" ", "_", School)), "/", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),"_index.html#timepoint-2>", StudentID, "</a>"))
  }
  
  #tolower(gsub(" ", "_", School)), "/", 
  return(student_links)
  
}
