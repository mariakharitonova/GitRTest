make_student_links_school_pdf_le <- function(dataset, pagenum){
  
  district_byStudent <- dataset %>% 
    group_by(classID, School, TeacherLast, Grade, StudentID) %>%
    select(classID, School, TeacherLast, Grade, StudentID, SEL.SS, UO.SS, SPS.SS, SC.SS)
    
  names(district_byStudent) <- c("classID", "School", "Teacher", "Grade", "StudentID","Overall SEL", 
                                 "Understanding Others", "Social Problem-Solving", "Self Control")
  
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