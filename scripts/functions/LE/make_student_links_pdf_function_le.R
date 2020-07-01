make_student_links_pdf_le <- function(dataset, pagenum){
  
  classroom_byStudent <- dataset %>% 
    group_by(School, TeacherLast, Grade, StudentID) %>%
    summarise(mean_overall = mean(SEL.SS, na.rm = T),
               mean_uo = mean(UO.SS, na.rm = T),
               mean_sps = mean(SPS.SS, na.rm = T), 
               mean_sc = mean(SC.SS, na.rm = T))
  
  names(classroom_byStudent) <- c("School", "Teacher", "Grade", "StudentID", "Overall SEL", 
                                 "Understanding Others",
                                 "Social Problem-Solving", "Self Control")
  
  student_links <- classroom_byStudent %>% ungroup %>% 
    mutate(StudentID = paste0("<a href=students/", StudentID,".pdf>", StudentID, "</a>"))
  
  
  # if(pagenum == 1){
  #   student_links <- classroom_byStudent %>% ungroup %>% 
  #     mutate(StudentID = paste0("<a href=students/", student_name_nice,".pdf>", StudentID, "</a>"))
  #   
  # }
  # else if(pagenum == 2){
  #   student_links <- classroom_byStudent %>% ungroup %>% 
  #     mutate(StudentID = paste0("<a href=", tolower(gsub(" ", "_", Teacher)), "/", tolower(gsub(" ", "_", StudentID)),".pdf#timepoint-2>", StudentID, "</a>"))
  # }
  
  #tolower(gsub(" ", "_", School)), "/", 
  return(student_links)
  
}
#tolower(gsub(" ", "_", School)), "/", 