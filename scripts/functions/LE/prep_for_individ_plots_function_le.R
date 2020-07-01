prep_for_individ_plot_le <- function(dataset, student_id){

  ci_table <- calculate_CI_le()
  
  #reshape the data
  student_melted <- melt(dataset, id.vars = c("StudentID", "District", "School", "Grade","Timepoint", "TeacherLast", "LastName"), 
                         measure.vars = c("SEL.SS", "UO.SS", "SPS.SS", "SC.SS"),na.rm=T)
  
  
  #add the estimated true score value
  student_melted <- 
    mutate(student_melted, estim_true = ifelse(variable == "SEL.SS", ((value-100)*ci_table$alpha[1] + 100), 
                                               ifelse(variable == "UO.SS", ((value-100)*ci_table$alpha[2] + 100),
                                                      ifelse(variable == "SPS.SS", ((value-100)*ci_table$alpha[3] + 100),
                                                             ifelse(variable == "SC.SS", ((value-100)*ci_table$alpha[4] + 100),"")))))
  
  #add the CI_85 values           
  student_melted <- merge(student_melted[,1:10], ci_table[,c("variable", "CI_85")], by="variable", all.X=T)
  
  
  
  #select the right student
  student <- student_melted[student_melted$StudentID==student_id,]
  
  
  #check if all 5 categories (Overall, ER, SPT, SPS, SC) are present, if not add, by hand
  if("SEL.SS" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "SEL.SS", value = 0, estim_true = 0, CI_85= 0))
  }
  
  if("UO.SS" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "UO.SS", value = 0, estim_true = 0, CI_85= 0))
  }
  

  if("SPS.SS" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "SPS.SS", value = 0, estim_true = 0, CI_85= 0))
  }
  
  if("SC.SS" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "SC.SS", value = 0, estim_true = 0, CI_85= 0))
  }
  
  
  student$variable <- recode(student$variable, SEL.SS = "Overall Score", 
                             UO.SS = "Understanding Others", 
                             SPS.SS= "Social Problem-Solving", SC.SS= "Self Control")
  levels(student$variable) <- gsub(" ", "\n", levels(student$variable))  
  
  #rename value into score
  student <- student %>% mutate(Score = value)
  
  return(student)

}