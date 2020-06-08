prep_for_individ_plot <- function(dataset, student_id){

  ci_table <- calculate_CI_ee()
  
  #reshape the data
  student_melted <- melt(dataset, id.vars = c("StudentID", "District", "School", "Grade","Timepoint", "TeacherLast", "LastName"), 
                         measure.vars = c("OverallScore", "ERScore", "SPTScore", "SPSScore", "SCScore"),na.rm=T)
  
  
  #add the estimated true score value
  student_melted <- 
    mutate(student_melted, estim_true = ifelse(variable == "OverallScore", ((value-100)*ci_table$alpha[1] + 100), 
                                               ifelse(variable == "ERScore", ((value-100)*ci_table$alpha[2] + 100),
                                                      ifelse(variable == "SPTScore", ((value-100)*ci_table$alpha[3] + 100),
                                                             ifelse(variable == "SPSScore", ((value-100)*ci_table$alpha[4] + 100),
                                                                    ifelse(variable == "SCScore", ((value-100)*ci_table$alpha[5] + 100),""))))))
  
  #add the CI_85 values           
  student_melted <- merge(student_melted[,1:10], ci_table[,c("variable", "CI_85")], by="variable", all.X=T)
  
  
  
  #select the right student
  student <- student_melted[student_melted$StudentID==student_id,]
  
  
  #check if all 5 categories (Overall, ER, SPT, SPS, SC) are present, if not add, by hand
  if("OverallScore" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "OverallScore", value = 0, estim_true = 0, CI_85= 0))
  }
  
  if("ERScore" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "ERScore", value = 0, estim_true = 0, CI_85= 0))
  }
  
  if("SPTScore" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "SPTScore", value = 0, estim_true = 0, CI_85= 0))
  }
  
  if("SPSScore" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "SPSScore", value = 0, estim_true = 0, CI_85= 0))
  }
  
  if("SCScore" %in% student$variable == FALSE){
    student <- rbind(student, data.frame(StudentID = params$student_id, District = district_name, School = params$school_name,
                                         Grade = student$Grade[1], Timepoint = student$Timepoint[1], TeacherLast = params$teacher_name,
                                         LastName = student$LastName[1], variable = "SCScore", value = 0, estim_true = 0, CI_85= 0))
  }
  
  
  student$variable <- recode(student$variable, OverallScore = "Overall Score", ERScore = "Emotion Recognition", SPTScore = "Social Perspective-Taking", SPSScore= "Social Problem-Solving", SCScore= "Self Control")
  levels(student$variable) <- gsub(" ", "\n", levels(student$variable))  
  
  #rename value into score
  student <- student %>% mutate(Score = value)
  
  return(student)

}