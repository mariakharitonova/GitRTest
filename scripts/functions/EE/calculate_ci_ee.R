calculate_CI_ee <- function(){
 ci_table <- tibble(
   variable = c("OverallScore", "ERScore", "SPSScore", "SPTScore", "SCScore"),
   alpha = c(0.91, 0.87, 0.86, 0.76, 0.84)   #the reliabilities for EE modules 
 )
  
  ci_table$SEM <- round(sqrt(1-ci_table$alpha)*15,2)  #calculate SEM
  ci_table$CI_85 <- round(ci_table$SEM*1.44,1)                 #calculate the margin of the 85% CI
  
  return(ci_table)
  
}

