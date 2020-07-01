calculate_CI_le <- function(){
 ci_table <- tibble(
   variable = c("SEL.SS", "UO.SS", "SPS.SS", "SC.SS"),
   alpha = c(0.87, 0.80, 0.8, 0.8)   #the reliabilities for LE modules 
 )
  
  ci_table$SEM <- round(sqrt(1-ci_table$alpha)*15,2)           #calculate SEM
  ci_table$CI_85 <- round(ci_table$SEM*1.44,1)                 #calculate the margin of the 85% CI
  
  return(ci_table)
  
}

