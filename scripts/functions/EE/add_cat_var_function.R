add_cat_vars <- function(datafile){
  
  datafile$overall.cat <- ifelse(datafile[,"OverallScore"] >= 115, "above", 
                                 ifelse((datafile[,"OverallScore"] < 115 & datafile[,"OverallScore"] >= 90), "meets",
                                        ifelse((datafile[,"OverallScore"] < 90 & datafile[,"OverallScore"] >=70), "below",
                                               ifelse(datafile[,"OverallScore"] < 70, "well-below",""))))
  
  datafile$er.cat <- ifelse(datafile[,"ERScore"] >= 115, "above", 
                            ifelse((datafile[,"ERScore"] < 115 & datafile[,"ERScore"] >= 90), "meets",
                                   ifelse((datafile[,"ERScore"] < 90 & datafile[,"ERScore"] >=70), "below",
                                          ifelse(datafile[,"ERScore"] < 70, "well-below",""))))
  
  datafile$spt.cat <- ifelse(datafile[,"SPTScore"] >= 115, "above", 
                             ifelse((datafile[,"SPTScore"] < 115 & datafile[,"SPTScore"] >= 90), "meets",
                                    ifelse((datafile[,"SPTScore"] < 90 & datafile[,"SPTScore"] >=70), "below",
                                           ifelse(datafile[,"SPTScore"] < 70, "well-below",""))))
  
  datafile$sps.cat <- ifelse(datafile[,"SPSScore"] >= 115, "above", 
                             ifelse((datafile[,"SPSScore"] < 115 & datafile[,"SPSScore"] >= 90), "meets",
                                    ifelse((datafile[,"SPSScore"] < 90 & datafile[,"SPSScore"] >=70), "below",
                                           ifelse(datafile[,"SPSScore"] < 70, "well-below",""))))
  
  datafile$sc.cat <- ifelse(datafile[,"SCScore"] >= 115, "above", 
                            ifelse((datafile[,"SCScore"] < 115 & datafile[,"SCScore"] >= 90), "meets",
                                   ifelse((datafile[,"SCScore"] < 90 & datafile[,"SCScore"] >=70), "below",
                                          ifelse(datafile[,"SCScore"] < 70, "well-below",""))))
  return(datafile)
  
  
  
}