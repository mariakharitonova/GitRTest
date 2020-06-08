#add categorical variables (above, meets, etc.)
add_cat_vars <- function(datafile) {
  
  datafile$SEL.SS.cat <- ifelse(datafile[,"SEL.SS"] >= 115, "above", 
                                ifelse((datafile[,"SEL.SS"] < 115 & datafile[,"SEL.SS"] >= 90), "meets",
                                       ifelse((datafile[,"SEL.SS"] < 90 & datafile[,"SEL.SS"] >=70), "below",
                                              ifelse(datafile[,"SEL.SS"] < 70, "well-below",""))))
  
  datafile$UO.SS.cat <- ifelse(datafile[,"UO.SS"] >= 115, "above", 
                               ifelse((datafile[,"UO.SS"] < 115 & datafile[,"UO.SS"] >= 90), "meets",
                                      ifelse((datafile[,"UO.SS"] < 90 & datafile[,"UO.SS"] >=70), "below",
                                             ifelse(datafile[,"UO.SS"] < 70, "well-below",""))))
  
  datafile$SPS.SS.cat <- ifelse(datafile[,"SPS.SS"] >= 115, "above", 
                                ifelse((datafile[,"SPS.SS"] < 115 & datafile[,"SPS.SS"] >= 90), "meets",
                                       ifelse((datafile[,"SPS.SS"] < 90 & datafile[,"SPS.SS"] >=70), "below",
                                              ifelse(datafile[,"SPS.SS"] < 70, "well-below",""))))
  
  
  datafile$SC.SS.cat <- ifelse(datafile[,"SC.SS"] >= 115, "above", 
                               ifelse((datafile[,"SC.SS"] < 115 & datafile[,"SC.SS"] >= 90), "meets",
                                      ifelse((datafile[,"SC.SS"] < 90 & datafile[,"SC.SS"] >=70), "below",
                                             ifelse(datafile[,"SC.SS"] < 70, "well-below",""))))
  
  
  return(datafile)
  
}