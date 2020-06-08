#a function that creates a table where each row is a SCHOOL
make_school_links <- function(datafile, pagenum){
  district_bySchool <- datafile %>%
    group_by(School) %>%
    summarise (count_kids =  n(),
               mean_SEL = round(mean(SEL.SS, na.rm=T),0),
               mean_UO = round(mean(UO.SS, na.rm=T),0),
               mean_SPS = round(mean(SPS.SS, na.rm=T),0),
               mean_SC = round(mean(SC.SS, na.rm=T),0))
  
  names(district_bySchool) <- c("School", "Number of children","Overall SEL", 
                                "Understanding Others",
                                "Social Problem-Solving",
                                "Self Control")
  
  
  if(pagenum == 1){
    school_links <- district_bySchool %>% 
      mutate(School = paste0("<a href='schools/", tolower(gsub(" ", "_", School)), "_index.html'>", School, "</a>"))
  }
  else if(pagenum == 2){
    school_links <- district_bySchool %>% 
      mutate(School = paste0("<a href='schools/", tolower(gsub(" ", "_", School)), "_index.html#timepoint-2'>", School, "</a>"))
  }
  
  
  return(school_links)
}