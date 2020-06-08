#a function that creates a table where each row is a SCHOOL
make_school_links <- function(datafile, pagenum){
  district_bySchool <- datafile %>%
    group_by(schoolID, School) %>%
    summarise (count_kids =  n(),
               mean_overall = round(mean(OverallScore, na.rm=T),0),
               mean_er = round(mean(ERScore, na.rm=T),0),
               mean_spt = round(mean(SPTScore, na.rm=T),0),
               mean_sps = round(mean(SPSScore, na.rm=T),0),
               mean_sc = round(mean(SCScore, na.rm=T),0))
  
  names(district_bySchool) <- c("schoolID", "School","Number of children","Overall Score", 
                                "Emotion Recognition",
                                "Social Perspective-Taking", "Social Problem-Solving", "Self Control")
  
  if(pagenum == 1){
    school_links <- district_bySchool %>% 
      mutate(School = paste0("<a href='schools/", schoolID, "/",  
                             tolower(gsub(" ", "_", School)), "_index.html'>", School, "</a>")) %>% 
      ungroup(schoolID) %>% 
      select(-1)
  }
  else if(pagenum == 2){
    school_links <- district_bySchool %>% 
      mutate(School = paste0("<a href='schools/",  schoolID, "/", 
                             tolower(gsub(" ", "_", School)), "_index.html#timepoint-2'>", School, "</a>")) %>% 
      ungroup(schoolID) %>% 
      select(-1)
  }
  
  
  return(school_links)
}