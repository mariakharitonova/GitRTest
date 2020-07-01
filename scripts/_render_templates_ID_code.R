## EE and LE Integrated test

library(data.table)
library(flexdashboard)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(reshape2)
library(DT)
library(plotly)
library(crosstalk)
library(tidyverse)
library(xfun)
library(extrafont)
library(kableExtra)
library(knitr)
library(Cairo)
library(here)
 

#### SET PARAMETERS HERE #######
student_data_var = TRUE

version <- "EE"
datafile <- "TE_EE_2017_18_fict_ID_shorter_T1T2_IDs_2schools.csv"
#datafile <- "Momentous_2020-06-11_le.csv"

color_palette_dark_light = c('#006D2C', '#41AB5D', '#A1D99B', '#d3f5c9') #from darkest to lightest
color_palette_light_dark = c("#d3f5c9", "#A1D99B","#41AB5D","#006D2C") #from lightest to darkest


#### LOAD FUNCTIONS ######
if(version == "EE"){
  setwd(here("scripts", "functions", "EE"))
  files.sources = list.files(here("scripts", "functions", "EE"))
  sapply(files.sources, FUN=source)
} else if(version == "LE"){
  setwd(here("scripts", "functions", "LE"))
  files.sources = list.files(here("scripts", "functions", "LE"))
  sapply(files.sources, FUN=source)
} else {
  print("neither")
}

#### CHANGE WD and LOAD DATA ######
setwd("../..")

data <- read_csv(here("data", datafile))
data <- subset(data, data$School != "NA") #remove NAs from school

includeT2 <- (2 %in% data$Timepoint)

#### CREATE CROSS-WALK TABLES #####

data_datatable <- data.table(data)
school_teacher_xwalk <- data_datatable[ , .(number_of_students = .N), by = c("School", "schoolID", 
                                                                             "TeacherLast", "classID", "District","districtID")]
student_xwalk <- data_datatable[ , .(number_of_students = .N), by = c("StudentID", "School", "schoolID", 
                                                                       "TeacherLast", "classID", "District","districtID")]


#### SET PATHS HERE #######

district_code <- data$districtID[1]
district_name <- data$District[1]
district_name_nice <- tolower(gsub(" ", "_", district_name))

district_dir <- paste0("../reports/",district_name_nice, "/districts/", district_code, "/")


dir.create(paste0(district_dir), showWarnings = F, recursive = T)
dir.create(paste0(district_dir, "/pdf_district/"), showWarnings = F, recursive = T)


pdf_district_1 <- paste0(district_dir, "pdf_district/" ,district_name_nice,"_pdf.pdf")
pdf_district_2 <- paste0(district_dir, "pdf_district/" ,district_name_nice,"_pdf_T2.pdf")
pdf_district_change <- paste0(district_dir, "pdf_district/" ,district_name_nice,"_pdf_change.pdf")

html_district_1 <- paste0(district_dir, district_name_nice,"_summary.html")



#render PDF repots, one for each distrct, one for each school, one for each classroom
rmarkdown::render("district_PDF_T1.Rmd", clean = TRUE,
                  output_file = pdf_district_1,
                  params = list(file_path = data, version = version)) #, set_title =  paste0(district_name, " District, EE")


if(includeT2 == TRUE){
  rmarkdown::render("district_PDF_T2.Rmd", clean = TRUE,
                    output_file = pdf_district_2,
                    params = list(file_path = data, version = version))

  
  rmarkdown::render("district_PDF_change.Rmd", clean = TRUE,
                    output_file = pdf_district_change,
                    params = list(file_path = data, version = version)) #, set_title =  paste0(district_name, " District, EE"
  
}


#render HTML reports that will include the PDF report
rmarkdown::render("district_T1.Rmd", clean = TRUE,
                  flex_dashboard(logo="xsel-labs-logo-for-dark-bg.png",
                                 theme= "cosmo",
                                 orientation = "rows",
                                 css= "styles.css"),
                  output_file = html_district_1,
                  params = list(file_path = data, set_title = paste0(district_name, " District, ", version), 
                                student_data = student_data_var, includeT2 = includeT2, version = version))





all_schools <- unique(data$School)
#i<- all_schools[1]

for (i in all_schools) {
  
  control_row <- school_teacher_xwalk[School == i, ]
  school_code <- data$schoolID[1]
  school_name_nice <- tolower(gsub(" ", "_", i))
  
  school_row <- control_row %>% 
    subset(School == i)
  
  
  school_dir <- paste0(district_dir,"schools/", school_row$schoolID[1], "/")
  pdf_schools_dir <- paste0(school_dir, "pdf_schools/")
  
  html_school_1 <- paste0(school_dir, school_name_nice,"_index.html")
  pdf_schools_1 <- paste0(pdf_schools_dir, school_name_nice,".pdf")
  pdf_schools_2 <- paste0(pdf_schools_dir,school_name_nice,"_T2.pdf")
  pdf_schools_change <- paste0(pdf_schools_dir, school_name_nice,"_change.pdf")
  
  dir.create(dirname(school_dir), showWarnings = F, recursive = T)
  dir.create(pdf_schools_dir, showWarnings = F, recursive = T)
  
  
  rmarkdown::render("school_PDF_T1.Rmd",clean = TRUE,
                    output_file = pdf_schools_1,
                    params = list(school_name = i, file_path = data, version = version))
  
  if(includeT2 == TRUE){
    rmarkdown::render("school_PDF_T2.Rmd", clean = TRUE,
                      output_file = pdf_schools_2,
                      params = list(school_name = i, file_path = data, version = version))
    
    rmarkdown::render("school_PDF_change.Rmd", clean = TRUE,
                      output_file = pdf_schools_change,
                      params = list(school_name = i, file_path = data, version = version))
    
  }
  
  
  rmarkdown::render("school_T1.Rmd", clean = TRUE,
                    flex_dashboard(logo="xsel-labs-logo-for-dark-bg.png",
                                   theme= "cosmo",
                                   orientation = "rows",
                                   css= "styles.css",
                                   navbar = list(list(
                                     title = "Back to district level",
                                     icon = "fa-building",
                                       href = paste0("../../",district_name_nice,"_summary.html"),
                                     align = "right"))),
                    output_file = html_school_1,
                    params = list(file_path = data, school_name = i, set_title = i, student_data = student_data_var, 
                                  includeT2 = includeT2, version = version))

  
  #all_classes <- unique(control_row$classID)
  all_classes <- unique(control_row$TeacherLast)
  #j <- all_classes[1]
  
  for (j in all_classes){
    
    #class_code <- data$classID[1]
    teacher_name_nice <- tolower(gsub(" ", "_", j))
    
    class_row <- control_row %>% 
      subset(TeacherLast == j)
    
    class_dir <- paste0(school_dir,"classrooms/", class_row$classID, "/")
    pdf_class_dir <- paste0(class_dir, "pdf_classrooms/")
    
    html_class_1 <- paste0(class_dir, teacher_name_nice,"_index.html")
    pdf_class_1 <- paste0(pdf_class_dir, teacher_name_nice,".pdf")
    pdf_class_2 <- paste0(pdf_class_dir,teacher_name_nice,"_T2.pdf")
    pdf_class_change <- paste0(pdf_class_dir, teacher_name_nice,"_change.pdf")
    
    dir.create(dirname(class_dir), showWarnings = F, recursive = T)
    dir.create(pdf_class_dir, showWarnings = F, recursive = T)
    
  
    rmarkdown::render("classroom_PDF_T1.Rmd", clean = TRUE,
                      output_file = pdf_class_1,
                      params = list(teacher_name = j, school_name = i, file_path = data, version = version))
    
    
    if(includeT2 == TRUE){
      rmarkdown::render("classroom_PDF_T2.Rmd", clean = TRUE,
                        output_file = pdf_class_2,
                        params = list(teacher_name = j, school_name = i, file_path = data, version = version, 
                                      set_title =  paste0(district_name, " District, EE")))
      
      
      rmarkdown::render("classroom_PDF_change.Rmd", clean = TRUE,
                        output_file = pdf_class_change,
                        params = list(teacher_name = j, school_name = i, file_path = data, version = version,
                                      set_title =  paste0(district_name, " District, EE")))

      
    }
    
    
    rmarkdown::render("classroom_T1.Rmd", clean = TRUE,
                      flex_dashboard(logo="xsel-labs-logo-for-dark-bg.png",
                                     theme= "cosmo",
                                     #vertical_layout = "scroll",
                                     orientation = "rows",
                                     css= "styles.css",
                                     navbar = list(list(
                                       title = "Back to school level",
                                       icon = "fa-building-o",
                                       href = paste0("../../",school_name_nice,"_index.html"),
                                       align = "right"))),
                      output_file = html_class_1,
                      params = list(file_path = data, school_name = i, teacher_name = j,
                                    set_title = paste0(j," (Classroom), ", i, " (School)"), 
                                    student_data = student_data_var, includeT2 = includeT2, version = version))
    
    control_student <- student_xwalk[School == i & TeacherLast == j, ]
    all_students <- unique(control_student$StudentID)
    #k <- all_students[1]
    
    for (k in all_students){
      
      #class_dir <- paste0(school_dir,"classrooms/", class_code, "/")
      pdf_student_dir <- paste0(class_dir, "students/")
      dir.create(pdf_student_dir, showWarnings = F, recursive = T)
      
      student_name_nice <- tolower(gsub(" ", "_", k))
     
      pdf_student <- paste0(pdf_student_dir, student_name_nice,".pdf")
     
      rmarkdown::render("individual_report_time_pdf.Rmd", clean = TRUE,
                        output_file =  pdf_student,
                        params = list(teacher_name = j, school_name = i, student_id = k, file_path = data,
                                      version = version))
      
      
    }
    

  }
}

unlink("*.log")
