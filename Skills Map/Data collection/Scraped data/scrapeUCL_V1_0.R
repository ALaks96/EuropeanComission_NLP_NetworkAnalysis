# Scraper for  UCL 

# these scrapers are based on a visual exploration of the HTML source of example pages
# As such, identification of location of the course content, name and master is possible.

# load libraries ####
library(RCurl)
library(XML)
library(gdata)
library(gtools)
library(selectr)
library(xml2)
library(rvest)
library(purrr)

# plus: https://uclouvain.be/en-prog-2018-lafr1ba  : connects to jobs! a way to verify the links! 

# get the course list 
library(readxl)
UCL_ENG_COURSE_LIST <- read_excel("C:/PWC projects/European Commission/Data/UCL_ENG_COURSE_LIST.xlsx", col_names = FALSE)

## extract the course list 
UCL_ENG_COURSE_LIST$code <- paste0(strsplit(UCL_ENG_COURSE_LIST$X__1, " ")[[1]][1],strsplit(UCL_ENG_COURSE_LIST$X__1, " ")[[1]][2])
for (i in 1:nrow(UCL_ENG_COURSE_LIST)){
  UCL_ENG_COURSE_LIST$code[i] <- paste0(strsplit(UCL_ENG_COURSE_LIST$X__1, " ")[[i]][1],strsplit(UCL_ENG_COURSE_LIST$X__1, " ")[[i]][2])
}

# make the structure for the output dataframe
course_UCL <- data.frame(Course_name = "name",
                        Course_content = "content", 
                        Course_Prerequisites = "prerequisites",
                        Course_Main_themes = "Main_themes",
                        Course_Main_Aims = "Main_Aims",                       
                        Course_master = "master", 
                        stringsAsFactors=FALSE) 

### the scraping function ####

scrape_UCL <- function(a_UCL_page=a_UCL_page , course_UCL=course_UCL){
  course_df_b <- course_UCL
  page_UCL<- a_UCL_page  %>%   html_nodes("div") %>% html_text()
  page_UCL<-trimws(gsub("\r?\n|\r", " ", page_UCL))
  page_UCL_df<- as.data.frame(page_UCL,stringsAsFactors=F)
  # content, prerequisits, main themes, Aims, content
  a_Course_content <- page_UCL_df[match("Content",page_UCL)+1,1] 
  a_Course_Prerequisites <- page_UCL_df[match("Prerequisites",page_UCL)+1,1]
  a_Course_Main_themes <- page_UCL_df[match("Main themes",page_UCL)+1,1] 
  a_Course_Main_Aims <- page_UCL_df[match("Aims",page_UCL)+1,1]
  # course name
  a_Course_name <-  a_UCL_page   %>%   html_nodes("h1") %>%   html_text()
  # course masters
  a_masters <- a_UCL_page  %>%   html_nodes('.col-sm-5')  %>% html_text()
  a_Course_masters <- a_masters[2:length(a_masters)]
  # check if the course has no master
  if (length(a_Course_masters)==0){
    a_Course_masters <- "free elective"
  }
  for(i in 1:length(a_Course_masters)){
    course_UCL$Course_name[1] <- a_Course_name
    course_UCL$Course_content[1] <- a_Course_content
    course_UCL$Course_Prerequisites[1] <-  a_Course_Prerequisites 
    course_UCL$Course_Main_themes[1] <- a_Course_Main_themes 
    course_UCL$Course_Main_Aims[1] <- a_Course_Main_Aims
    course_UCL$Course_master[1] <- a_Course_masters[i]
    course_df_b <- rbind(course_df_b,course_UCL)
  }
  return(course_df_b[2:nrow(course_df_b),])
}


# apply the function to all the courses
a_UCL_page <- read_html(paste0("https://uclouvain.be/en-cours-2018-",UCL_ENG_COURSE_LIST$code[1]))
a_course_UCL <- scrape_UCL(a_UCL_page , course_UCL)
for(j in 2:nrow(UCL_ENG_COURSE_LIST)){
  a_UCL_page <- try(read_html(paste0("https://uclouvain.be/en-cours-2018-",UCL_ENG_COURSE_LIST$code[j] )),
                    silent= T)
  if(class(a_UCL_page)[1] == "xml_document"){
    a_course_full_2 <- scrape_UCL(a_UCL_page , course_UCL)
    a_course_UCL<- smartbind(a_course_UCL,a_course_full_2)
  }
}

# a bit of cleaning
a_course_UCL<- unique(a_course_UCL) #1575 

write.csv(a_course_UCL, file = "courses_UCL_all.csv", row.names = FALSE)

