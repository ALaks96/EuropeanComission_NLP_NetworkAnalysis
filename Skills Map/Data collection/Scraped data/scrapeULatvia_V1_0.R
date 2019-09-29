## scrape Latvia

# studies/study-process/courses/courses/

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
library(plyr)

course_ULat <- data.frame(Course_name = "name",
                         Course_content = "content", 
                         Course_code = "code",
                         Course_master = "master", 
                         stringsAsFactors=FALSE) 

course_ULat_all <- course_ULat


# https://www.lu.lv/en/nc/studies/study-process/courses/courses/?tx_lustudycatalogue_pi1%5B%40widget_0%5D%5BcurrentPage%5D=2

# loop over these pages to get all the courses

for(i in 1:427){
  a_ULat_course_list <- read_html(paste0("https://www.lu.lv/en/nc/studies/study-process/courses/courses/?tx_lustudycatalogue_pi1%5B%40widget_0%5D%5BcurrentPage%5D=",i))
  courses_ULat <- a_ULat_course_list  %>%    html_nodes("tr") #  %>% html_text()
    for(j in 3*(1:(length(courses_ULat)/3))-2){
    a_ULat_course <- courses_ULat[j] %>%    html_nodes("div")  %>% html_text()
    a_ULat_course <-trimws(gsub("\r?\n|\r", " ", a_ULat_course))
    a_ULat_course <-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", a_ULat_course, perl=TRUE)
    a_ULat_course <- as.data.frame(a_ULat_course,stringsAsFactors=F)
    a_ULat_course <- strsplit(a_ULat_course[1,1]," ")
    a_ULat_course <- as.data.frame(a_ULat_course,stringsAsFactors=F)
    course_ULat$Course_code <- a_ULat_course[4,]
    
    a_ULat_co_content <- courses_ULat[j] %>%    html_nodes("p")  %>% html_text()
    course_ULat$Course_content <-  toString(a_ULat_co_content[3:length(a_ULat_co_content)])
    course_ULat$Course_name <-  a_ULat_co_content[1]
    course_ULat_all <- rbind(course_ULat_all,course_ULat )

  }
}

course_ULat_all <- unique(course_ULat_all)


## The links to studies


# https://www.lu.lv/en/nc/studies/study-process/courses/programme-search/
# https://www.lu.lv/en/nc/studies/study-process/courses/programme-search/?tx_lustudycatalogue_pi1%5B%40widget_0%5D%5BcurrentPage%5D=3
# loop over these pages to get the studies

a_ULat_edu_list <- read_html(paste0("https://www.lu.lv/en/nc/studies/study-process/courses/programme-search/?tx_lustudycatalogue_pi1%5B%40widget_0%5D%5BcurrentPage%5D=",1))
courses_ULat <- a_ULat_edu_list %>%    html_nodes("tbody")  %>%    html_nodes("tr") #  %>% html_text()
links_edu_ULat <- courses_ULat  %>% html_nodes("a") %>% html_attr("href")
links_edu_ULat_studies <- as.data.frame(links_edu_ULat,stringsAsFactors=F)

for(i in 2:24){
  a_ULat_edu_list <- read_html(paste0("https://www.lu.lv/en/nc/studies/study-process/courses/programme-search/?tx_lustudycatalogue_pi1%5B%40widget_0%5D%5BcurrentPage%5D=",i))
  courses_ULat <- a_ULat_edu_list %>%    html_nodes("tbody")  %>%    html_nodes("tr") #  %>% html_text()
  links_edu_ULat <- courses_ULat  %>% html_nodes("a") %>% html_attr("href")
  links_edu_ULat_df <- as.data.frame(links_edu_ULat,stringsAsFactors=F) 
  links_edu_ULat_studies <- rbind(links_edu_ULat_studies,links_edu_ULat_df)
}

# links_edu_ULat_studies
a_study_list <- read_html(paste0("https://www.lu.lv/",links_edu_ULat_studies[1,1]))

a_study_list_ULat <- a_study_list %>%    html_nodes("main")  %>%    html_nodes("table") #  
a_study_ULat <- a_study_list_ULat %>% html_nodes("tr") %>% html_nodes("td") %>% html_text() # %>% html_attr("td") #  %>% html_text()
a_study_ULat_df <- as.data.frame(a_study_ULat,stringsAsFactors=F) 

a_study_courses <- subset(a_study_ULat_df,a_study_ULat_df$a_study_ULat %in% course_ULat_all$Course_code)
a_study_courses$program <- a_study_list %>%    html_nodes("h1")  %>% html_text()
colnames(a_study_courses)[1] <- "Course_code"

full_list <- join(a_study_courses,course_ULat_all)

for( i in 2:nrow(links_edu_ULat_studies)){
  a_study_list <- read_html(paste0("https://www.lu.lv/",links_edu_ULat_studies[i,1]))
  
  a_study_list_ULat <- a_study_list %>%    html_nodes("main")  %>%    html_nodes("table") #  
  a_study_ULat <- a_study_list_ULat %>% html_nodes("tr") %>% html_nodes("td") %>% html_text() # %>% html_attr("td") #  %>% html_text()
  a_study_ULat_df <- as.data.frame(a_study_ULat,stringsAsFactors=F) 
  
  a_study_courses <- subset(a_study_ULat_df,a_study_ULat_df$a_study_ULat %in% course_ULat_all$Course_code)
  if(nrow(a_study_courses)>0){  
    a_study_courses$program <- a_study_list %>%    html_nodes("h1")  %>% html_text()
    colnames(a_study_courses)[1] <- "Course_code"
    full_list_part <- join(a_study_courses,course_ULat_all)
    full_list <- rbind(full_list,full_list_part)
  }
}


# be sure to have no duplicates
ULatvia_scraped <- unique(full_list)

# write the results to csv for the next step : NLP 

write.csv(ULatvia_scraped, file = "courses_ULatvia_all.csv", row.names = FALSE)



# https://www.lu.lv/en/nc/studies/study-process/courses/programme-search/?tx_lustudycatalogue_pi1%5Bprogram%5D=20901&tx_lustudycatalogue_pi1%5Baction%5D=detail&tx_lustudycatalogue_pi1%5Bcontroller%5D=Course&cHash=79979bf2536805a44c6efc16ce2263a2
# gives the studies + courses
  a_ULat_edu_spec  %>%    html_nodes("tr")%>%    html_nodes("td") #  %>% html_text()
  

