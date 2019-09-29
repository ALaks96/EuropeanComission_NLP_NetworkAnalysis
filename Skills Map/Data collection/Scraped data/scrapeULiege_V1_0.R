## Scraper for ULiege 

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


## the course list course ####
# it's a part from this link : https://www.programmes.uliege.be/cocoon/20182019/en/recherche.html# 
## these aspects are taken : <a href="/cocoon/20182019/en/formations/P1CLAS01.html"> 
a_UL_course_list <- read_html("courses_VULIEGE_all.html")

# extract the course codes
courses_UL<- a_UL_course_list  %>% 
  html_nodes(".u-courses-results__row__cell--title") %>%   html_text()
courses_UL<- trimws(gsub("\r?\n|\r", " ", courses_UL))
courses_UL<- as.data.frame(courses_UL,stringsAsFactors=F)


# make the structure for the output dataframe
course_UL <- data.frame(Course_name = "name",
                        Course_content = "content", 
                        Course_Main_Aims = "Main_Aims",                       
                        Course_master = "master",
                        Course_Code = "course_code",
                        stringsAsFactors=FALSE) 

### scraper function ####
scrape_UL <- function(a_UL_page=a_UL_page , course_UL=course_UL){
  page_UL<- a_UL_page  %>%   html_nodes("section") %>% html_text()
  page_UL_df <- as.data.frame(page_UL,stringsAsFactors=F)
  # content # get the course content from the page 
  a_Course_content_UL <- page_UL_df[max(grep("Learning unit contents", page_UL_df[,1])+1) ,1]
  # if the content is not found, we need to have an empty cell
  if(a_Course_content_UL == "Learning outcomes of the learning unit\n"){
    a_Course_content_UL <- c(NA)
  }
  course_UL$Course_content <- a_Course_content_UL
  # Main_Aims # this is extra information that serves as course content
  a_Course_aims_UL <- page_UL_df[max(grep("Learning outcomes of the learning unit", page_UL_df[,1])+1) ,1]
  if(a_Course_aims_UL == "Prerequisite knowledge and skills\n"){
    a_Course_aims_UL <- c(NA)
  }
  course_UL$Course_Main_Aims <- a_Course_aims_UL
  # course name
  a_Course_name <-  a_UL_page  %>%   html_nodes("h1") %>%   html_text()
  course_UL$Course_name <-  a_Course_name
  # course code
  course_UL$Course_Code <- courses_UL$courses_UL[j]
  return(course_UL)
}


### use the function for all the courses in the course list
j <- 1
a_UL_page <- try(read_html(
  paste0("https://www.programmes.uliege.be/cocoon/20182019/en/cours/",courses_UL$courses_UL[j],".html" )),
  silent= T)
a_course_UL <- scrape_UL(a_UL_page , course_UL)

for(j in 2:nrow(courses_UL)){
  a_UL_page <- try(read_html(
    paste0("https://www.programmes.uliege.be/cocoon/20182019/en/cours/",courses_UL$courses_UL[j],".html" )),
    silent= T)
  if(class(a_UL_page)[1] == "xml_document"){
    a_course_full_2 <- scrape_UL(a_UL_page , course_UL)
    a_course_UL<- smartbind(a_course_UL,a_course_full_2)
  }
}

# we still don't have the name of the programs, so we need to attach them to the courses in order to make the graph

# Bachelors list and master list to get the program connected
UL_edu_page <- read_html("https://www.programmes.uliege.be/cocoon/recherche.html?source=formation")  
page_edu_UL<- UL_edu_page  %>%   html_nodes("tbody") %>% html_nodes("tr") %>% html_text()

links_edu_UL <- UL_edu_page  %>% html_nodes("a") %>% html_attr("href")
links_edu_UL<- as.data.frame(links_edu_UL,stringsAsFactors=F)
links_edu_UL<- as.data.frame(links_edu_UL[12:444,1],stringsAsFactors=F)

# replace the formations with # programs
for( i in 1:nrow(links_edu_UL)){
  links_edu_UL[i,1] <- gsub("formations", "programmes", links_edu_UL[i,1])
  links_edu_UL[i,1] <- gsub(".html", "_C.html", links_edu_UL[i,1])
}

# get one program completed
i <- 1 
UL_prog_page <- read_html(paste0("https://www.programmes.uliege.be",links_edu_UL[i,1])) 
page_prog_UL<- UL_prog_page  %>%   html_nodes("tr") %>% html_nodes(".u-link") %>% html_text()

# Now I have to link this education track courses to the courses list
page_prog_UL <- as.data.frame(page_prog_UL,stringsAsFactors=F)
colnames(page_prog_UL) <- c("Course_Code")
page_prog_UL$master <- UL_prog_page  %>%   html_nodes("h1") %>% html_text()
page_prog_UL$code_html  <- links_edu_UL[i,1]

# get all the courses with the scraped information attached to the master. 
test_edu <- join(page_prog_UL, a_course_UL, match = "all" )

# get all the other prorams
# this code is not complete, as it can't handle some errors :
### Error in open.connection(x, "rb") : HTTP error 500.  : means that the program doesn't exist for the year 2018/2019
# when an error occured, the code was changed manually (i <- i+1 and continue with the rest)
for( i in 2:nrow(links_edu_UL)){
  UL_prog_page <- read_html(paste0("https://www.programmes.uliege.be",links_edu_UL[i,1])) 
  page_prog_UL<- UL_prog_page  %>%   html_nodes("tr") %>% html_nodes(".u-link") %>% html_text()
  page_prog_UL <- as.data.frame(page_prog_UL,stringsAsFactors=F)
  if(nrow(page_prog_UL)>0){
    colnames(page_prog_UL) <- c("Course_Code")
    page_prog_UL$master <- UL_prog_page  %>%   html_nodes("h1") %>% html_text()
    page_prog_UL$code_html  <- links_edu_UL[i,1]
    new_matches <- join(page_prog_UL, a_course_UL, match = "all" )
    test_edu <- smartbind(test_edu,new_matches)
  }
}

# be sure to have no duplicates
ULiege_scraped <- unique(test_edu)

# write the results to csv for the next step : NLP 

write.csv(ULiege_scraped[,1:6], file = "courses_Uliege_all.csv", row.names = FALSE)
  