# draft scraper caliweb

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

# make the structure for the output dataframe

course_df <- data.frame(Course_name = "name",
                           Course_content = "content", 
                           Course_skills = "skills",
                           Course_master = "master", 
                           stringsAsFactors=FALSE) 

# the main scraping function : 

### the scraping function ####
get_a_course <- function(p_nr=p_nr , course_df=course_df){
  course_df_b <- course_df 
  a_page_C <- try(read_html(
    paste0("https://caliweb.cumulus.vub.ac.be/?page=course-offer&id=",p_nr,"&anchor=1&target=pr&year=1819&language=en&output=html")),
    silent= T
    )
  if(a_page_C != "Error in doc_parse_raw(x, encoding = encoding, base_url = base_url, as_html = as_html,  : \n  Failed to parse text\n" ){
    a_text_dd <- a_page_C  %>%     html_nodes("dd") %>%     html_text()
    a_text_dt <- a_page_C  %>%     html_nodes("dt") %>%     html_text()
    a_Course_content <- a_text_dd[match("Course Content",a_text_dt)]
    a_Course_skills <- a_text_dd[match("Learning Outcomes",a_text_dt)]
    a_Course_name <-  a_page_C  %>%   html_nodes("title") %>%   html_text()
    a_masters <- a_page_C  %>%
      html_nodes("p") 
    the_masters <- a_masters[length(a_masters)]
    a_Course_masters <- the_masters %>% html_nodes("a") %>% html_text()
    if (length(a_Course_masters)==0){
      a_Course_masters <- "free elective"
    }
    for(i in 1:length(a_Course_masters)){
      course_df$Course_name[1] <- a_Course_name
      course_df$Course_content[1] <- a_Course_content
      course_df$Course_skills[1] <- a_Course_skills
      course_df$Course_master[1] <- a_Course_masters[i]
      course_df_b <- rbind(course_df_b,course_df)
    }
  }
  return(course_df_b[2:nrow(course_df_b),])
}

# make a first dataframe
p_nr <- "009000"
a_course_full <- get_a_course(p_nr,course_df)
p_nr <- "009003"
a_course_full_2 <- get_a_course(p_nr,course_df)

a_course_full<- smartbind(a_course_full,a_course_full_2)
a_course_full

# loop over the pages
for(j in 1:12000){
  a_nr <- 0000 + j
  p_nr <- paste0("00",a_nr)
  a_course_full_2 <- get_a_course(p_nr,course_df)
  a_course_full<- smartbind(a_course_full,a_course_full_2)
}
# timing less than  an hour.

### data cleaning ###
courses_VUB_all <- a_course_full

# remove 404
courses_VUB_all <- subset(courses_VUB_all, courses_VUB_all$Course_name != '404') #14483 remaining
# remove "under construction"
courses_VUB_all <- subset(courses_VUB_all, courses_VUB_all$Course_content != 'under construction') #14385 remaining
# '?' '..........................' or more points,  ^[:alpha:]

courses_VUB_all <- subset(courses_VUB_all, str_count(courses_VUB_all$Course_content, "[[:alpha:]]")>0 ) #13805 remaining

courses_VUB_all <- unique(courses_VUB_all) # 13431 remaining
length(unique(courses_VUB_all$Course_name)) # 3141 courses
length(unique(courses_VUB_all$Course_master)) # 507 masters

write.csv(courses_VUB_all, file = "courses_VUB_all.csv", row.names = FALSE)
