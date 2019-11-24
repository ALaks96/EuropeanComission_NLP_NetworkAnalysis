# GLHF

library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)
library(ndtv)
library(networkD3)
library(feather)
library(DT)

data <- read_feather('google_w2v_results_prototype1_v3.feather')
don <- read.csv('occupations_full.csv')
don <- don %>% 
  rename(Skills = Skill)

full_data <- read_feather('google_w2v_results_prototype1_full_v3.feather')
full_data_tbl <- full_data

# List of all the education programs from our data on four Belgian universities:
universities <- c("University of Antwerp","UCL","Uliege","VUB", "University of Latvia", "University of Malta")

valid_programs <- full_data %>%
	select(Program,Courses) %>%
	distinct() %>%
        group_by(Program) %>%
        mutate(Course_count = n()) %>%
        ungroup() %>%
        filter(Course_count > 5)

full_data <- full_data %>%
	filter(Similarity >= 0.082) %>%
	filter(total_skills_program > 15) %>%
	mutate(Similarity = round(Similarity,2)) %>%
	filter(Program %in% valid_programs$Program)

# Link programs to skills (through courses)
edgeList_1 <- data %>% 
  group_by(Program,Skills) %>% 
  summarize(Matches = n()) %>% 
  rename(from = Program,
         to = Skills) %>% 
  ungroup()

# Link skills to Jobs
edgeList_2 <- don %>%
  group_by(Occupation) %>%
  mutate(Matches = n()) %>%
  ungroup() %>%
  rename(from = Skills,
         to = Occupation)

# Put all the links in one dataframe
edgeList <- edgeList_1 %>%
  rbind(edgeList_2)

# Listing all the nodes from above links
programs <- full_data %>% 
  filter(Program %in% unique(edgeList$from)) %>% 
  select(Program, University) %>% 
  rename(name = Program, group = University) %>% 
  distinct()
skills_1 <- data.frame(name = unique(full_data$Skills), group = "skill") 
skills_2 <- data.frame(name = unique(don$Skills), group = "skill")
skills <- skills_1 %>% 
  rbind(skills_2) %>% 
  distinct()
occupations <- data.frame(name = unique(edgeList_2$to), group = "Profession")

# Putting all the nodes and their types in a dataframe

nodeList <- programs %>% 
  rbind(skills) %>% 
  rbind(occupations)


programs <- full_data %>%
  arrange(University) %>% 
  select(Program) %>%
  distinct()

program_titles <- programs$Program

# List of all ocupations/professions in our data:
occupations <- full_data %>%
	select(Occupation) %>%
	distinct() %>%
	arrange(Occupation)

occupation_titles <- occupations$Occupation

# Defining a click script for the network visualization
MyClickScript <- 'd3.select(this).select("circle").transition().duration(750).attr("r", 40)'
# MyClickScript <- 'alert("You clicked " + d.name + " which is in row " + (d.index + 1) +  " of your original R data frame");'

# Define UI for the platform

ui <- fluidPage(theme = shinytheme('sandstone'),
		title = 'Skills Map',
                
                navbarPage(tags$strong("Skills Map",
                                       style = 'font-size: 35px'),
                           
                           tabPanel("Welcome",
                                    mainPanel(htmlOutput(outputId = "welc",
                                                         style = "font-size: 20px"),
					img(src = "space.png", height = 130, width = 500),
					img(src = "ec.png", height = 130, width = 200))),
                           
                           tabPanel("Job to education explorer",
                                    style = "font-size: 20px",
                                    sidebarPanel(
                                                 style = "font-size: 16px",
                                      selectInput('occ1',
                                                  "Choose the job you're interested in!", 
                                                  choices = occupation_titles,
                                                  selected = 'energy engineer'),
                                      sliderInput('relo',
                                                  'Maximum number of unshared skills displayed',
                                                  min = 5,
                                                  max = 100,
                                                  value = 5),
				      h4("The Similarity metric in the table on the right is computed as the ratio of the number of 
					  skills shared between an Education Program and Job over the total skills needed in a Job
					  and yielded by a Program")),
                                    mainPanel(h3("The graph below shows the most relevant education program to your selected job"),
					      forceNetworkOutput(outputId = "graphjob_comp", height = "500px",width = "100%"),
                                              DT::dataTableOutput(outputId = "jobtable"))),
                           
                           
                           
                           tabPanel("Education to job explorer",
                                    style = "font-size: 20px",
                                    sidebarPanel(
                                                 style = "font-size: 16px",
                                      selectInput('ep1',
                                                  "Choose the education program you're interested in!", 
                                                  choices = program_titles,
                                                  selected = "Traduction et interpretation"),
                                      sliderInput('relep',
                                                  'Maximum number of unshared skills displayed',
                                                  min = 5,
                                                  max = 100,
                                                  value = 5),
                                      h4("The Similarity metric in the table on the right is computed as the ratio of the number of
                                          skills shared between an Education Program and Job over the total skills needed in a Job
                                          and yielded by a Program")),
                                    mainPanel(h3("The graph below shows the most relevant job to your selected education program"),
					      forceNetworkOutput(outputId = "graphep_comp", height = "500px",width = "100%"),
                                              DT::dataTableOutput(outputId = "eptable"))),
                           
                           tabPanel("Match 1 job & 1 program",
                                    style = "font-size: 20px",
                                    sidebarPanel(
                                                 style = "font-size: 16px",
                                      selectInput('occ2',
                                                  "Choose the job you're interested in!", 
                                                  choices = occupation_titles,
                                                  selected = 'psychology lecturer'),
                                      selectInput('ep2',
                                                  "Choose the education program you're interested in!", 
                                                  choices = program_titles,
                                                  selected = "Criminologie, a finalite"),
                                      h4("The Similarity metric in the table on the right is computed as the ratio of the number of
                                          skills shared between an Education Program and Job over the total skills needed in a Job
                                          and yielded by a Program")),
                                    mainPanel(h3("The graph below shows all the skills that your selected job and program have in common or not"),
					      forceNetworkOutput(outputId = "one_v_one", height = "500px",width = "100%"),
					      tags$strong("NOTE: Table will be empty if there were no matches", style = "font-size: 14px"),
                                              div(DT::dataTableOutput(outputId = "one_v_one_table"),style = "font-size: 80%; width: 80%"))),
                           
                           tabPanel("Graph explorer",
                                    style = "font-size: 24px",
                                    sidebarPanel(
                                                 style = "font-size: 16px",
                                                 selectInput('epexp',
                                                             "Choose the education program you're interested in!", 
                                                             choices = program_titles,
                                                             selected = "Master [120] in Business Engineering"),
                                                 sliderInput('relexpep',
                                                             'Lower the relevance of your search to get broader results',
                                                             min = 5,
                                                             max = 100,
                                                             value = 5)),
                                    mainPanel(h3("NOTE : The graph below shows other relevant jobs to your selected education program"),
					      forceNetworkOutput(outputId = "graphep", height = "500px",width = "100%"),
					      tags$strong('NOTE: If the graph freezes, reduce the slider to 5 and/or choose another program.', style = 'font-size: 14px'),
					      DT::dataTableOutput(outputId = "explore"))),                           
                           
			   tabPanel(tags$strong("Assistance", style = "font-size: 11px"),
                                    mainPanel(htmlOutput(outputId = "instr",
                                                         style = "font-size: 20px")))
                )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$instr <- renderUI({str0 = "If you need any assistance, please contact by mail the following adress : joelle.liegeois@ext.ec.europa.eu"
                            str1 = "This Shiny App is a proof of concept to demonstrate the value of using AI to support policy makers."
			    str01 = "This specific prototype uses Natural Language Processing to link jobs with education programs through common skills."
                            space = " "
                            str2 = "The data used is the ESCO dataset on one hand (https://ec.europa.eu/esco/portal/home) and data obtained from six university websites."
                           str3 = "The underlying model is as follows:"
			   str03 = "Jobs are linked to skills and each skill has a description."
			   str003 = "Universities have education programs, which are composed of courses. Each of the courses has a course description."
                           str4 = "This proof of concepts matches course descriptions with skill descriptions."
			   str04 = "When a course description and a skill description are similar, a connection is made."
			   str5 = "In this app we only visualize jobs, skills and education programs."
			   str05 = "Each of the tabs gives a different view on the data."
			   str005 = "You will find these results formatted in a graph fashion and rendered with a network visualization."
			   str0005 = "You can click and drag nodes to arrange the visualization to your needs,"
			   str00005 = "as well as choose among a few options to better suit your investigation."
			   str6 = "Job to education explorer tab"
			   str7 = "In the second tab, the graph is exposed from a job perspective. You must select a job on the left side of 
				   the shiny app among the list of possibilities in the drop down menu."
			   str07 = "You may use the slider to adjust the limit of the amount of output displayed in the graph."
			   str007 = "The graph will show the selected job and one or more education programs that have the most skills in common with your selected job."
			   str0007 = "All the skills that the job and the education program have in common will be shown. If you want to see more of the non-matching
				      skills, you can adjust the slider below the selection menu."
			   str00007 = "Underneath the graph you will find a list of all the programs that were linked to your selected 
				       job in order of decreasing similarity."
			   str8 = "Education to job explorer tab"
			   str9 = "The third tab shows a similar lay-out as the previous tab, but instead of selecting a job, you select an education program."
			   str09 = "Again the most linked programs will appear in the graph visualization. Other jobs which were linked to your selected program will appear 
				    in the table in decreasing order of similarity. The slider allows you to visualise skills which are not in common between the elements exposed in the network visualization"
			   str10 = "The graphs may show some odd results for certain jobs and educations. This has to do with the quality of the data, the matching
				    threshold that was chosen and the actual mismatch between the education and the job."
			   str11 = "Compare 1 job & 1 education tab"
			   str12 = "The fourth tab compares any job with any education program among their respective list."
			   str13 = "Graph explorer tab"
			   str14 = "The fifth tab was our first attempt to visualise the data. You can use it to explore our results."
			   str014 = "Choose a program, for which several top matches and links between skills and jobs will be shown."
			   str15 = "We hope this tool will suit you, there's room for improvement so any criticism and suggestions are more than welcome!"
                           HTML(paste(
                                      space,
                                      str1,
				      str01,
                                      space,
                                      str2,
                                      space,
                                      str3,
				      str03,
				      str003,
                                      space,
                                      str4,
				      str04,
				      space,
				      str5,
					str05,
					str005,
					str0005,
					str00005,                
                                      space,
				      str6,
				      space,
				      str7,
					str07,
					str007,
					str0007,
					str00007,
				      space,
				      str8,
				      space,
				      str9,
					str09,
				      space,
				      str10,
				      space,
				      str11,
				      space,
				      str12,
				      space,
				      str13,
				      space,
				      str14,
					str014,
				      space,
				      str15,
				      space,
				      str0,
					space,
					space,
					space,
				      sep = '<br/>'))})

  output$welc <- renderUI({
                        str0 = 'Welcome to this Shiny App!'
			str01 = "This app is a proof of concept developed for the European Commission."
			str001 = "By the use of AI (NLP and graphs), it visualises the relevance between education programs and jobs, through the skills described."
                        str1 = 'The different tabs above offer different views. You are now in the "welcome" tab.'
                        str2 = "The second tab allows you to choose a job. It visualises the job, job skills and 
				the education programs that teach the most of those skills in it's curriculum. "
                        str3 = 'The third tab allows you to choose an education program. The visualisation will show you the 
				chosen program, the skills learned, and the jobs that have the most skills in common.'
                        str4 = 'The fourth tab allows you to compare any job with any education program from those at our disposal.'
                        str5 = 'The fifth tab was our first attempt to visualise the data.'
			str6 = 'You can use it to explore our results.'
			str7 = 'For more details, you can go to the final tab: "assistance".'
                        space = ''
                        HTML(paste(space,
                                      str01,
				      str001,
				      space,
				      str1,
                                      space,
                                      str2,
                                      space,
                                      str3,
                                      space,
                                      str4,
                                        space,
                                        str5,
					str6,
					space,
					str7,
					space,
					space,
					space,
                                        sep = '<br/>'))
                        })

  ############################################### JOB PANEL ###############################################
  
  ############################################### COMPARISON ##############################################
  
  top_edge <- reactive({
    full_data %>%
      filter(Occupation %in% input$occ1) %>% 
      top_n(1,Similarity)
  })

  program_skill_edges <- reactive({
    top_edge() %>% 
      select(Program,Skills,Common_skills_program) %>% 
      rename(from = Program,
             to = Skills,
             Similarity = Common_skills_program) 
  })

  job_skill_edges <- reactive({
    top_edge() %>% 
      select(Skills, Occupation, Common_skills_job) %>% 
      rename(from = Skills,
             to = Occupation,
             Similarity = Common_skills_job) 
    })

  missing_program_skills <- reactive({
    full_data %>%
      filter(Program %in% top_edge()$Program) %>%
      select(Program,Skills,skill_weight_program) %>% 
      distinct() %>% 
      rename(from = Program,
             to = Skills,
             Similarity = skill_weight_program) %>% 
      sample_n(input$relo,replace = TRUE) 
  })

  missing_job_skills <- reactive({
    don %>%
      filter(Occupation %in% input$occ1) %>%
      distinct() %>%
      group_by(Occupation) %>% 
      mutate(Similarity = 1/n()) %>% 
      ungroup() %>% 
      rename(from = Skills,
             to = Occupation) %>%
      sample_n(input$relo,replace = TRUE) 
  })

filtered_edgeList5 <- reactive({
    program_skill_edges() %>%
      rbind(job_skill_edges()) %>%
      rbind(missing_job_skills()) %>%
      rbind(missing_program_skills())
    })

  filtered_nodeList5 <- reactive({
    nodeList %>%
      filter(name %in% filtered_edgeList5()$from |
             name %in% filtered_edgeList5()$to) %>%
      mutate(nodesize = case_when(group == "Profession" ~ '90',
                                  group %in% c("University of Antwerp","UCL","Uliege","VUB", "University of Latvia", "University of Malta") ~ '10',
                                  group == "skill" ~ '1',
                                  TRUE ~ as.character(0))
      )
  })

  filtered_edgeList6 <- reactive({
    filtered_edgeList5() %>%
      mutate(from.index = match(filtered_edgeList5()$from,
                                filtered_nodeList5()$name)-1) %>%
      mutate(to.index = match(filtered_edgeList5()$to,
                              filtered_nodeList5()$name)-1) %>%
      as.data.frame()
  })

  output$graphjob_comp <- renderForceNetwork(forceNetwork(
               Links = filtered_edgeList6(),
               Nodes = filtered_nodeList5(),
               Source = "from.index",
               Target = "to.index",
               Value = "Similarity",
               NodeID = "name",
               Nodesize = "nodesize",
               Group = "group",
               linkDistance = 200,
               fontSize = 30,
               opacity = 0.8,
	       charge = -90,
               zoom = F,
	       #colourScale = 'd3.scaleOrdinal().range(["#dc6900","#eb8c00","#e0301e","#a32020","#602320","#ff4e50","#fc913a","#740001","#ae0001","#eeba30","#d3a625","#000000"])' ,
	       colourScale = 'd3.scaleOrdinal().range(["#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"])',
	       #colourScale = 'd3.scaleOrdinal().range(["#602320","#a32020","#e0301e","#eb8c00","#dc6900","#eeba30"])',
               linkColour = "#afafaf",
               linkWidth = networkD3::JS("function(d) { return d.value*10; }"),
               legend = T,
	       bounded = T,
               clickAction = MyClickScript))

  ############################################## TABLE ##############################################
  
  
  job_tbl_data <- reactive({
    full_data %>% 
      filter(Occupation %in% input$occ1) %>% 
      select(Occupation,Program,University,Similarity,total_common_skills,total_skills_job) %>% 
      arrange(desc(Similarity)) %>%
      distinct() %>%
      mutate(Similarity = case_when(Similarity < 0.09 ~ "Low",
				    Similarity >= 0.09 & Similarity <= 0.1 ~ "Medium",
				    Similarity > 0.1 ~ "High",
				    TRUE ~ as.character("Unknown"))) %>%
      rename('Total common skills' = total_common_skills,
	     'Job skills count' = total_skills_job,
             Job = Occupation)
	 
  })
  
  output$jobtable <- DT::renderDataTable({
    job_tbl_data()
  })

  ############################################################# EDUCATION PANEL ##############################################################
  
  ############################################################### COMPARISON #################################################################
  
  top_edge1 <- reactive({
    full_data %>%
      filter(Program %in% input$ep1) %>% 
      top_n(1,Similarity)
  })
  
  program_skill_edges1 <- reactive({
    top_edge1() %>% 
      select(Program,Skills,Common_skills_program) %>% 
      rename(from = Program,
             to = Skills,
             Similarity = Common_skills_program) 
  })
  
  job_skill_edges1 <- reactive({
    top_edge1() %>%
      select(Skills, Occupation, Common_skills_job) %>%
      rename(from = Skills,
             to = Occupation,
             Similarity = Common_skills_job) 
  })
  
  missing_program_skills1 <- reactive({
    full_data %>%
      filter(Program %in% input$ep1) %>%
      select(Program,Skills,skill_weight_program) %>% 
      distinct() %>% 
      rename(from = Program,
             to = Skills,
             Similarity = skill_weight_program) %>% 
      sample_n(input$relep,replace = TRUE) 
  })
  
  missing_job_skills1 <- reactive({
    don %>%
      filter(Occupation %in% unique(top_edge1()$Occupation)) %>%
      distinct() %>%
      group_by(Occupation) %>%
      mutate(Similarity = 1/n()) %>%
      ungroup() %>% 
      rename(from = Skills,
             to = Occupation) %>%
      sample_n(input$relep,replace = TRUE) 
  })
  
  filtered_edgeList7 <- reactive({
    program_skill_edges1() %>%
      rbind(job_skill_edges1()) %>%
#     rbind(missing_job_skills1()) %>%
      rbind(missing_program_skills1()) %>%
      distinct() 
  })
  
  filtered_nodeList7 <- reactive({
    nodeList %>%
      filter(name %in% filtered_edgeList7()$from |
               name %in% filtered_edgeList7()$to) %>%
      mutate(nodesize = case_when(group == "Profession" ~ '10',
                                  group %in% c("University of Antwerp","UCL","Uliege","VUB", "University of Latvia", "University of Malta") ~ '90',
                                  group == "skill" ~ '1',
                                  TRUE ~ as.character(0))
             )
  })
  
  filtered_edgeList8 <- reactive({
    filtered_edgeList7() %>%
      mutate(from.index = match(filtered_edgeList7()$from,
                                filtered_nodeList7()$name)-1) %>%
      mutate(to.index = match(filtered_edgeList7()$to,
                              filtered_nodeList7()$name)-1) %>%
      as.data.frame()
  })
  
  output$graphep_comp <- renderForceNetwork(forceNetwork(
    Links = filtered_edgeList8(),
    Nodes = filtered_nodeList7(),
    Source = "from.index",
    Target = "to.index",
    Value = "Similarity",
    NodeID = "name",
    Nodesize = "nodesize",
    Group = "group",
    linkDistance = 200,
    fontSize = 30,
    opacity = 0.8,
    zoom = F,
    charge = -90,
    linkColour = "#afafaf",
    linkWidth = networkD3::JS("function(d) { return d.value*10; }"),
    #colourScale = 'd3.scaleOrdinal().range(["#dc6900","#eb8c00","#e0301e","#a32020","#602320","#ff4e50","#fc913a","#740001","#ae0001","#eeba30","#d3a625","#000000"])' ,
    colourScale = 'd3.scaleOrdinal().range(["#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"])',
    legend = T,
    bounded = T,
    clickAction = MyClickScript))

  ############################################################### EXPLORATION ################################################################

# Choosing one program
  filtered_programs3 <- reactive({
  edgeList %>%
    filter(from %in% input$epexp) %>%
    arrange(desc(Matches)) %>%
    head(input$relexpep)
  })

# Retrieving all the jobs sharing the skills defined above
  filtered_occupation3 <- reactive({
  edgeList %>%
    filter(from %in% as.character(filtered_programs3()$to)) %>%
    arrange(desc(Matches)) %>%
    head(input$relexpep)
  })

  filtered_missing_skills_exp <- reactive({
      edgeList %>% 
        filter(to %in% unique(filtered_occupation3()$to)) %>%
        head(input$relexpep)  
  })

# Binding above rows to define filtered edge list
  filtered_edgeList3 <- reactive({
  filtered_occupation3() %>%
    rbind(filtered_programs3()) %>%
    rbind(filtered_missing_skills_exp())
  })



## Defining filtered nodes

# Given edge list defined above, retrieve all the node names accordingly
  filtered_nodeList3 <- reactive({
  nodeList %>%
    filter(name %in% unique(filtered_edgeList3()$from) |
             name %in% unique(filtered_edgeList3()$to)) %>%
    mutate(nodesize = case_when(group == "Profession" ~ '10',
                                group %in% c("University of Antwerp","UCL","Uliege","VUB", "University of Latvia", "University of Malta") ~ '90',
                                group == "skill" ~ '1',
                                TRUE ~ as.character(0))
    )
  })

# Creating indexes for the node names
# filtered_nodeList4 <- reactive({
#   filtered_nodeList3() %>%
#     cbind(c(seq(1,nrow(filtered_nodeList3())) - 1))
# })

# Naming columns appropriately
#reactive({names(filtered_nodeList()) <- c("name","group","nodeID")})

# Re-defining indices of edges for the network
  filtered_edgeList4 <- reactive({
  filtered_edgeList3() %>%
    mutate(from.index = match(filtered_edgeList3()$from,
                              filtered_nodeList3()$name)-1) %>%
    mutate(to.index = match(filtered_edgeList3()$to,
                            filtered_nodeList3()$name)-1) %>%
      as.data.frame() 
  })
  
  output$graphep <- renderForceNetwork(forceNetwork(Links = filtered_edgeList4(),
                                                    Nodes = filtered_nodeList3(),
                                                    Source = "from.index",
                                                    Target = "to.index",
                                                    Value = "Matches",
                                                    NodeID = "name",
                                                    Nodesize = "nodesize",
                                                    Group = "group",
                                                    linkDistance = 200,
                                                    height = 1200,
                                                    width = 800,
                                                    fontSize = 30,
						    charge = -90,
                                                    opacity = 0.8,
                                                    zoom = F,
                                                    linkColour = "#afafaf",
                                                    linkWidth = networkD3::JS("function(d) { return d.value/15; }"),
                                                    #colourScale = 'd3.scaleOrdinal().range(["#dc6900","#eb8c00","#e0301e","#a32020","#602320","#ff4e50","#fc913a","#740001","#ae0001","#eeba30","#d3a625","#000000"])' ,
						    colourScale = 'd3.scaleOrdinal().range(["#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"])' ,
						    legend = T,
						    bounded = T,
                                                    clickAction = MyClickScript))

  explore_tbl_data <- reactive({
	full_data %>%
		filter(Program %in% input$epexp) %>%
		select(University, Program, Courses, Skills) %>%
		distinct()
  })

  output$explore <- DT::renderDataTable({
	explore_tbl_data()
  })

  ############################################## TABLE ##############################################
  
  
  ep_tbl_data <- reactive({
    full_data %>% 
      filter(Program %in% input$ep1) %>% 
      select(University,Program,Occupation,Similarity,total_common_skills,total_skills_program) %>% 
      arrange(desc(Similarity)) %>%
      distinct() %>%
      mutate(Similarity = case_when(Similarity < 0.09 ~ "Low",
                                    Similarity >= 0.09 & Similarity <= 0.1 ~ "Medium",
                                    Similarity > 0.1 ~ "High",
                                    TRUE ~ as.character("Unknown"))
	    ) %>%
      rename('Total common skills' = total_common_skills,
             'Program skills count' = total_skills_program,
             Job = Occupation)
    })
  
  output$eptable <- DT::renderDataTable({
    ep_tbl_data()
  })

############################################## 1 VS 1 ##############################################
  
  
  comp_top_edge_job <- reactive({
    full_data %>%
      filter(Occupation %in% input$occ2 & Program %in% input$ep2) %>% 
      select(Occupation,Skills,Similarity) %>% 
      rename(from = Skills,
             to = Occupation) %>% 
      distinct()
  })
  
  comp_top_edge_program <- reactive({
    full_data %>%
      filter(Occupation %in% input$occ2 & Program %in% input$ep2) %>% 
      select(Program,Skills,Similarity) %>% 
      rename(from = Program,
             to = Skills) %>% 
      distinct()
  })
  
  comp_missing_program_skills <- reactive({
    full_data %>%
      filter(Program %in% input$ep2) %>%
      select(Program,Skills,skill_weight_program) %>% 
      distinct() %>% 
      rename(from = Program,
             to = Skills,
             Similarity = skill_weight_program)
  })
  
  comp_missing_job_skills <- reactive({
    don %>%
      filter(Occupation %in% input$occ2) %>%
      distinct() %>%
      group_by(Occupation) %>% 
      mutate(Similarity = 1/n()) %>% 
      ungroup() %>% 
      rename(from = Skills,
             to = Occupation) 
  })
  
  comp_filtered_edgeList5 <- reactive({
    comp_top_edge_job() %>%
      rbind(comp_top_edge_program()) %>%
      rbind(comp_missing_program_skills()) %>%
      rbind(comp_missing_job_skills())
  })
  
  comp_filtered_nodeList5 <- reactive({
    nodeList %>%
      filter(name %in% comp_filtered_edgeList5()$from |
               name %in% comp_filtered_edgeList5()$to) %>%
      mutate(nodesize = case_when(group == "Profession" ~ '60',
                                group %in% c("University of Antwerp","UCL","Uliege","VUB", "University of Latvia", "University of Malta") ~ '60',
                                group == "skill" ~ '5',
                                TRUE ~ as.character(0))
             )
  })
  
  comp_filtered_edgeList6 <- reactive({
    comp_filtered_edgeList5() %>%
      mutate(from.index = match(comp_filtered_edgeList5()$from,
                                comp_filtered_nodeList5()$name)-1) %>%
      mutate(to.index = match(comp_filtered_edgeList5()$to,
                              comp_filtered_nodeList5()$name)-1) %>%
      as.data.frame()
  })
  
  output$one_v_one <- renderForceNetwork(forceNetwork(
    Links = comp_filtered_edgeList6(),
    Nodes = comp_filtered_nodeList5(),
    Source = "from.index",
    Target = "to.index",
    Value = "Similarity",
    NodeID = "name",
    Nodesize = "nodesize",
    Group = "group",
    linkDistance = 200,
    fontSize = 30,
    opacity = 0.8,
    zoom = F,
    bounded = T,
    charge = -90,
    linkColour = "#afafaf",
    linkWidth = networkD3::JS("function(d) { return d.value*10; }"),
    legend = T,
    #colourScale = 'd3.scaleOrdinal().range(["#dc6900","#eb8c00","#e0301e","#a32020","#602320","#ff4e50","#fc913a","#740001","#ae0001","#eeba30","#d3a625","#000000"])' ,
    colourScale = 'd3.scaleOrdinal().range(["#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"])',
    clickAction = MyClickScript))

  ############################################## TABLE ##############################################
  
  
  one_v_one_tbl_data <- reactive({
    full_data_tbl %>% 
      filter(Program %in% input$ep2 & Occupation %in% input$occ2) %>% 
      select(Occupation,Skills,Program,Courses,University,Similarity,total_common_skills,total_skills_job,total_skills_program) %>%
      mutate(Similarity = case_when(Similarity < 0.09 ~ "Low",
                                    Similarity >= 0.09 & Similarity <= 0.1 ~ "Medium",
                                    Similarity > 0.1 ~ "High",
                                    TRUE ~ as.character("Unknown"))
            ) %>%
      rename('Total common skills' = total_common_skills,
             'Job skills count' = total_skills_job,
	     'Program skills count' = total_skills_program,
	     Job = Occupation)
  })
  
  output$one_v_one_table <- DT::renderDataTable({
    one_v_one_tbl_data()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

