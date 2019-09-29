# Created by Alexis Laks

library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)
library(ndtv)
library(networkD3)


data <- read.csv('NLP_results_prototype2/google_w2v_results_prototype2.csv')

# Defining edges for the graph

data <- data %>% 
  mutate(Courses = case_when(Courses == "DD2423 Image Analysis and Computer Vision" ~ "Image Analysis and Computer Vision",
                             Courses == "DD2425 Robotics and Autonomous Systems" ~ "Robotics and Autonomous Systems",
                             Courses == "OPERATIONS RESEARCH" ~ "Operations Research",
                             Courses == "Statistical Methods in Data Mining" ~ "Data Mining",
                             Courses == "A Network Tour of Data Science" ~ "Topics in Networks and Distributed Systems",
                             Courses == "Advanced Machine Learning" ~ "Machine Learning",
                             Courses == "Advanced topics on privacy enhancing technologies" ~ "Privacy Enhancing Technologies",
                             Courses == "Cloud Computing and Virtualization" ~ "Big Data",
                             Courses == "DD2424 Deep Learning in Data Science" ~ "Deep Learning",
                             Courses == "DD2437 Artificial Neural Networks and Deep Architectures" ~ "Artificial Neural Networks",
                             Courses == "Deep Learning " ~ "Deep Learning",
                             Courses == "Distributed information systems" ~ "Database Systems",
                             Courses == "Energy-Efficient Parallel Computing Systems for Data Analytics" ~ "Big Data",
                             Courses == "How To Write Fast Numerical Code" ~ "Programming Paradigms",
                             Courses == "Information security and privacy" ~ "Computer and network security",
                             Courses == "Introduction to natural language processing" ~ "Natural Language Processing",
                             Courses == "Lab in data science" ~ "Data Science",
                             Courses == "Machine learning" ~ "Machine Learning",
                             Courses == "Mathematical foundations of signal processing" ~ "Signal Processing",
                             Courses == "Model Driven Engineering" ~ "Machine Learning",
                             Courses == "Natural Language" ~ "Natural Language Processing",
                             Courses == "Network Science" ~ "Artificial Neural Networks",
                             Courses == "Principles of Distributed Computing" ~ "Big Data",
                             Courses == "Program Analysis for System Security and Reliability" ~ "Computer and network security",
                             Courses == "System Security" ~ "Computer and network security",
                             TRUE ~as.character(Courses))) %>% 
  group_by(Sector,Courses) %>% 
  summarize(Matches = n()) %>%
  ungroup()

edgeList <- data %>% 
  rename(from = Sector,
         to = Courses) %>% 
  ungroup()

# Contructing the node dataframe containing all the NACE sectors and AI topics.

# Defining all unique sector nodes

sectors <- data.frame(name = unique(edgeList$from), group =  as.character(unique(edgeList$from)))

# Defining all unique course nodes

courses <- data.frame(name = unique(edgeList$to), group = "Course")

# Putting all unique nodes in one dataframe

nodeList <- sectors %>%
  rbind(courses)

# Defining all the NACE Sectors we've chosen
sectors <- nodeList %>% 
  filter(group != "Course")

sector_titles <- sectors$name

# Defining all the AI courses in our data
courses <- nodeList %>% 
  filter(group == "Course")

course_names <- courses$name

# Defining a click script for the network visualization
MyClickScript <- 'd3.select(this).select("circle").transition().duration(750).attr("r", 40)'
#MyClickScript <- 'alert("You clicked " + d.name + " which is in row " + (d.index + 1) +  " of your original R data frame");'

ui <- fluidPage(theme = shinytheme('sandstone'),
		title = "Impact of AI on sectors",
                
                ################################################## GRAPH VIEW ################################################### 
                navbarPage(tags$strong("Impact of AI on sectors",
				       style = "font-size: 28px"),

			   tabPanel("Welcome",
				    style = "font-size: 20px",
                                    mainPanel(htmlOutput(outputId = "welc"),
					img(src = "space.png",height = 130, width = 450),
                                        img(src = "ec.png", height = 130, width = 200))),
			
                           tabPanel('GRAPH VIEW',
                                    sidebarPanel(
						 style = "font-size: 16px",
                                      selectInput('sect',
                                                  "Choose the sector(s) you're interested in!",
                                                  choices = sector_titles,
                                                  multiple = TRUE,
                                                  selected = sector_titles),
                                      sliderInput('nb',
                                                  'Minimum number of matches', 
                                                  min = 0,
                                                  max = 20,
                                                  value = 7),
                                      sliderInput('top',
                                                  'Amount of output displayed in graph', 
                                                  min = 0,
                                                  max = 25,
                                                  value = 15)),
                                    mainPanel(h3('The graph below shows the most relevant AI topics to each NACE sector you selected on the left'),
					      forceNetworkOutput(outputId = "graphai", height = "600px",width = "100%"),
					      tags$strong("If you get the error 'An error has occurred. Check your logs or contact the app author for clarification.' no results correspond to your selection (relevance is too high).
						    Please try other sectors with the same relevance or change the relevance level.", style = "font-size: 14px"),
                                              DT::dataTableOutput(outputId = "table"))),
                           
                           ################################################## HISTOGRAM VIEW ################################################### 
                           
                           tabPanel('HISTOGRAM VIEW',
                                    sidebarPanel(
						 style = "font-size: 16px",
                                      selectInput('secth',
                                                  "Choose the sector(s) you're interested in!",
                                                  choices = sector_titles,
                                                  multiple = TRUE,
                                                  selected = sector_titles),
                                      sliderInput('match',
                                                  'Minimum number of matches', 
                                                  min = 0,
                                                  max = max(data$Matches),
                                                  value = 11),
                                      selectInput('xcol',
                                                  "Choose your X axis variable",
                                                  choices = c("Sector", "Courses"),
                                                  selected = 'Courses'),
                                      selectInput('col',
                                                  "Choose how you color the graphs",
                                                  choices = c("Sector", "Courses"),
                                                  selected = 'Sector'),
                                      selectInput('facet_row',
                                                  'Create a graph for each course or sector inline',
                                                  c(None = '.', c("Courses","Sector")),
                                                  selected = "clarity"),
                                      selectInput('facet_col',
                                                  'Create a graph for each course or sector incolumn',
                                                  c(None = '.', c("Courses","Sector")),
                                                  selected = "clarity"),
                                      sliderInput('plotHeight',
                                                  'Adjust the height of the plot', 
                                                  min = 100,
                                                  max = 5000,
                                                  value = 830),
                                      sliderInput('plotWidth',
                                                  'Adjust the width of the plot',
                                                  min = 100,
                                                  max = 5000,
                                                  value = 1100)),
                                    mainPanel(plotlyOutput("Histogram", height = "250%"),
					      tags$strong("If you get the error 'An error has occurred. Check your logs or contact the app author for clarification.' no results correspond to your selection (relevance is too high). Please try other sectors with the same relevance or change the relevance level.", style = "font-size: 14px"))),
			
                           tabPanel(tags$strong("Assistance", style = "font-size: 11px"),
                                    style = "font-size: 20px",
                                    mainPanel(htmlOutput(outputId = "instr")))
                )

)





# Define server logic
server <- function(input, output) {
  
  output$instr <- renderUI({str0 = "If you need any assistance, please contact by mail the following adress : joelle.liegeois@ext.ec.europa.eu"
			   str1 = "This shinyapp is a platform to expose the results obtained with NLP(Natural Language Processing) 
				   to find the most relevant AI topics in pre-selected professional sectors."
                           space = " "
                           str2 = "For each of the sectors, sector specific research journals with high impact value were selected.
				   Within those journals, the amount of articles that treated AI topics were counted for each of the different AI topics."
                           str3 = "The assumption is that when a certain AI topic appears more often, it is affecting that sector more."
				str03 = "Each of the sectors shows a different AI topic profile."
				str0003 = "Knowing which types of AI are affecting a certain sector allows people to be informed of these topics with more care. It shows which  
					   AI topics are trending in research for their sector."
				str00003 = "A take-away for education may thus be to incorporate some general information on those AI topics in their curriculum."
                           str4 = "The Graph View tab"
			   str12 = "In the first tab is a network visualization of all the links that we have established."
				str012 = "On the left side you may choose one or several sectors among those we’ve pre-selected to visualize which
					  topics are the most relevant to them."
				str0012 = "The width of the link between the nodes represent how many times the topic was matched to a specific sector 
				    through research articles. You may add an additional threshold to the matches, showing only topics who’ve been matched x amount of times at minimum to your selected topic(s)."
				str00012 = "You will find these results formatted in a graph fashion and rendered with a network visualization."
				str000012 = "You can click and drag nodes to arrange the visualization to your needs, as well as choose among
					     a few options to better suit your investigation."
			   str13 = "The Histogram view tab"
			   str14 = "In the second tab is a histogram representation of these same results."
				str014 = "You can choose which sector(s) you wish to investigate."
				str0014 = "Several possibilities to arrange the visualization are available:"
                           str5 = "- Choose the X axis, either representing sectors or topics"
                           str6 = "- Choose the colors to distinguish between either sectors or topics"
                           str7 = "- Modify the minimum threshold for cosine similarity, limiting the results of the plot"
                           str8 = "- Modify the minimum amount of matches between a sector and a topic for the result to be visualized"
                           str9 = "- Replace stacked bars with a plot per sector or topic, aligning them line by line"
                           str10 = "- Replace stacked bars with a plot per sector or topic, aligning them column by column"
                           str11 = "- Adjust the height and width of the plot"
                           HTML(paste(space,
                                      str1,
                                      space,
                                      str2,
                                      space,
                                      str3,
					str03,
					str0003,
					str00003,
				      space,
					str4,
					space,
					str12,
					str012,
					str0012,
					str00012,
					str000012,
					space,
					str13,
					space,
					str14,
					str014,
					str0014,
                                      space,
                                      str5,
                                      str6,
                                      str8,
                                      str9,
                                      str10,
                                      str11,
				      space,
				      str0,
space,
space,
space,
space,
space,
                                      sep = '<br/>'))})
  output$welc <- renderUI({
			str0 = 'Welcome to this Shiny App!'
			str01 = 'This app is a proof of concept developed for the European Commission.'
			str001 = 'By the use of AI (NLP and graphs), it visualizes which AI concepts are trending in the research community for ten different professional sectors.'
			str1 = 'The different tabs above offer different views. You are now in the "welcome" tab.'
			str2 = 'The Graph View tab shows the different sectors and the AI concepts related to them in a network visualization.'
			str02 = "You can choose to limit which sectors are shown from the selection box."
			str002 = "The two sliders allow you to increase or decrease the relevance and amount of output shown. The first one
				 allows to adjust the minimum amount of matches between a professional sector and an AI concept. The second one 
				 simply limits the amount of output shown in total."
			str00002 = "You will find in this web-page the most recurrent AI topics in current research for 10 different NACE sectors."
			str3 = "The histogram tab shows the same results but in histograms,"
			str03 = "You can re-arrange the plots (axis, sectors, concepts), in order to visualise the data the way you want."
			str4 = "For more details, you can go to the final tab: 'assistance'."
			space = ''
			HTML(paste(space,
                                      str0,
				      space,
				      str01,
				      str001,
                                      space,
				      str1,
				      space,
                                      str2,
					str02,
					str002,
					str00002,
                                      space,
                                      str3,
					str03,
                                      space,
                                      str4,
space,
space,
space,
					sep = '<br/>'))			
			})
  
  
  ################################################## GRAPH VIEW ###################################################
  
  filtered_edgeList1 <- reactive({
    edgeList %>% 
    filter(from %in% input$sect) %>% 
    filter(Matches >= input$nb) %>% 
    group_by(from) %>%
    top_n(input$top,Matches) %>%
    ungroup()
  })
  
  filtered_nodeList1 <- reactive({
    nodeList %>% 
      filter(name %in% filtered_edgeList1()$from | name %in% filtered_edgeList1()$to) %>% 
      mutate(nodesize = case_when(group == "Course" ~ '10',
                                  group != "Course" ~ '90',
                                  TRUE ~ as.character(0))
             ) %>%
      mutate_all(as.character)
  })
  
  filtered_edgeList2 <- reactive({
    filtered_edgeList1() %>% 
      mutate(from.index = match(filtered_edgeList1()$from,
                                filtered_nodeList1()$name)-1) %>% 
      mutate(to.index = match(filtered_edgeList1()$to,
                              filtered_nodeList1()$name)-1) %>%
      as.data.frame()
  })
  
  
  output$graphai <- renderForceNetwork(forceNetwork(Links = filtered_edgeList2(),
                                                    Nodes = filtered_nodeList1(),
                                                    Source = "from.index",
                                                    Target = "to.index",
                                                    Value = "Matches",
                                                    NodeID = "name",
                                                    Nodesize = "nodesize",
						    #colourScale = 'd3.scaleOrdinal().range(["#dc6900","#eb8c00","#e0301e","#a32020","#602320","#ff4e50","#fc913a","#740001","#ae0001","#eeba30","#d3a625","#000000"])' ,
                                                    colourScale = 'd3.scaleOrdinal().range(["#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#E69F00", "#56B4E9","#000000","#FF6F61","#6B5B95","#264E36","#D69C2F"])',
						    Group = "group",
                                                    linkDistance = 200,
                                                    height = 1200,
                                                    width = 800,
						    charge = -30,
                                                    fontSize = 30,
                                                    opacity = 0.8,
                                                    zoom = F,
                                                    bounded = T,
						    linkColour = "#afafaf",
                                                    linkWidth = networkD3::JS("function(d) { return d.value/7; }"),
                                                    legend = T,
                                                    clickAction = MyClickScript))


  ################################################## TABLE ########################################################

  output$table <- DT::renderDataTable({
    data %>%
	filter(Sector %in% input$sect) %>%
	filter(Matches > input$nb) %>%
        arrange(desc(Sector,Matches))
  })

  ############################################# HISTOGRAM VIEW ###################################################
  
  data_hist <- reactive({
    data %>%
      filter(Matches > input$match) %>% 
      filter(Sector %in% input$secth) %>% 
      mutate(Matches = as.numeric(Matches)) 
    
  })
  
  output$Histogram <- renderPlotly({
    p <- ggplot(data_hist(),
                aes_string(x = input$xcol,
                           y = data_hist()$Matches, 
                           fill = input$col)) +
      geom_col() +
#      scale_fill_manual(values = c("#dc6900","#eb8c00","#e0301e","#a32020","#602320","#ff4e50","#fc913a","#740001","#ae0001","#eeba30","#d3a625","#000000")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Histogram of number of matches (Y axis) between AI courses (X axis) and Professional sectors (Colors)") 
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p,
             height = input$plotHeight,
             width = input$plotWidth,
             autosize = TRUE)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

