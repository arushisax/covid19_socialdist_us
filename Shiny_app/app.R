# Arushi is analyzing Twitter sentiment about social distancing and looking into social distancing data as well
# We load the necessary packages that will be used in the application.

library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggthemes)
library(shinythemes)
library(leaflet.extras)
library(tm)
library(wordcloud)
require(devtools)
library(wordcloud2)
library(ggpubr)
library(geojsonio)
library(rgdal)

# Import sentiment data for US map: https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da

us_senti <- read_rds("us_senti_locations.rds")

# Categorize sentiment values
us_senti$sent_type <- ifelse(us_senti$sent.value < 0, "Negative", 
                                    ifelse(us_senti$sent.value == 0, "Neutral", 
                                           ifelse(us_senti$sent.value > 0, "Positive", "other")))
# Set Colors for each sentiment value
pal <- colorFactor(
    palette = c('red', 'blue', 'green'),
    domain = us_senti$sent_type
)

# Load data for word cloud
data_cleaned <- read_rds("wordcloud_data2.rds")

####### For Google Search analysis tab
# Load google search trends vs. social distancing table

scatterdata <- read_rds("joined_dataF.rds")


# Define UI for application that draws a histogram
ui <-   shinyUI(
    navbarPage("COVID-19: Early Public Sentiment about #SocialDistancing",
        theme = shinytheme("united"),
        
        ##########
        ##ABOUT##
        #########
        tabPanel("At a Glance",
                 br(),
                 ## Load image
                 imageOutput("flattencurve", width = "100%", height = "100%"),
                 br(),
                 
                 h2("How do US citizens feel about COVID-19 social distancing and how well are they adhering to the measure? ", align = "center"),
                 h4(em("An analysis of Twitter activity to understand public reaction in March 2020"), align = "center"),
                 
                  h3("Methodology"),
                 
                 br(),
                 
                 p("In this project I explore the following questions by section:"),
                 br(),
                 p(strong("Tweet Analysis - Word Cloud:")),
                 p(em("What are the most common themes and concepts when people discuss social distancing on Twitter?")),
                 p("+ A word cloud of common words in my aggregated Social Distancing Tweets collection."),
                 
                 br(),
                 
                 p(strong("Tweet Analysis - Sentiments:")),
                 p(em("How positive or negative are people’s opinions about social distancing?")),
                 p("+ Most common negative words and positive words. The takeaway from this analysis is that there are more positive themes as opposed to negative themes in this random sampling."),
                 
                 br(),
                 
                 p(strong("Geographic Analysis:")),
                 p(em("Does positive or negative sentiment vary by US state?")),
                 p("+ There is a mix of sentiment throughout the United States, with positive and neutral statements having greater representation than expected. These Tweets are a random sample and do not accurately capture public sentiment because many citizens do not post thoughts on Twitter. Furthermore, sentiment analysis libraries are not sophisticated enough to pick up tones such as sarcasm, which are quite common on Twitter."),
                 
                 br(),
                 
                 p(strong("Google Search Trends:")),
                 p(em("If people are social distancing more, are they also more likely to read about social distancing?")),
                 p("+ Found reasonably strong correlation between social distancing and search activity related to those concepts. Therefore, as individuals are forced or inclined to social distance, they want to increase their awareness of the topic. One can also make the reverse claim but I chose social distancing as my independent variable, as opposed to the outcome variable, because social distancing is more likely to be an exogenous directive mandated by local and /or state governments. Given some of correlation coefficients and p-values demonstrated by these charts, it is reasonable to state that as social distancing increases within a US state, the appetite to learn about it may also increase.")
                 
                 
                 
        ),
        
        tabPanel("Tweet Analysis",
                 tabsetPanel(
                     
                     # this page includes the world cloud and sentiment analysis of specific words found throughout the tweets 
                     
                     tabPanel("Word Cloud",
                              
                              h3("Selected English-language Tweets about Social Distancing (March 2020)"),
                              
                              br(),
                              
                              h4("Prominent Expressions about Social Distancing"),
                              
                              sidebarLayout(
                                  sidebarPanel(sliderInput("freq",
                                                         "Minimum Frequency:",
                                                         min = 1,  max = 50, value = 15),
                                                        sliderInput("max",
                                                         "Maximum Number of Words:",
                                                         min = 1,  max = 200,  value = 100)),
                              # Show Word Cloud
                              mainPanel(
                                  plotOutput("plot")))),
                              
                            
                     
                     tabPanel("Sentiment Analysis",
                              
                              h3("Positive vs. Negative Sentiments for Selected Themes"),
                              
                              br(),
                              
                              sidebarPanel(
                                    helpText("The first chart demonstrates the sentiment stregnth (polarity) of common words that were found across Tweets related to social distancing. Words with strong positive polarity reflect higher positive number
                                             and words with strong negative polarity are reflected by high negative numbers.'Virus', 'hard', and 'death' had the strongest negative polarity meanwhile 
                                             'happy', 'safe', and 'Trump' have the strongest positive connotations in this Twitter data set."
                                      
                                  )),
                              mainPanel(tabsetPanel(
                                  tabPanel(
                                        imageOutput("polarity_chart2"),
                                        )
                            )
                            ),
                                br(),
                                br(),
                                br(),
                               
                             sidebarPanel(
                                helpText("This chart shows the frequency of most common positive and negative words in the Social Distancing tweets. 'Virus', 'Hard', 'Death' and 'Risk' were most common negative themes whereas as 'happy', 'safe', 'trump', and 'love' where most commonly used in a positive context. These findings are consistent with the Word Cloud in earlier tab.")),
                            
                            mainPanel(tabsetPanel(
                                    tabPanel(
                                        imageOutput("freq_chart"),
                                    ))
                                    )
                            
                     
                     
                     ))),
        
        #this tab shows the interactive maps 
        
        tabPanel("Geographic Analysis",
                 
                 h3("How positive, negative, or neutral is sentiment across the United States?"),
                 
                 br(),
                 
                 h4("There is a relatively equal distribution of positive, negative, and neutral sentiment with positive sentiment being greater than expected"),
        
             mainPanel( 
                #this will create a space for us to display our map
                leafletOutput(outputId = "mymap"))),
        
        tabPanel("Google Search Trends",
                 
                 h3("Does adherence to social distancing measures correlate with Google search activity across the United States?"),
                 
                 br(),
                 
                 h4("Google searches for 'Social Distancing' has reasonably strong positive correlation with social distancing adherence. High p-values for 'COVID-19' and 'Social Distancing' searches indicate that the relationship between social distancing and Google searches is significant."),
                 
                 sidebarPanel(
                     helpText("Choose a Google search term to understand its search popularity vs. actual social distancing within each US state"),
                     tags$i("Note: The more negative the Social Distancing score, the greater a state's adherence to social distancing (ex: -10 means more distancing than -5)."),
                     br(),
                     
                     selectInput("term", "Google Search Term:",
                                 choices = list("COVID-19" = "COVID-19",
                                                "coronavirus" = "coronavirus",
                                                "social distancing" = "social distancing",
                                                "All of the above" = "all_terms"
                                               ),
                                 selected = "COVID-19")),
                 
                 mainPanel(plotOutput("scatterplot"))
                 
              
                 ),
        tabPanel("About",
                 
                 
                 # Title
                 
                 h2("Understanding how US citizens feel about and conduct social distancing"),
                 br(),
                 div(),
                 
                 
                 
                 br(),
                 
                 fluidRow(column(2), column(8,
                                            
                                            h4(strong("About this Project")),          
                                            
                                            #text to introduce project
                                            
                                            p("The purpose of the project is to understand public sentiment about social distancing, across the United States, via Twitter Data and correlate it to the extent to which citizens are effectively distancing."),
                                            
                                            br(),
                                            
                                            p("On March 11, 2020 the World Health Organization officially declared COVID-19 (a coronavirus) to be a global pandemic. Within that week, states across the United States of American, as well as countries around the world, quickly began instituting ‘social distancing’ public health measures to slow the transmission of infection and prevent overwhelming health care systems. Social Distancing varied from state-to-state and ranged from large-scale measures such as canceling group events and closing bars and restaurants to individual-level measures such as staying 6 feet away from all persons outside of your immediate household. Because these measures curtail normal life and daily interactions, they undoubtedly took a toll on the public."),
                                            
                                            br(),
                                            
                                            h4(strong("The Data")),
                                            
                                            
                                            
                                            p("My analysis utilizes three data sets:"),
                                            
                                            span(),
                                            br(),
                                            
                                            p("(1) Twitter Data: A random sample of 15,000 English language, US-based tweets with the words ‘social distancing’ posted after March 11, 2020. The Twitter API does not allow users to specific day / month timeframes prior to last 12 days."),
                                            p("(2) Google Search Trends: Explored popularity of three individual search terms “COVID-19”, “coronavirus”, and “social distancing” between March 11, 2020 and March 29, 2020 and took an average across all three terms as well. Search popularity score is calculated on a scale from 0 to 100, where 100 is the location with most popularity as a fraction of total searches and a value of 50 indicates a location in which the search term is half as popular."),
                                            p("(3) -	Google COVID-19 Mobility Reports: My data source was a library called Amarang (GitHub) which scraped and disaggregated Google’s published PDF charts demonstrating COVID-19 mobility in the US. I took an average of the values across counties and categories (Parks, Shopping Malls, Restaurants, etc.) from March 11, 2020 to March 29, 2020 to get an aggregate state level view. The data shows an increase or decrease in movement vs. baseline activity. The more negative the number, the greater the social distancing adherence in that US state. Note that some US states had stricter lockdowns compared to other states."),
                                            
                                            br(),
                                            
                                            h4(strong("About Me")),
                                            
                                            p("My name is Arushi Saxena and I am first year Masters in Design Engineering (MDE) candidate at Harvard’s Graduate School of Design and the School of Engineering and Applied Sciences. My research focus is on the ethical design, development, and use of technology."),
                                            p("You can reach me at ",
                                              a("arushisaxena@mde.harvard.edu",
                                                href = "mailto: arushisaxena@mde.harvard.edu",),
                                              "or ",
                                              a("LinkedIn.",
                                                href = "https://www.linkedin.com/in/arushisaxena/"))
                                            
                                            
                 )))
        
        
        )
    )
        
        
                             
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Load image in About page
    output$flattencurve <- renderImage({
        
        list(src = 'flatten_curve.jpeg',
             height = 300,
             width = 500,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
    
    # Make the Word Cloud
    output$plot <- renderPlot({
        wordcloud(names(data_cleaned), data_cleaned,scale=c(8,0.25),
                     min.freq = input$freq, max.words=input$max,
                     colors=brewer.pal(8, "Dark2"))
    })
    
    # First Sentiment Analysis Chart (Polarity) 
    output$polarity_chart2 <- renderImage({
        list(
            src = "polarity_chart2.png",
            contentType = 'image/png',
            width = 600,
            height = 400
        )
    }, deleteFile = FALSE)
    
    # Second Sentiment Analysis Chart (Frequency) 
    output$freq_chart <- renderImage({
        list(
            src = "freq_chart.png",
            contentType = 'image/png',
            width = 600,
            height = 400
        )
    }, deleteFile = FALSE)
    
    
    # create the map
    output$mymap <- renderLeaflet({
        leaflet(us_senti) %>% 
            setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = us_senti, lat = ~ lat, lng = ~ lng, weight = 5, 
                       popup = paste("Sentiment:",sep = " ",us_senti$sent_type, "<br>",
                                     "Sentiment Score:", us_senti$sent.value, "<br>",
                                     "Text:", us_senti$text, "<br>"),
                       label = ~as.character(paste0(sent_type)), 
                       fillOpacity = 0.5, color = ~pal(sent_type), radius = 5)
    })
    
    #Scatterplot with reactive data

    output$scatterplot <- renderPlot({
        
        if(input$term == "coronavirus"){            
            scatterdata %>% 
                ggplot(aes(x=average, y=`coronavirus`, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Search Term Popularity: 'Coronavirus'") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 1, size = 3)
        }
        else if(input$term == "COVID-19"){
            scatterdata %>% 
                ggplot(aes(x=average, y=`COVID-19`, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Search Term Popularity: 'COVID-19'") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 2, size = 3)
        }
        else if(input$term == "social distancing"){
            scatterdata %>% 
            ggplot(aes(x=average, y=`social distancing`, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Search Term Popularity: 'Social Distancing'") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 2, size = 3)
         }
        else if(input$term == "all_terms"){
            scatterdata %>% 
                ggplot(aes(x=average, y=all_terms, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Avg. Search Popularity of COVID-19-related terms") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 2, size = 3)
        }
    
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
