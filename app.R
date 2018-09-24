#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rsconnect::setAccountInfo(name='kurtxz', token='13A8B8F4A668444A4E2FBE58F2CDD85E', secret='20/8Js1Qq6J4g/YqV0O0fwpVjrBUe6g5vecd284H')

library(shiny)
library(DT)
library(dplyr)
library(wordcloud)
library(tm)
library(ggplot2)
library(plotly)
library(stringr)
library(maps)
library(maptools)

#Read data
cybersecurity.df <- readRDS("cybersecurity.df.rds")
cyber_geo <- readRDS("cybersecurity.geo.rds")
cyber_geo$longitude <- as.numeric(cyber_geo$longitude)
cyber_geo$latitude <- as.numeric(cyber_geo$latitude)
cybersecurity.df$favoriteCount <- as.numeric(cybersecurity.df$favoriteCount)
cyberattack.df <- readRDS("cyberattack.df.rds")
cybercrime.df <- readRDS("cybercrime.df.rds")
bitdefender.df <- readRDS("bitdefender.df.rds")
kaspersky.df <- readRDS("kaspersky.df.rds")
#Word Cloud Preparation
cybersecurity.df$term <- "cybersecurity"
cyberattack.df$term <- "cyberattack"
cybercrime.df$term <- "cybercrime"
bitdefender.df$term <- "bitdefender"
kaspersky.df$term <- "kaspersky"
cyberall <- rbind(cybersecurity.df,cyberattack.df,cybercrime.df)
softwareall <- rbind(bitdefender.df,kaspersky.df)

cybers <- c("Cybersecurity","Cyberattack","Cybercrime")
software <- c("Bitdefender","Kaspersky")
#Sentiment Analysis Preparation
bit_result <- read.csv("bit_result.csv")
kp_result <- readRDS("kp_result.rds")
bit_result <- readRDS("bit_result.rds")
# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel(fluidRow(
  column(8, 
         h3("MA615 Twitter Text Mining Project - Cybersecurity")
        
  ))),
  tabsetPanel(
    tabPanel("Inrtodction",
             titlePanel("Cybersecurity"),
             shiny::hr("2017 Cyberthreats Defense Report have done a survey and data analysis on successful cyberattacks a company or a organization experienced in 15 countries. 
                       It shows that 80.6% respondents in America have compromised by at least one successful cyberattack in the past 12 months. The beast situation happened in Brazil and the worst situation happpened in China, 64.7% and 95.9%."),
             shiny::h6(""),
             shiny::br("It's not hard to see the severity of cyberattack and importance of cybersecurity. Thus, I choose cybersecurity and cyberattack(cybercrime) as my topics to see the reactions and comments people have on social media Twitter.
                       I will use text analysis and mapping to see the where and what kind of reactions and comments peopel have gloabally. And also I select first three best protection softwares against cyberattacks, 
                       using sentiment analysis to see people's attitude towards these three softwares."),
             shiny::h6(""),
             shiny::a(href="http://www.crn.com/sites/default/files/ckfinderimages/userfiles/images/crn/custom/Webroot_Q3_2017_CyberEdge_Cyberthreat_Defense_Report.pdf", "Click here to read the report"),
             br(),
             img(src="cybersecurity.jpg", height=300, width=600)),
    
    tabPanel("Map",
   # Application title
     titlePanel("Cybersecurity"),
   
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("map")
        )),
   tabPanel("Timeline",
            # Application title
            titlePanel("Cybersecurity"),
            
            # Show a plot of the generated distribution
            mainPanel(
              plotlyOutput("timeline")
            )),
   tabPanel("Word Cloud",
            sidebarPanel(
              selectInput("selection1", "Choose a term:",
                          choices = cybers, selected = 1),
              hr(),
              sliderInput("freq1",
                          "Minimum Frequency:",
                          min = 1,  max = 100, value = 30),
              sliderInput("max1",
                          "Maximum Number of Words:",
                          min = 1,  max = 300,  value = 100)
            ),
            mainPanel(
              plotOutput(
                outputId = "wordcloud1"
              )
            ),
            sidebarPanel(
              selectInput("selection2", "Choose a software:",
                          choices = software, selected = 1),
              hr(),
              sliderInput("freq2",
                          "Minimum Frequency:",
                          min = 1,  max = 100, value = 30),
              sliderInput("max2",
                          "Maximum Number of Words:",
                          min = 1,  max = 300,  value = 100)
            ),
            mainPanel(
              plotOutput(
                outputId = "wordcloud2"
              )
            )),
   tabPanel("Images",
            splitLayout(
              h2("Bitdefender"),
              h2("Kaspersky")
            ),
            hr(),
            splitLayout(mainPanel(
              img(src="bitdefender.PNG", height=250, width=200)),
              mainPanel(img(src="kaspersky.jpg", height=250, width=200))),
            hr(),
            br(),
            splitLayout(mainPanel(
              h5("Bitdefender is a Romanian cybersecurity and anti-virus software company. 
                 It was founded in 2001 by Florin Talpe?? who is currently the CEO. 
                 Bitdefender develops and sells anti-virus software, internet security software, 
                 endpoint security software, and other cybersecurity products and services.")),
              mainPanel(
              h5("Kaspersky Lab is a multinational cybersecurity and anti-virus provider headquartered in Moscow, 
                 Russia and operated by a holding company in the United Kingdom. It was founded in 1997 by Eugene Kaspersky, 
                 who is currently the CEO. Kaspersky Lab develops and sells antivirus, internet security, password management, 
                 endpoint security, and other cybersecurity products and services."))
            
            ),
            br(),
            splitLayout(
              a(href="https://store.google.com/product/google_home", "Click to visit Bitdenfender website"),
              a(href="https://store.google.com/product/google_home", "Click to visit Kaspersky website")
            ),
            br(),
            br(),
            br(),
            p("Caption: Images and informations are from Wikipedia.")
           ),
   tabPanel("Sentiment Analysis",
            sidebarLayout(
              sidebarPanel(
                radioButtons("s", "Select a software:",
                             list("Bitdefender"='a', "Kaspersky"='b'))
              ),   
              
              mainPanel(
                plotlyOutput("histogram")
              )
            )
   )

    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  #map
  output$map <- renderPlotly({
    
    map <- NULL
    mapWorld <- borders("world", colour="gray45", fill="light green") # create a layer of borders
    map <- ggplot() +  mapWorld
    map <- map+ geom_point(aes(cyber_geo$longitude,cyber_geo$latitude), data = cyber_geo, color="dark red", size=2.5) 
    map <- map + theme_bw()
    ggplotly(map)
    
  })
  
  #timeline
  output$timeline <- renderPlotly({
    
    t1 <- ggplot(cybersecurity.df, aes(x = created))
    t1 <- t1 + geom_line(aes(y = favoriteCount),color = "navy")
    t1 <- t1 + labs(x="Time", y="Number of Favorites")
    ggplotly(t1)
    
  })
  
  #term selection
  
  termselection <- reactive({
    ifelse(input$selection1 == "Cybersecurity","cybersecurity",
           ifelse(input$selection1 == "Cyberattack","cyberattack","cybercrime"))
  })
  
  brandselection <- reactive({
    
    ifelse(input$selection2 == "Bitdefender","bitdefender","kaspersky")

  })
  
  #word selection
  
  cloudata1 <- reactive({
    cyberall %>%
      filter(term == termselection())
  })
  
  cloudata2 <- reactive({
    softwareall %>%
      filter(term == brandselection())
  })
  
  output$wordcloud1 <- renderPlot({
    words1 <- cloudata1()
    cyberword <- Corpus(VectorSource(str_replace_all(words1$text, "@", "")))
    cyberword <- tm_map(cyberword, removePunctuation)
    cyberword <- tm_map(cyberword, content_transformer(tolower))
    cyberword <- tm_map(cyberword, removeWords, stopwords("english"))
    cyberword <- tm_map(cyberword, removeWords, c("cybersecurity","cyber","security","levelnet",
                                                  "amp","visit","part","cyberattack","attack","cybercrime",
                                                  "crime"))
    cyberword <- tm_map(cyberword, stripWhitespace)
  
    pal <- brewer.pal(8,"Dark2")
    pal <- pal[-(1:4)]
    set.seed(168)
    wordcloud(words = cyberword, scale=c(3,0.2), min.freq = input$freq1, max.words=input$max1, random.order=FALSE, 
            rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2")[5:8])
  })
  
  output$wordcloud2 <- renderPlot({
    words2 <- cloudata2()
    softwareword <- Corpus(VectorSource(str_replace_all(words2$text, "@", "")))
    softwareword <- tm_map(softwareword, removePunctuation)
    softwareword <- tm_map(softwareword, content_transformer(tolower))
    softwareword <- tm_map(softwareword, removeWords, stopwords("english"))
    softwareword <- tm_map(softwareword, removeWords, c("cybersecurity","cyber","security","levelnet",
                                                  "amp","visit","part","cyberattack","attack","cybercrime",
                                                  "crime","software","securitysoftware","bitdefender","kaspersky"))
    cyberword <- tm_map(softwareword, stripWhitespace)
    
    pal <- brewer.pal(8,"Dark2")
    pal <- pal[-(1:4)]
    set.seed(168)
    wordcloud(words = softwareword, scale=c(3,0.2), min.freq = input$freq2, max.words=input$max2, random.order=FALSE, 
              rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2")[5:8])
  })
  #Sentiment Analysis
  output$histogram <- 
    renderPlotly({
      
      if(input$s == "a"){
        hist1 <- ggplot(data=bit_result, aes(bit_result$score)) + geom_histogram(fill = "yellow", binwidth = 0.5) + theme_bw() + 
          scale_x_continuous(breaks=seq(-4, 4, 1)) + geom_vline(xintercept = mean(bit_result$score),color = "red",size = 1.3) + 
          labs(title = "Histogram of sentiment score of Bitdefender", y = "Frequency", x= "Score")
        ggplotly(hist1)
        
      }
       else{
           hist2 <- ggplot(data=kp_result, aes(kp_result$score)) + geom_histogram(fill = "yellow", binwidth = 0.5) + theme_bw() + 
           scale_x_continuous(breaks=seq(-4, 5, 1)) + geom_vline(xintercept = mean(kp_result$score),color = "red",size = 1.3) + 
           labs(title = "Histogram of sentiment score of Kaspersky", y = "Frequency", x= "Score")
           ggplotly(hist2)
         
       }
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

