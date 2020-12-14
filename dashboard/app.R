library(shiny)
library(tm)
library(magrittr)
library(tidytext)
library(wordcloud)
library(shinydashboard)
library(ggplot2)
library(stringr)
library(tidyverse)

list_org <- read.csv("listings2.csv")
listings <-list_org %>% 
    separate(location, into=c("area","state"),sep=",")# %>% filter(state == "MA")
data(stop_words)
tidy_list<- list_org%>% unnest_tokens(word,description) %>%
  anti_join(stop_words) #remove stop words

# Define UI for application that draws a histogram
ui <-dashboardPage(
    skin="green",
    dashboardHeader(title = "Job of Data Scientists"),
    dashboardSidebar(
        width = 150,
        sidebarMenu(
            menuItem("Dashboard", tabName = "tab1", icon = icon("dashboard")),
            menuItem("Plot", tabName = "tab3", icon = icon("th")),
            menuItem("Word Frequency", tabName = "tab2", icon = icon("th")),
            menuItem("Biogram", tabName = "tab4", icon = icon("th"))
        )
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "tab1",
                    fluidRow(
                        column(4,
                               selectInput("company1",
                                           "Company:",
                                           c("All",
                                             unique(as.character(listings$company))))),
                        
                        column(4,
                               selectInput("state1",
                                           "State:",
                                           c("All",
                                             unique(as.character(listings$state)))))
                    ),
                    column(4,
                           selectInput("area1",
                                       "Area:",
                                       c("All",
                                         unique(as.character(listings$area))))),
                    column(4,
                           selectInput("title1",
                                       "Title:",
                                       c("All",
                                         unique(as.character(listings$title))))),
                    DT::dataTableOutput("table")
                    
            ),
            
            tabItem(tabName = "tab2",
                    # Boxes need to be put in a row (or column)
                    fluidRow(
                        
                        box( collapsible = TRUE,solidHeader = TRUE,#background = "lime", 
                             title = "Control of word cloud",
                             sliderInput("freq",
                                         "Minimum Frequency:",
                                         min = 1,  max = 50, value = 15),
                             sliderInput("max",
                                         "Maximum Number of Words:",
                                         min = 1,  max = 300,  value = 100)),
                        # Show Word Cloud
                        box(plotOutput("plot1"), title = "Word Cloud", status = "primary")
                        
                        
                    ),
                    fluidRow(  
                        box( collapsible = TRUE,solidHeader = TRUE,#background = "lime", 
                             title = "Show the top n words",
                             sliderInput("max2",
                                         "Maximum Number of Words:",
                                         min = 4,  max = 30,  value = 5)),
                        # top n words
                        box(plotOutput("plot5"), title = "The words with most frequency in descriptions", status = "primary")
                    )
            ),
            
            tabItem(tabName = "tab3",
                    titlePanel(title = "Areas and companies with most Data Scientist job positions"),
                    sliderInput("num_area",
                                "States:",
                                min = 3,  max = 16, value = 1),
                    plotOutput("plot2"),
                    # sliderInput("num_comany",
                    #             "Maximum Number of cpmpanies:",
                    #             min = 3,  max = 10,  value = 1),
                    plotOutput("plot3")
                    
            ),
            
            tabItem(tabName = "tab4",
                    h2("Common bigram plot"),
                    h4("The words appear together has common bigram. The Plot shows the common bigrams in job descriptions, showing those that occurred more than 3 times and where neither word was a stop word"),
                    plotOutput("plot4")
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table <- DT::renderDataTable(DT::datatable({
        data <- listings
        if (input$title1 != "All") {
            data <- data[data$title == input$title1,]
        }
        if (input$company1 != "All") {
            data <- data[data$company == input$company1,]
        }
        if (input$state1 != "All") {
            data <- data[data$state == input$state1,]
        }
        if (input$area1 != "All") {
            data <- data[data$area == input$area1,]
        }
        data<- select(data, -c("link", "X", "description"))
        
        data
    }))
    
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    output$plot1 <- renderPlot({
        
        ## tidy text with unnest_tokens(), take each word into a token
        data(stop_words)
        frequency<- list_org %>%
            unnest_tokens(word,description) %>% 
            anti_join(stop_words) %>%
            mutate(word = str_extract(word, "[a-z']+"))   %>% count(word)
        #wordcloud
        wordcloud_rep(frequency$word, frequency$n, scale=c(4,1.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))
    })
    
    # plot the most n common words in description
    output$plot5 <- renderPlot({
        num<-input$max2
        list_org %>% unnest_tokens(word,description) %>%
          anti_join(stop_words) %>%
            count(word, sort=TRUE) %>% 
            mutate(word= reorder(word,n)) %>%
            filter(!is.na(word)) %>%
            head(num) %>%
            ggplot(aes(n, word)) +
            geom_col() +
            labs(y=NULL)
        
    })
    output$plot2 <- renderPlot({
        num<-3
        if (input$num_area <20) { num <-input$num_area}
        plot_area<-list_org %>% 
            separate(location, into=c("area","state"),sep=",") %>%
            filter(!is.na(state)) %>%
            count(state, sort=TRUE) %>% 
            mutate(state= reorder(state,n)) %>%
            head(num) %>%
            ggplot(aes(state,n)) +
            geom_col() +
            labs(y=NULL)+ ylab("Position") +xlab("State")
        
        plot_area })
    
    
    output$plot3 <- renderPlot({
        #num2 <- input$company
        plot_company <-list_org %>%
            filter(!is.na(company)) %>%
            count(company, sort=TRUE) %>% 
            mutate(company= reorder(company,n)) %>%
            head(10) %>%
            ggplot(aes(company,n)) +
            geom_col() +
            labs(y=NULL) + ylab("Position") +xlab("Company")+
            theme(axis.text.x = element_text(size = 12, color = "darkblue", vjust = 0.5, hjust = 0.5, angle = 45))
        plot_company}) 
    
    #biogram plot
    output$plot4 <- renderPlot({
        library(ggraph)  #devtools::install_github('thomasp85/ggraph')
        library(igraph)
        count_bigrams <- function(dataset) {
            dataset %>%
                unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word) %>%
                count(word1, word2, sort = TRUE)
        }
        
        visualize_bigrams <- function(bigrams) {
            set.seed(2016)
            
            bigrams %>%
                ggraph (layout = "fr") +
                geom_edge_link() +
                geom_node_point(color = "lightgreen", size = 5) +
                geom_node_text(aes(label = name), vjust = 1, hjust = 1)
        }
        #take it to the text-list_org
        list_org %>%
            count_bigrams()%>%
            filter(n > 3 & !is.na(word1) & !is.na(word2)) %>%
            graph_from_data_frame() %>%
            visualize_bigrams()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
