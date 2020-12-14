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

## tidy text with unnest_tokens(), take each word into a token
data(stop_words)
frequency<- list_org %>%
    unnest_tokens(word,description) %>% 
    anti_join(stop_words) %>%
    mutate(word = str_extract(word, "[a-z']+"))   %>% count(word)

# Define UI for application that draws a histogram
ui <-dashboardPage(
    skin="green",
    dashboardHeader(title = "Job of Data Scientists"),
    dashboardSidebar(
        width = 150,
        sidebarMenu(
            menuItem("Dashboard", tabName = "tab1", icon = icon("dashboard")),
            menuItem("Word Frequency", tabName = "tab2", icon = icon("dashboard")),
            menuItem("Areas and companies with most job positions", tabName = "tab3", icon = icon("th"))
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
            # Show Word Cloud
        box(plotOutput("plot1"), title = "Word Cloud", status = "primary"),
        
        box( collapsible = TRUE,solidHeader = TRUE,#background = "lime", 
            title = "Control of word cloud",
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 1,  max = 50, value = 15),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1,  max = 300,  value = 100))
        )),
        
        tabItem(tabName = "tab3",
                plotOutput("plot2"),
                plotOutput("plot3")
               
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
        wordcloud_rep(frequency$word, frequency$n, scale=c(4,1.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))
    })
    
    
    output$plot2 <- renderPlot({
        plot_area<-list_org %>% 
            separate(location, into=c("area","state"),sep=",") %>%
            filter(!is.na(state)) %>%
            count(state, sort=TRUE) %>% 
            mutate(state= reorder(state,n)) %>%
            head(10) %>%
            ggplot(aes(state,n)) +
            geom_col() +
            labs(y=NULL)
        plot_area})
    
    
    output$plot3 <- renderPlot({
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
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
