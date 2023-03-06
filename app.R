#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
data <- read_delim("UAH-lower-troposphere-long.csv.bz2")


ui <- navbarPage(
  
  # Application title
  titlePanel("Problem Set 6"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "range", label = "Select the range of years", value = c(1978,2018),min = 1978, max = 2023),
      radioButtons("Color", "Choose Color", 
                   choices = c("red", "green", "darkgrey", "yellow", "blue")),
      selectInput("region", "Select a region", choices = unique(data$region))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", textOutput("summary"),p("This dataset contains", nrow(data), "and", ncol(data), "coloumns")
                 ,tableOutput("SumTable")),
        tabPanel("Plot", plotOutput("plot"), p("This graph represents shows the temperature between the years")),
        tabPanel("Table", textOutput("text"),tableOutput("table"))
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  data1 <- reactive({
    subset(data, year>input$range[1] & year<input$range[2])
  })
  
  output$SumTable <- renderTable({
      data %>% 
      sample_n(5)
  })
  output$plot <- renderPlot({
  data1() %>% 
      group_by(year) %>% 
      ggplot(aes(year, temp))+
      geom_point(col = input$Color)+ggtitle(paste("temp vs time between the years", input$range[1], "and", input$range[2]))+
      geom_smooth()
    })
  output$text <- renderPrint({
    paste("you are looking at:",as.character(input$region))
  })
   output$table <- renderTable({
      Regionfil <- subset(data, data$region == input$region)
    })  
}
# Run the application 
shinyApp(ui = ui, server = server)
