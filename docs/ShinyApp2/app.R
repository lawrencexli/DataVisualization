#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)
library(rsconnect)
library(stringr)
library(hrbrthemes)
data <- read.csv("./Data/us_data_1976_2020.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("journal"),
  
    # Application title
    titlePanel("Distribution of Unemployment Rate, Labor Participation Rate, and Minimum Wage"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Year:",
                        min = 1976,
                        max = 2020,
                        value = 2020)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"),
           plotlyOutput("distPlot2"),
           plotlyOutput("distPlot3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
      year = input$bins
      data_year = data[data$year == year, ]
      
      
      i <- ggplot(data_year, aes(x = Percent.Unemployment.Rate, y = ..density..)) +
        geom_histogram(binwidth = 0.4, fill = "#d1ab75", alpha = 0.8) + 
        scale_y_continuous(breaks = seq(0, 0.7, 0.1), limits=c(0, 0.7)) +
        scale_x_continuous(breaks = seq(0, 20, 5), limits=c(0, 20)) +
        scale_color_ipsum() +
        stat_density(geom="line",color="#d18980") +
        ylab("Density") +
        xlab("Unemployment Rate in Percentage") +
        ggtitle("Density Distribution of Unemployment Rates") +
        theme_ipsum_tw() +
        geom_vline(data=data_year, aes(xintercept=mean(Percent.Unemployment.Rate)),
                   color="#d18975",
                   linetype="dashed") +
        theme(legend.position = "none") 
    
      i
    })
    
    output$distPlot2 <- renderPlotly({
      year = input$bins
      data_year = data[data$year == year, ]
      
      
      j <- ggplot(data_year, aes(x = PercentLaborForceParticipation, y = ..density..)) +
        geom_histogram(binwidth = 0.4, fill = "#758bd1") + 
        scale_y_continuous(breaks = seq(0, 0.7, 0.1), limits=c(0, 0.7)) +
        scale_x_continuous(breaks = seq(50, 80, 5), limits=c(50, 80)) +
        scale_color_ipsum() +
        stat_density(geom="line",color="#75b8e1") +
        ylab("Density") +
        xlab("Labor Participation Rate in Percentage") +
        ggtitle("Density Distribution of Labor Force Participation Rate") +
        theme_ipsum_tw() +
        geom_vline(data=data_year, aes(xintercept=mean(PercentLaborForceParticipation)),
                   color="#75b8d1",
                   linetype="dashed") +
        theme(legend.position = "none") 
      
      j
    })
    
    output$distPlot3 <- renderPlotly({
      year = input$bins
      data_year = data[data$year == year, ]
      
      
      k <- ggplot(data_year, aes(x = actualminimumwage2020dollars)) +
        geom_histogram(binwidth = 0.4, fill = "#2d543d") + 
        scale_y_continuous(breaks = seq(0, 51, 5), limits=c(0, 51)) +
        scale_x_continuous(breaks = seq(0, 15, 5), limits=c(0, 15)) +
        scale_color_ipsum() +
        ylab("Count") +
        xlab("Actual Minimum Wage in 2020 US dollars, inflation adjusted") +
        ggtitle("Counting Distribution of Minimum Wage") +
        theme_ipsum_tw() +
        geom_vline(data=data_year, aes(xintercept=mean(actualminimumwage2020dollars)), 
                   color="#8fd175",
                   linetype="dashed") +
        theme(legend.position = "none") 
      
      k
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
