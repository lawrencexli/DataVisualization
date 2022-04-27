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
library(usmap)
library(shinythemes)
library(rsconnect)
library(stringr)
data <- read.csv("./Data/us_data_1976_2020.csv")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
    theme = shinytheme("journal"),

    # Application title
    titlePanel("Unemployment Rate, Labor Participation Rate, and Minimum Wage by State"),

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
           plotlyOutput("plot"),
           plotlyOutput("plot2"),
           plotlyOutput("plot3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlotly({
      year = input$bins
      data_year = data[data$year == year, ]
      

      x <- plot_usmap(data = data_year, values = "Percent.Unemployment.Rate", 
                       color = "black") + 
        scale_fill_continuous(low = "white", high = "darkorange",
                              name = "Percentage Unemployment Rate",
                              label = scales::comma,
                              limits=c(min(data $ Percent.Unemployment.Rate), 
                                       max(data $ Percent.Unemployment.Rate))) + 
        theme(legend.position = "right")
      
      
      ggplotly(x)
      
    })
    
    output$plot2 <- renderPlotly({
      year = input$bins
      data_year = data[data$year == year, ]
      
      y <- plot_usmap(data = data_year, values = "PercentLaborForceParticipation", 
                      color = "black") + 
        scale_fill_continuous(low = "white", high = "darkblue",
                              name = "Percentage Labor Force Participation",
                              label = scales::comma,
                              limits=c(min(data $ PercentLaborForceParticipation), 
                                       max(data $ PercentLaborForceParticipation))) + 
        theme(legend.position = "right")
      
      
      ggplotly(y)
      
    })
    
    output$plot3 <- renderPlotly({
      year = input$bins
      data_year = data[data$year == year, ]
      
      z <- plot_usmap(data = data_year, values = "actualminimumwage2020dollars", 
                      color = "black") + 
        scale_fill_continuous(low = "white", high = "darkgreen",
                              name = "Minimum Wage 2020 dollars, inflation adjusted",
                              label = scales::comma,
                              limits=c(min(data $ actualminimumwage2020dollars), 
                                       max(data $ actualminimumwage2020dollars))) + 
        theme(legend.position = "right") + 
        labs(caption="H")
      
      
      ggplotly(z)
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
