# Created by Susan Fung, January 2018
#
# Description:
# Interactive visualization that shows the global well-being using GDP per capita and life expectancy. 


# Packages required
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)


# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Mental Health Explorer"),
  
  # Sidebar with 4 controls 
  sidebarLayout(
    sidebarPanel(

      # Checkbox Group to filter by company size 
      checkboxGroupInput("sizeInput", "Company Size",
                         choices = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"),
                         selected = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000")),
      
      # Single checkbox to select tech companies
      checkboxInput('techCheck', 'Show Tech Companies Only'),
      
      # Checkbox Group to filter by program 
      checkboxGroupInput("programInput", "Program",
                         choices = c("Remote Work" = "remote_work",
                                     "Mental Health Benefits" = "benefits",
                                     "Self-help resources" = "seek_help",
                                     "Anonymity" = "anonymity"),
                         selected = c("Remote Work" = "remote_work",
                                      "Mental Health Benefits" = "benefits",
                                      "Self-help resources" = "seek_help",
                                      "Anonymity" = "anonymity"))

    ),
    
    
    # Show explanation and scatterplot
    mainPanel(
      h4("My well-being indicator consists of GDP per capita and life expectancy. Countries in the upper right corner are properous and healthy, while countries in the lower left corner are less so"),
      
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  

  survey <- read_csv("../data/survey.csv")

  
  output$scatterPlot <- renderPlot({
    p<-gapminder %>%
      filter(year >= input$yearSlider[1],
             year <= input$yearSlider[2],
             continent == input$continentInput) %>%
      ggplot(aes(x=lifeExp, y=gdpPercap, color = continent))+
      geom_point(alpha=input$tranSlider, size = input$sizeSlider)+
      scale_y_continuous(name="GDP per Capita (USD)",labels=scales::dollar_format()) +
      scale_x_continuous(name="Life Expectancy (years)")+
      ggtitle("Global GDP per Capita and Life Expectancy")
    
    if (input$trendCheck){
      p<-p + geom_path()
    }
    
    print(p)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

