# Created by Susan Fung, January 2018
#
# Description:
# Interactive visualization that shows the global well-being using GDP per capita and life expectancy. 


# Packages required
library(shiny)
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)

survey <- read_csv("../data/survey.csv")


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
      plotOutput("treatmentPlot")
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  
  output$treatmentPlot <- renderPlot({
    t<-survey %>%
      ggplot(aes(x=treatment, color = Gender))+
      geom_bar()

    
    print(t)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

