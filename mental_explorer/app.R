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
# Data wrangling
survey$Gender<-gsub("\\<f\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<F\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<female\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<woman\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<Woman\\>","Female",survey$Gender)

survey$Gender<-gsub("\\<m\\>","Male",survey$Gender)
survey$Gender<-gsub("\\<M\\>","Male",survey$Gender)
survey$Gender<-gsub("\\<male\\>","Male",survey$Gender)

survey<-survey%>%
  filter(Gender == c("Female", "Male"))


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
    
    data<-survey %>%
      select(input$programInput,Gender,tech_company,Age, treatment)
    
    if (input$techCheck){
      data<-data %>%
        filter(tech_company == "Yes")
    }

    
    t<-data %>%
      ggplot(aes(x=treatment, color = Gender))+
      geom_bar()

    
    print(t)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

