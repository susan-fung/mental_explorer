# Created by Susan Fung, January 2018
#
# Description:
# Interactive visualization that shows percentage of respondents that claim they have sought mental health treatment

# Packages required
library(shiny)
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)

# Read data
survey <- read_csv("../data/survey.csv")


# Clean-up gender data
survey$Gender<-gsub("\\<f\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<F\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<female\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<woman\\>","Female",survey$Gender)
survey$Gender<-gsub("\\<Woman\\>","Female",survey$Gender)

survey$Gender<-gsub("\\<m\\>","Male",survey$Gender)
survey$Gender<-gsub("\\<M\\>","Male",survey$Gender)
survey$Gender<-gsub("\\<male\\>","Male",survey$Gender)

# Create age group
survey$age_group <- as.character(cut(survey$Age, breaks = c(20,30,40,50,60,Inf),
                                     labels=c('20-29', '30-39', '40-49', '50-59', 'Over 60')))

survey<-survey%>%
  filter(Gender %in% c("Female", "Male"))%>%
  filter(!is.na(age_group))


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
      checkboxInput('techCheck', 'Show Tech Companies Only')
  ),    
    
    # Show bar chat
    mainPanel(
      plotOutput("treatmentPlot")
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  filtered <- reactive({
    if (input$techCheck){
      survey %>%
        select(Gender,tech_company,age_group,treatment, no_employees)%>%
        filter(no_employees %in% input$sizeInput)%>%
        filter(tech_company == "Yes")
    }
    else{
      survey %>%
        select(Gender,tech_company,age_group,treatment, no_employees)%>%
        filter(no_employees %in% input$sizeInput)
    }
  })
  
  output$treatmentPlot <- renderPlot({
    t<-filtered() %>%
      filter(treatment =="Yes")%>%
      ggplot(aes(x=age_group, fill = Gender))+
      coord_flip() +
      geom_bar(position = "fill")+
      xlab("Age Group")+
      ylab("Percentage")+
      ggtitle("Percentage of Respondents Who Had Sought Mental Health Treatment")
    
    print(t)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)