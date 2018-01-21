# Created by Susan Fung, January 2018
#
# Description:
# Interactive visualization that shows mental health treatment and attitude in the workplace

# Packages required
library(shiny)
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)


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
  
  # Sidebar with 2 controls 
  sidebarLayout(
    sidebarPanel(
      
      # Checkbox Group to filter by company size 
      checkboxGroupInput("sizeInput", "Company Size",
                         choices = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"),
                         selected = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000")),
      
      # Single checkbox to select tech companies
      checkboxInput('techCheck', 'Show Tech Companies Only')
  ),    
    
    # Show bar charts
    mainPanel(
      plotOutput("treatmentPlot"),
      plotOutput("attitudePlot")
    )
  )
)


server <- function(input, output) {
  
  # Reactivity to filter data based on user input
  filtered <- reactive({
    if (input$techCheck){
      survey %>%
        filter(no_employees %in% input$sizeInput)%>%
        filter(tech_company == "Yes")
    }
    else{
      survey %>%
        filter(no_employees %in% input$sizeInput)
    }
  })
  
  # Bar chart that shows the percentage of respondents who had sought treatment
  output$treatmentPlot <- renderPlot({
    t<-filtered() %>%
      ggplot(aes(x=age_group, fill = treatment))+
      coord_flip() +
      geom_bar(position = "fill")+
      xlab("Age Group")+
      ylab("Percentage")+
      facet_wrap(~Gender)+
      ggtitle("Mental Health Treatment")+
      scale_colour_brewer(palette = "Pastel1")
    
    print(t)
  })
  
  # Bar chart that shows the attitude towards mental health illness
  output$attitudePlot <- renderPlot({
    t<-filtered() %>%
      select(coworkers, supervisor, obs_consequence)%>%
      gather(attitude, Response)%>%
      ggplot(aes(x=attitude, fill = Response))+
      coord_flip() +
      geom_bar(position = "fill")+
      scale_x_discrete(labels = c("Will you talk to your supervisors?","Have you observed any negative consequences?", "Will you talk to your coworkers?"))+
      ylab("Percentage")+
      xlab("")+
      ggtitle("Attitude towards Mental Health Illness")+
      scale_colour_brewer(palette = "Pastel1")
    
    print(t)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)