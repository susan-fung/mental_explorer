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
library(scales)

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

# Filter NAs
survey<-survey%>%
  filter(Gender %in% c("Female", "Male"))%>%
  filter(!is.na(age_group))

# Recode attitude
survey$coworkers[survey$coworkers == "Some of them"]<-"Yes"
survey$supervisor[survey$supervisor == "Some of them"]<-"Yes"
survey$obs_consequence[survey$obs_consequence == "Yes"]<-"temp"
survey$obs_consequence[survey$obs_consequence == "No"]<-"Yes"
survey$obs_consequence[survey$obs_consequence == "temp"]<-"No"


ui <- fluidPage(
  
  # Application title
  titlePanel("Mental Health Illness and Attitude in the Workplace"),
  
  # Sidebar with 2 controls 
  sidebarLayout(
    sidebarPanel(
      
      # Checkbox Group to filter by company size 
      checkboxGroupInput("sizeInput", "Company Size",
                         choices = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"),
                         selected = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000")),
      
      # Single checkbox to select tech companies
      radioButtons("techCheck", "Company Type",
                         choices = c("Tech Companies Only", "All Companies"),
                         selected = c("All Companies"))
      
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
    if (input$techCheck == "Tech Companies Only"){
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
    df<-filtered() %>% 
        group_by(age_group, Gender, treatment)%>%
        summarise(n = n())%>%
        mutate(prob = n/sum(n)) %>%
        filter(treatment == "Yes")
      
    t<-df %>%
      ggplot(aes(x=age_group, y = prob, fill = Gender))+
      geom_bar(data = subset(df, Gender %in% "Female"), stat = "summary", fun.y="mean")+
      geom_bar(data = subset(df, Gender %in% "Male"), aes(y = -prob), stat = "summary", fun.y="mean") +
      coord_flip()+
      scale_fill_manual(name="Gender",values=c("Female"="darkorange", "Male"="blue1"))+
      xlab("Age Group")+
      ylab("Percentage")+
      scale_y_continuous(labels = percent)+
      ggtitle("Percentage of Respondents Who Had Sought Mental Health Treatment")
    
    print(t)
  })
  
  # Bar chart that shows attitude towards mental health illness
  output$attitudePlot <- renderPlot({
    t<-filtered() %>%
      select(coworkers, supervisor, obs_consequence)%>%
      gather(attitude, Response)%>%
      ggplot(aes(x=attitude, fill = Response))+
      coord_flip() +
      geom_bar(position = "fill")+
      scale_x_discrete(labels = c("I will talk to my coworkers","I will talk to my supervisors", 
                                  "I have not observed any negative consequences"))+
      ylab("Percentage")+
      scale_y_continuous(labels = percent)+
      xlab("")+
      ggtitle("Attitude towards Mental Health Illness")+
      scale_fill_manual(name = "Response", values = c("Yes"="olivedrab3", "No"="orangered2"))
    
    print(t)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)