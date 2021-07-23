library(shiny)
library(ggplot2)

## Define UI for miles per gallon application

# dataset <- diamonds
dataset <- read.csv("~/Documents/Datasets/trauma_dataset.csv")
dataset <- dataset %>% mutate(age_group=dplyr::case_when(
  age <= 18            ~ "0-18",
  age > 18 & age <= 30 ~ "18-30",
  age > 30 & age <= 45 ~ "30-45",
  age > 45 & age <= 64 ~ "45-64",
  age > 64             ~ "> 64"
  ),
  age_group = factor(
    age_group,
    level = c("0-18", "18-30", "30-45", "45-64", "> 64")
  )
)

title <- "EMS data correspondence analysis"

## Define UI for application that plots random distributions

shinyUI(pageWithSidebar(
  
  ## Application title
  headerPanel(title),
  
  ## Sidebar with a slider input for number of observations
  
  sidebarPanel (
    
    sliderInput('sampleSize','Sample Size', min=1, max=nrow(dataset),
                value=nrow(dataset),
                step=500,
                round=0),
    
    selectInput('x','var1',names(dataset), names(dataset)[[24]]),
    selectInput('y','var2',names(dataset), names(dataset)[[11]]),
    
    selectInput('color','Color',c('None',names(dataset))),
    
    selectInput('shape','Shape',c('None',names(dataset)))
    
  ),
  
  
  mainPanel(
    h3("contingency table"),
    tableOutput("cotable"),
    h3("chi sq p value"),
    textOutput("p"),
    h3("screeplot"),
    plotOutput('screeplot'),
    h3("correspondence vis"),
    plotOutput('corrplot')
    )
  
  
))
