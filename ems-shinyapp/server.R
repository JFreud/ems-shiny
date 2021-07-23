library(shiny)
library(ggplot2)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(reshape2)

trauma <- read.csv("~/Documents/Datasets/trauma_dataset.csv")
trauma <- trauma %>% mutate(age_group=dplyr::case_when(
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

shinyServer(function(input,output) {
  
  dataset <- reactive(
    trauma[sample(nrow(trauma), input$sampleSize),] 
  )
  
  output$p <- renderText({
    contin_table <- with(dataset(), table(get(input$x), get(input$y)))
    contin_table <- contin_table[!apply(contin_table<10,1,any,na.rm=T),]
    return(chisq.test(contin_table)$p.value)
  })
  
  output$cotable <- renderTable({
    contin_table <- with(dataset(), table(get(input$x), get(input$y)))
    contin_table <- contin_table[!apply(contin_table<10,1,any,na.rm=T),]
    return(contin_table)
  })
  
  output$screeplot <- renderPlot({
    
    contin_table <- with(dataset(), table(get(input$x), get(input$y)))
    contin_table <- contin_table[!apply(contin_table<10,1,any,na.rm=T),]
    res.ca <- CA(contin_table, graph = FALSE)
    
    return(fviz_screeplot(res.ca, addlabels = TRUE))
  })
  
  output$corrplot <- renderPlot({
    
    contin_table <- with(dataset(), table(get(input$x), get(input$y)))
    contin_table <- contin_table[!apply(contin_table<10,1,any,na.rm=T),]
    
    res.ca <- CA(contin_table, graph = FALSE)
    
    return(fviz_ca_biplot(res.ca, repel = TRUE))
  })
})


