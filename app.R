library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)

ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  titlePanel("National TB prevalence survey - sample size calculator"),
  
   sidebarLayout(
      sidebarPanel(
        sliderInput("cluster", "Cluster size", 300, 1000, c(500), step=100),
        numericInput("prev", "Bacteriologically confirmed prevalence per 100 000 (all ages)", min=100, max=1500, value=500),
        sliderInput("prop", "Percentage of the population 15+ years",10, 80, c(64), step=1, post ="%"),
        sliderInput("prec", "Precision", 0.20, 0.30, c(0.25), step=0.05),
        sliderInput("k", "Level of k", 0.4, 0.6, c(0.6), step =0.1),
        sliderInput("part", "Participation rate", 80, 100, c(85), step=1, post ="%"),
      ),
  
    mainPanel(
    
    h4("The estimated sample size is:", align = "left"),
    
    textOutput("clustertext"),
    
    hr(),
    print("The calculator is based on the formula presented in chapter 5 from the Tuberculosis prevalence survey: a handbook (v2)  
          which can be downloaded from here:
          https://apps.who.int/iris/bitstream/handle/10665/44481/9789241548168_eng.pdf?sequence=1
        "),
  
    ),
       
  ))

server <- function(input, output) {
  
output$clustertext <- renderText(
    
((   
  (1+((input$cluster-1)        
        *(((input$k*input$k)*
             ( (input$prev/(input$prop/100))/100000)         
          / ( 1- ( (input$prev/(input$prop/100))/100000) )
        )))))
   *
  ( 
    (1.96*1.96)
   *((( 1- ( (input$prev/(input$prop/100))/100000) )) /    
       ( (input$prec*input$prec) *  (input$prev/(input$prop/100))/100000) )
  ))   
/ (input$part/100)
   )}

shinyApp(ui = ui, server = server)
