# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to estimate the sample size for a national TB prevalence survey
# Irwin Law, January 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)

ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  # Give the page a title
  
  titlePanel("National TB prevalence survey - sample size calculator"),
  
  # Generate a sidebar for 6 user defined inputs
  
  sidebarLayout(
      sidebarPanel(
        sliderInput("cluster", "Cluster size", 300, 1000, c(500), step=100),
        numericInput("prev", "Bacteriologically confirmed prevalence per 100 000 (all ages)", min=100, max=1500, value=500),
        sliderInput("prop", "Percentage of the population 15+ years",10, 80, c(64), step=1, post ="%"),
        sliderInput("prec", "Precision", 0.20, 0.30, c(0.25), step=0.05),
        sliderInput("k", "Level of k", 0.4, 0.6, c(0.6), step =0.1),
        sliderInput("part", "Participation rate", 80, 100, c(85), step=1, post ="%"),
    ),
  
  # Generate a small section with the answers  
  
  mainPanel(
    
    h4 ("The estimated sample size is:", align = "left"),
    textOutput("clustertext1"),
    h4("The estimated number of clusters  is:", align = "left"),
    textOutput("clustertext2"),
    hr(),
    
  # Generate some notes
  
    HTML(paste0("<div id='metadata'>",
              "<i>",
              "The calculator is based on the formula presented in chapter 5 from the 
              <a href='https://apps.who.int/iris/bitstream/handle/10665/44481/9789241548168_eng.pdf?sequence=1'target='_blank'> WHO tuberculosis prevalence survey: a handbook (v2)</a>. ",
              "Source code on <a href='https://github.com/GTB-TBPS' target='_blank'>Github</a>.
              </i><br /><br /></div>"))),
    ))

server <- function(input, output) {
  
# Render the output for the calculation 
  
#p<-(((1+((input$cluster-1)*(((input$k*input$k)*
#     ((input$prev/(input$prop/100))/100000) / ( 1- ( (input$prev/(input$prop/100))/100000)))))))*
#     ((1.96*1.96)*((( 1- ((input$prev/(input$prop/100))/100000) )) / 
#     ((input$prec*input$prec)*(input$prev/(input$prop/100))/100000)))) / (input$part/100)

  
output$clustertext1 <- renderText({
 
(((1+((input$cluster-1)*(((input$k*input$k)*
((input$prev/(input$prop/100))/100000) / ( 1- ( (input$prev/(input$prop/100))/100000)))))))*
((1.96*1.96)*((( 1- ((input$prev/(input$prop/100))/100000) )) / 
((input$prec*input$prec)*(input$prev/(input$prop/100))/100000)))) / (input$part/100)

})

output$clustertext2 <- renderText({
  (((1+((input$cluster-1)*(((input$k*input$k)*
                              ((input$prev/(input$prop/100))/100000) / ( 1- ( (input$prev/(input$prop/100))/100000)))))))*
     ((1.96*1.96)*((( 1- ((input$prev/(input$prop/100))/100000) )) / 
                     ((input$prec*input$prec)*(input$prev/(input$prop/100))/100000)))) / (input$part/100) / (input$cluster)
  
})

}

shinyApp(ui = ui, server = server)
