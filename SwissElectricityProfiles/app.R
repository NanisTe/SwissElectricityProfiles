#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# DEBUGGING wit:
# cat(stderr(),...,"\n")
# browser() but stops calculation

library(shiny)
library(hash)
library(dygraphs)






# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
   # Application title
   titlePanel("Swiss Electricity Profiles Visualisation"),
   uiOutput("uiLang"),
   textOutput("uiPrint"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        uiOutput("uiYear")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         dygraphOutput("ElProfile")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  selLang <- hash()
  selLang["en"] <- "Language"
  selLang["de"] <- "Sprache"
  
  
  selYear <- hash()
  selYear["en"] <- "Select Year"
  selYear["de"] <- "Wähle das Jahr"
  
  #Load data
  load("./dygraphs_ElDatPlots.Rdata", envir = .GlobalEnv)
  
  rv <- reactiveValues()
  rv$Lang <- selLang[["en"]]
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   
   
   # UI
   output$uiLang <- renderUI({
     selectInput("uiLang",
                 "",
                 c("en","de","fr","it")
                 )
   })
   output$uiYear <- renderUI({
     selectInput("uiYear",
                 selYear[[input$uiLang]],
                 as.character(seq.int(2010,2018)))
   })
   
   output$ElProfile <- renderDygraph({
     # plot dygraph if the year string is not empty
     y <- as.character(input$uiYear)
     if(!length(y) == 0 & class(y)=="character"){
       cat(stderr(),y,"\n")
       
       ElDat.Plots[[y]]
     }
   })
   
   output$uiPrint <- renderText({rv$Lang})
   
   observeEvent(input$uiLang,{rv$Lang <- selLang[[input$uiLang]]})
     
}

# Run the application 
shinyApp(ui = ui, server = server)

