#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Random Normally Distributed Numbers in a histogram"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
 
        sliderInput("numOfNum",
                    "Number of Numbers:",
                    min = 1000,
                    max = 5000,
                    value = 3000),
               
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 12,
                    value = 5),
        
        textInput(inputId = "Header",label = "Header for graph",value = "Histogram of X")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         plotlyOutput("histPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  theNumbers <- reactive({
    rnorm(input$numOfNum)
  })
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- theNumbers()
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',main = input$Header)
   })
   
   output$histPlot <- renderPlotly({
     # generate bins based on input$bins from ui.R
     x    <- theNumbers()
     minX <- min(x)
     maxX <- max(x)+0.00001
     sizeX <- (maxX+0.00001-minX)/input$bins
     xbins=list(start=minX,end=maxX,size=sizeX)
     plot_ly()%>%
       add_trace(x = x,type = "histogram",xbins=xbins,cumulative=list(enabled=TRUE),name="cumulative distribution",marker=list(line=list(color="white",width=1)))%>%
       add_trace(x = x,opacity = 1,type = "histogram",xbins=xbins,name="distribution",marker=list(line=list(color="yellow",width=1),color="blue"))%>%
       layout(title=input$Header,barmode='overlay')%>%
       config(displayModeBar = FALSE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

