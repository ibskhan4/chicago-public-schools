#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#

library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Show a plot of the generated distribution

    navbarPage("Chicago Public Schools",
               tabPanel("about",
                HTML(readLines("~/Documents/ml6/about_proj.html", skip = 4))),
               tabPanel("Data",
                        plotOutput("image"))
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$image <- renderImage({
        # Return a list containing the filename
        list(src = "graph.png",
             width = 800,
             height = 800,
             contentType = 'image/png',
             alt = "This is alternate text"
        )}, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)