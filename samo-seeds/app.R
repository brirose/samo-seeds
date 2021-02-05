

#SAMO Seeds app

library(tidyverse)
library(shiny)
library(here)

### define data etc once its clean



# Define UI for application
ui <- fluidPage(
    
    navbarPage("SAMO Seeds",
               tabPanel("Overview"
                   
               ),
               tabPanel("Flora",
                        sidebarLayout(
                            sidebarPanel("widgets",
                                        # PUT WIDGETS HERE
                            ),
                            mainPanel("output",
                                      plotOutput("HERE"))
                        )
               ),
               tabPanel("Phenology"),
               tabPanel("Map of Locations"),
               tabPanel("Herbarium Label")
               
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ##Need to clean data to use it lol
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
