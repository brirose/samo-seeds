

#SAMO Seeds app

library(tidyverse)
library(shiny)
library(shinythemes)

### define data etc once its clean



# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"), # theme may change if I have time :)
                
                navbarPage("SAMO Seeds",
                           tabPanel("Overview"
                                    
                           ),
                           tabPanel("Flora",
                                    sidebarLayout(
                                        sidebarPanel("Choose Taxon",
                                                     selectInput("select", label = h3("Select box"), 
                                                                 choices = list("Choice 1" = 1, 
                                                                                "Choice 2" = 2, 
                                                                                "Choice 3" = 3), 
                                                                 selected = 1)
                                        ),
                                        mainPanel("output",
                                                  plotOutput("HERE")
                                        )
                                    )
                           ),
                           
                           tabPanel("Phenology",
                                    sidebarLayout(
                                        sidebarPanel("widgets",
                                                     
                                                     selectInput("select", label = h3("Select box"), 
                                                                 choices = list("Choice 1" = 1, 
                                                                                "Choice 2" = 2, 
                                                                                "Choice 3" = 3), 
                                                                 selected = 1),
                                                     radioButtons("radio", label = h3("Radio buttons"),
                                                                  choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                                  selected = 1)
                                                     
                                        ),
                                        mainPanel("output",
                                                  plotOutput("HERE"))
                                    )
                           ),
                           
                           tabPanel("Map of Locations",
                                    sidebarLayout(
                                        sidebarPanel("widgets",
                                                     checkboxGroupInput("checkGroup", 
                                                                        label = h3("Checkbox group"), 
                                                                        choices = list("Choice 1" = 1, 
                                                                                       "Choice 2" = 2, 
                                                                                       "Choice 3" = 3),
                                                                        selected = 1)
                                        ),
                                        mainPanel("output",
                                                  plotOutput("HERE"))
                                        )
                                    ),
                           tabPanel("Herbarium Label",
                                    sidebarLayout(
                               sidebarPanel("widgets",
                                            textInput("text", 
                                                      label = h3("Text input"), 
                                                      value = "Enter text..."), # PUT WIDGETS HERE
                               ),
                               mainPanel("output",
                                         plotOutput("HERE"))
                               )
                           )
                           
               
    )
)

# Define server logic

server <- function(input, output) {

    ##Need to clean data to use it lol
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
