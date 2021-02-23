

#SAMO Seeds app

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(here)

# DATA

## spatial data
col_loc <- here("data/loc.gpkg")
samo_plants <- here("data/natives_samo.gpkg")

## other data
flowering_times <- here("data/flowering_times.csv")






# Define UI for application
ui <- fluidPage(
  #setBackgroundImage(src = "IMG_20200407_133353.jpg"), # image as background; not sure I like it
  theme = shinytheme("flatly"), # theme may change if I have time :)
                
                navbarPage("SAMO Seeds",
                           tabPanel("Overview",
                                   mainPanel(
                                     p("Conservation collection of seed necessitates careful planning. This tool aids seed collectors in organizing collection trips."),
                                     img(src = "IMG_20200420_114317.jpg", 
                                         width = 400,
                                         align = "center"),
                                     h4("Data"),
                                     p("CalFlora data download tool (2021)"),
                                     p("Santa Monica Mountains Rancho Sierra Vista seed collection program data (2020)")
                                   ) 
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
