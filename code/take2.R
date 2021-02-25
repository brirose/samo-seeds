

#SAMO Seeds app

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(here)

# DATA

## spatial data
col_loc <- read_sf(here("data/loc.gpkg"))
samo_plants <- read_sf(here("data/natives_samo.gpkg"))

## other data
flowering_times <- read_csv(here("data/flowering_times.csv"))





# UI definition
ui <- fluidPage(
  #setBackgroundImage(src = "IMG_20200407_133353.jpg"), # image as background; not sure I like it
  theme = shinytheme("flatly"), # theme may change if I have time :)
  navbarPage("SAMO Seeds",
             tabPanel("Overview",
                      mainPanel(
                        p("Conservation collection of seed necessitates careful planning.
                                        This tool aids seed collectors in organizing collection trips."),
                        img(src = "IMG_20200420_114317.jpg", 
                            width = 400,
                            align = "center"),
                        h4("Data"),
                        p("CalFlora data download tool (2021)"),
                        p("Santa Monica Mountains Rancho Sierra Vista 
                                       seed collection program data (2020)")
                      ) 
             ),
             tabPanel("Flora",
                      sidebarLayout(
                        sidebarPanel("Choose Taxon",
                                     selectInput("select", 
                                                 label = h5("Select taxon of interest"), 
                                                 inputId = "pick_taxon",
                                                 choices = unique(flowering_times$taxon)
                                     )
                        ),
                        mainPanel("output",
                                  plotOutput("plot_pls")
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

# Server Definition
server <- function(input, output) {
  
  # taxon 
  taxon_reflective <- reactive({
    flowering_times %>% 
      filter(taxon %in% input$pick_taxon)
  })
  ## outputs taxon
  
  output$plot_pls <- renderPlot(
    ggplot(data = taxon_reflective(), aes(x = year)) +
      geom_col()
  )
  
  output$text <- renderText("caption")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

tabPanel("Prey Descriptions",
         sidebarLayout(
           sidebarPanel(selectInput(inputId = "select_prey", 
                                    label = h5("Select prey species:"), 
                                    choices = list("Peppermint Shrimp" = "pep",
                                                   "Mysid Shrimp" = "mys",
                                                   "Cardinalfish" = "car",
                                                   "Saddle Blenny" = "sad",
                                                   "Other" = "oth"),
                                    selected = "pep"),
           ),
           mainPanel(uiOutput("img1"),
                     textOutput("description"))

output$img1 <- renderUI({
  if(input$select_prey == "pep"){
    img(height = "75%", width = "75%", src = 'https://cdn11.bigcommerce.com/s-sid5v/images/stencil/2048x2048/products/1091/3349/2__49824.1553225499.jpg?c=2')}
  else if(input$select_prey == "mys"){
    img(height = "75%", width = "75%", src = 'https://reefs.com/blog/wp-content/uploads/2003/09/breeder1a-146b69570826ead8b1c21be3489a5300.jpg')}
  else if(input$select_prey == "car"){
    img(height = "75%", width = "75%", src = 'http://www.nad-lembeh.com/wp-content/uploads/2013/09/MG_5104.jpg')}
  else if(input$select_prey == "sad"){
    img(height = "75%", width = "75%", src = 'https://www.petmd.com/sites/default/files/Labrisomid-Saddl-Blenny%209990733.jpg')}
}
