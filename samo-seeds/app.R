# functions
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(here)
library(lubridate)
library(tmap)

#tmap setup
tmap_mode("view")

# data

## spatial
col_loc <- read_sf(here("data/loc.gpkg"))
samo_plants <- read_sf(here("data/desired_natives_samo.gpkg"))

## other
flowering_times <- read_csv(here("data/flowering_times.csv"))



# UI 

ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "SAMO Seeds",
    tabPanel(
      "Overview",
      mainPanel(
        width = 12,
        p("Conservation collection of seed necessitates careful planning.
          This tool aids seed collectors in organizing collection trips."),
        img(src = "IMG_20200420_114317.jpg", 
            width = 400,
            align = "left"),
        h4("Developer"),
        p("Bri Baker | 2021 \n"),
        h4("Data Sources"),
        p("CalFlora data download tool (2021)"),
        p("Santa Monica Mountains Rancho Sierra Vista 
          seed collection program data (2020)")
      )
    ),# close overview
    tabPanel(
      "Flora",
      sidebarLayout(
        sidebarPanel(
          "Choose Taxon",
          selectInput(
            "select", 
            label = h5("Select taxon of interest"), 
            inputId = "flora_taxon",
            choices = unique(flowering_times$taxon)
          )
        ),# close sidebar
        mainPanel(
          "Species information",
          tmapOutput(
            "loc_map"
          )
        ) # close main panel
      )
    ) # close flora tab
  )
)

# SERVER
server <- function(input, output) {
####----FLORA TAB ----
  ##filter points
   flora_loc <- reactive({
    samo_plants %>% 
      filter(taxon %in% input$flora_taxon)
  })
  ##tmap  
  output$loc_map <- renderTmap({
    tm_shape(flora_loc())+
      tm_dots() 
  })
  
}

shinyApp(ui = ui, server = server)