# functions
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(here)
library(lubridate)
library(tmap)
library(sf)

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
    
    ####----TAXON TAB ----####
    tabPanel(
      "Flora",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            h4("Select taxon of interest"), 
            inputId = "taxon_taxon",
            choices = unique(flowering_times$taxon)
          ),
          h4("Species information"),
          textOutput(
            "sp_info"
          )
        ),# close sidebar
        mainPanel(
          tmapOutput(
            "loc_map"
          )
        ) # close main panel
      )
    ), # close TAXON tab
    
    ####----FLOWERING TAB ----####
    tabPanel(
      "In Flower",
      sidebarLayout(
        sidebarPanel(
          "Collection List",
          checkboxGroupInput(
            "Select collection months",
            inputId = "flowering_months",
            choices = unique(flowering_times$month)
          )
        ),
        mainPanel(
          "Collection species in flower",
          tableOutput("flowering_table")
        )
      )
      
    ), # close FLOWERING tab
    
    ####----LOCATION TAB----####
    tabPanel(
      "Location",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "Select Location",
            inputId = "loc_name",
            choices = col_loc$name
          ),
          radioButtons(
            "Select Month",
            inputId = "loc_month",
            choices = unique(flowering_times$month)
          )
        ),
        mainPanel(
          tmapOutput(
            "flowering_map"
          )
        )
      )
    )# close LOC tab
  )
)

# SERVER
server <- function(input, output) {
  
####----TAXON TAB ----####
  ##--filter from inputs--##
   taxon_loc <- reactive({
    samo_plants %>% 
      filter(taxon %in% input$taxon_taxon)
  })
   
   taxon_info <- reactive({
     flowering_times %>% 
       filter(taxon %in% input$taxon_taxon) %>% 
       slice(1)
   })
     
  ##--outputs--##
   #-map-#
  output$loc_map <- renderTmap({
    tm_basemap("Esri.WorldTopoMap") +
    tm_shape(taxon_loc())+
      tm_dots() 
  })
  
  #-taxon info-#
  output$sp_info <- renderText({
    paste(
       input$taxon_taxon, "(", taxon_info()$common_name, 
       ") is a ", taxon_info()$lifeform,
       "of family", taxon_info()$family,
       " that flowers from ", taxon_info()$start_bloom, 
       " to ", taxon_info()$end_bloom, "."
       
    )
  })
  
####----FLOWERING TAB ----####
  ##--filter from inputs--##
  flowering_taxa <- reactive({
    flowering_times %>% 
      filter(month %in% input$flowering_months) %>% 
      dplyr::select(taxon, month) %>% 
      mutate(id = as.numeric(factor(taxon))) %>% 
      pivot_wider(id_cols = id,
                  names_from = month,
                  values_from = taxon) %>% 
      dplyr::select(-id)
  })
  
  ##--output table--##
  output$flowering_table <- renderTable({
    flowering_taxa()
  })
  
####----LOCATION----####
  ##--filter from inputs--##
  loc_place <- reactive({
    col_loc %>% 
      filter(
        name %in% input$loc_name
      )
  })
  
  loc_time <- reactive({
    flowering_times %>% 
      filter(
        month %in% input$loc_month
      )
  })
  
  loc_flowering <- reactive({samo_plants %>% 
      filter(taxon %in% loc_time()$taxon) %>% 
      st_filter(loc_place(), 
                join = st_within)
  })
    
  ##--output--##
  output$flowering_map <- renderTmap({
    tm_basemap("Esri.WorldTopoMap") +
    tm_shape(loc_flowering())+
      tm_dots(col = "taxon") 
  })
  
}

shinyApp(ui = ui, server = server)