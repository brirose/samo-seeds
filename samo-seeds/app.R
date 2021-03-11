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
          This tool aids seed collectors in organizing collection trips.",
          style = "padding-bottom: 10px"),
        img(src = "IMG_20200420_114317.jpg", 
            width = 350,
            align = "left",
            style = "padding-right: 30px; padding-bottom: 20px"),
        h4("Tools"),
        p("Flora: Use this to see the locations of
          individuals of a species you choose"),
        p("In Flower: Use this to make a list of species in flower 
          for a given month, or compare between months"),
        p("Location: Use this to see the locations 
          of species at a given collection location"),
        p("Commonly Collected Species: Use this to show new collectors 
          some species they might be looking for"),
        h4("Developer",
           style = "padding-top: 40px"),
        p("Bri Baker | 2021",
          style = "padding-bottom: 10px"),
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
          tableOutput(
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
    ),# close LOC tab
    
    ####----ID----####
    tabPanel(
      "Commonly Collected Species",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "select_plant",
            label = h5("Select plant species:"),
            choices = list(
              "Anemopsis californica",
              "Artemisia douglasiana",
              "Encelia californica",
              "Eriogonum cinereum",
              "Eriogonum fasciculatum",
              "Eschscholzia californica",
              "Grindelia camporum",
              "Phacelia grandiflora",
              "Rosa californica",
              "Salvia leucophylla",
              "Solanum xantii"
            )
          ),
          textOutput(
            "plant_info"
          )
        ),
        mainPanel(
          uiOutput("plantimg")
        )
      )
    )
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
  output$sp_info <- renderTable({
    data.frame(
      "Attribute" = c("Taxon", 
                      "Common Name", 
                      "Lifeform", 
                      "Family"
      ),
      "Info" = c(input$taxon_taxon,
                 taxon_info()$common_name,
                 taxon_info()$lifeform,
                 taxon_info()$family
      )
    )
   # paste(
      # input$taxon_taxon, "(", taxon_info()$common_name, 
      # ") is a ", taxon_info()$lifeform,
      # "of family", taxon_info()$family,
      # " that flowers from ", taxon_info()$start_bloom, 
      # " to ", taxon_info()$end_bloom, "."
       
   # )
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
  
  ####----ID----####
  
  img_info <- reactive({
    flowering_times %>% 
      filter(taxon %in% input$select_plant) %>% 
      slice(1)
  })
  
  ###---outputs---###
  output$plant_info <- renderText({
    paste(
      img_info()$taxon, "is also known as", img_info()$common_name, 
      "and is a", img_info()$lifeform,
      "of family", img_info()$family,
      " that flowers from ", img_info()$start_bloom, 
      " to ", img_info()$end_bloom, "."
      
    )
  })
  
  output$plantimg <- renderUI({
    if(input$select_plant == "Anemopsis californica"){
      img(height = 350, src = "ane_cal.jpg")}
    else if(input$select_plant == "Artemisia douglasiana"){
      img(height = 350, src = "art_dou.jpg")}
    else if(input$select_plant == "Encelia californica"){
      img(height = 350, src = "enc_cal.jpg")}
    else if(input$select_plant == "Eriogonum cinereum"){
      img(height = 350, src = "eri_cin.jpg")}
    else if(input$select_plant == "Eriogonum fasciculatum"){
      img(height = 350, src = "eri_fas.jpg")}
    else if(input$select_plant == "Eschscholzia californica"){
      img(height = 350, src = "esc_cal.jpg")}
    else if(input$select_plant == "Grindelia camporum"){
      img(height = 350, src = "gri_cam.jpg")}
    else if(input$select_plant == "Phacelia grandiflora"){
      img(height = 350, src = "pha_gra.jpg")}
    else if(input$select_plant == "Rosa californica"){
      img(height = 350, src = "ros_cal.jpg")}
    else if(input$select_plant == "Salvia leucophylla"){
      img(height = 350, src = "sal_leu.jpg")}
    else if(input$select_plant == "Solanum xantii"){
      img(height = 350, src = "sol_xan.jpg")}
  })
  
}

shinyApp(ui = ui, server = server)