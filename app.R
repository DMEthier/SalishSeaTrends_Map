library(shiny)
library(leaflet)
library(dplyr)
library(readr)
#library(naturecounts)

ui <- fluidPage(
  titlePanel("Salish Sea Coastal Waterbird Trends"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "species",
        label = "Select Species:",
        choices = NULL, # choices will be updated in server
        selected = NULL
      )
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  print("Server Started")
  # Read and preprocess data
  trends <- read.csv("Data/SalishSea_Species_TrendsSlope_SPDE.csv") %>% 
    tidyr::drop_na(results_code) %>% 
    select(area_code, species_code, species_id, trnd, lower_ci, upper_ci, percent_change)
  print("Trends Loaded")
  species<-read.csv("Data/Species.csv")
  print("Species Loaded")
  events <- read_csv("Data/events.csv") %>%
    select(SurveyAreaIdentifier, DecimalLatitude, DecimalLongitude) %>%
    distinct()
  print("Events Loaded")
  
  # Merge trends with site coordinates
  trends_map <- trends %>%
    left_join(events, by = c("area_code" = "SurveyAreaIdentifier"), relationship = "many-to-many")
  
  trends_map <- trends_map %>% 
    left_join(species, by="species_id")
  
  # Update species choices in selectInput
  observe({
    species_choices <- sort(unique(trends_map$english_name))
    updateSelectInput(session, "species",
                      choices = species_choices,
                      selected = species_choices[1])
  })
  
  # Reactive filtered data based on selected species
  filtered_data <- reactive({
    req(input$species)
    trends_map %>%
      filter(english_name == input$species) %>%
      mutate(
        popup = paste0(
          "<b>Area:</b> ", area_code, "<br>",
          "<b>Species:</b> ", english_name, "<br>",
          "<b>Trend (Annual % Change):</b> ", round(trnd, 2), "<br>",
          "<b>95% CI:</b> [", round(lower_ci, 2), ", ", round(upper_ci, 2), "]<br>",
          "<b>Percent Change:</b> ", round(percent_change, 1), "%<br>"
        )
      )
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    pal <- colorNumeric(palette = "RdYlBu", domain = data$trnd, reverse = TRUE)
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~DecimalLongitude,
        lat = ~DecimalLatitude,
        radius = 8,
        color = ~pal(trnd),         # border color
        fillColor = ~pal(trnd),     # fill color (same as border)
        stroke = TRUE,              # border around the point
        weight = 1,                 # border thickness
        opacity = 1,                # border opacity
        fillOpacity = 1,            # fill opacity (fully solid)
        popup = ~popup
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~trnd,
        title = "Trend (slope)",
        opacity = 1
      )
  })
}

shinyApp(ui, server)