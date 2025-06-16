library(shiny)
library(leaflet)
library(dplyr)
library(readr)
#library(naturecounts)

ui <- fluidPage(
  titlePanel(HTML("Salish Sea Coastal Waterbird Trends<br>Winter 2008/09 - 2024/25")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "species",
        label = "Select Species:",
        choices = NULL,
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
  
  trends_map <- trends_map %>%
    mutate(
      english_name = case_when(
        english_name %in% c("gull (large)", "Iceland Gull", "Iceland Gull (Thayer's)", "Western x Glaucous-winged Gull (hybrid)", "Glaucous Gull", "Glaucous-winged Gull", "Western Gull", "Herring Gull", "Iceland (Thayer's) Gull", "Iceland (Thayer's Gull)", "WEGU x GWGU hybrid", "California Gull") ~ "Large Gull",
        english_name %in% c("scaup sp.", "Lesser Scaup", "Greater Scaup", "Greater/Lesser Scaup") ~ "Greater-Lesser Scaup",
        english_name %in% c("Eared Grebe", "Horned Grebe") ~ "Eared-Horned Grebe",
        english_name %in% c("Canada Goose", "Cackling Goose") ~ "Canada-Cackling Goose",
        english_name %in% c("Clark's Grebe", "Western Grebe") ~ "Western-Clark's Grebe",
        TRUE ~ english_name
      )
    )
  
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
    pal <- colorNumeric(
      palette = c("red", "white", "blue"),
      domain = data$trnd
    )
    
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
        title = "Trend (annual % change)",
        opacity = 1
      )
  })
}

shinyApp(ui, server)