library(shiny)
library(deckgl)
library(shinythemes)
library(shinydashboard)
library(RColorBrewer)
library(readxl)
library(leaflet)
library(ggplot2)
library(mapboxapi)

final_data <- read.csv("zurich station final.csv")



plot_data <- read.csv("day bus.csv") 
plot_data$frequency <- plot_data$frequency.h
plot_data$busline <- as.character(plot_data$busline)
plot_data <- plot_data[order(plot_data$busline), ]

sorted_stations <- sort(final_data$CHSTNAME)

final_data$popup <- paste(
  "<b>Station: </b>", final_data$CHSTNAME, "<br>",
  "<b>Tram: </b>", final_data$tram_line, "<br>",
  "<b>Bus: </b>", final_data$bus_line, "<br>",
  "<b>Train: </b>", final_data$train_line
)


zurich_station <- read.csv("zurich station.csv")
my_token = "pk.eyJ1IjoidGFuZ2ppbiIsImEiOiJjbHY4bHp0ejYwa2JjMm1uMWNsaWRqZHF1In0.woE71XgVyNj27hNmF41sOg"
Sys.setenv(MAPBOX_API_TOKEN = my_token)

header <- dashboardHeader(
  title = span("Daylight Public Transportation in Zurich", style = "display: inline-block; white-space: nowrap;"),
  titleWidth = 400
)

body <- dashboardBody(
  fluidRow(
    column(4,
           box(width = NULL, solidHeader = TRUE, status = "warning",
               sliderInput("train_weight", "Train Weight", min = 0, max = 1, value = 0.5, step = 0.1),
               sliderInput("tram_weight", "Tram Weight", min = 0, max = 1, value = 0.5, step = 0.1),
               sliderInput("bus_weight", "Bus Weight", min = 0, max = 1, value = 0.5, step = 0.1),
               sliderInput("radius", "Radius", min = 100, max = 500, value = 200),
               radioButtons("style", "Map Style:",
                            c("3D Map" = "TRUE",
                              "2D Map" = "FALSE")
               ),
               tags$div("Hold Ctrl to rotate the map."),
               height = "600px"
           )
    ),
    column(8,
           box(width = NULL, solidHeader = TRUE,
               deckglOutput("map", height = "600px")
           )
    )
  ),
  fluidRow(
    column(12,
           box(width = 12,solidHeader = TRUE, height = 530,leafletOutput("leafmap", height = 500)
    ))
  ),
  
  
  fluidRow(
    column(3,box(solidHeader = TRUE, status = "warning",width = 12, height = 330,
           fluidRow(
             column(12,
                    selectInput("station", "Select Station", choices = c("", sorted_stations))
             ),
             column(12,
                    div(class = "button-container",
                        actionButton("resetButton", "Reset Map View")
                    )
             )
           ))
    ),
    column(9, 
           box(offset = 1, solidHeader = TRUE,width = 12, height = 330,
               plotOutput("freqPlot", click = "plot_click", height = 300)
           )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body,
  tags$head(
    tags$style(HTML("
      .wrapper {
        max-width: 1200px;
        margin: 0 auto;
      }
      .main-header .logo,
      .main-header .navbar {
        width: 100%;
        max-width: 1200px;
        margin: 0 auto;
      }
      .skin-blue .main-header .logo{
    background-color: #057a8d;
    color: #fff;
    border-bottom: 0 solid transparent;
      }
      .skin-blue .main-header .navbar {
    background-color: #0c8195;
      }
    "))
  )
)


server <- function(input, output) {
  
  # Initialize legend outside render function
  colors <- brewer.pal(8, "RdBu")
  labels <- seq(28, 0, by = -4)
  legend_added <- FALSE
  
  output$map <- renderDeckgl({ {
      # Otherwise, add hexagon layer with the updated data
      zurich_station$total <- input$train_weight / (input$train_weight + input$tram_weight + input$bus_weight) * zurich_station$train + 
        input$tram_weight / (input$train_weight + input$tram_weight + input$bus_weight) * zurich_station$tram + 
        input$bus_weight / (input$train_weight + input$tram_weight + input$bus_weight) * zurich_station$bus
      
      deck <- deckgl(
        initialViewState = list(
          latitude = 47.3769,
          longitude = 8.5417,
          zoom = 11,
          pitch = 55,
          bearing = 30
        )
      ) %>%
        add_hexagon_layer(
          data = zurich_station,
          getPosition = ~X_KOORD + Y_KOORD,
          getColorWeight = ~total,
          getElevationWeight = ~total,
          colorAggregation = "sum",
          elevationAggregation = "sum",
          properties = list(
            extruded = (input$style == 'TRUE'),
            radius = input$radius,
            elevationScale = 4,
            elevationDomain = c(0, 30),
            colorDomain = c(0, 30),
            colorRange = rev(colors),
            tooltip = "
            <p>Station count: {{points.length}}</p>
            <p>{{#points}}<div>{{CHSTNAME}}</div>{{/points}}</p>",
            onClick = JS("obj => console.log(obj)"),
            autoHighlight = TRUE,
            getElevationValue = JS("points => points.reduce((acc, point) => acc + point.total, 0)")
          )
        ) %>%
        add_mapbox_basemap("mapbox://styles/mapbox/light-v11")
      
      # Add legend only if it's not already added
      if (!legend_added) {
        deck <- deck %>%
          add_legend(
            colors,
            c("28-32", "24-28", "20-24", "16-20", "12-16", "8-12", "4-8", "0-4"),
            title = "cumulative <br> hourly <br> frequency",
            pos = "top-right",
            style = "max-width: 200px;"
          )
        legend_added <<- TRUE  # Update legend_added to TRUE
      }
      
      deck
    }
  })
  selected_bus <- reactive({
    req(input$plot_click)
    clicked_x <- input$plot_click$x
    clicked_busline <- plot_data$busline[which.min(abs(clicked_x - seq_along(plot_data$busline)))]
    clicked_busline
  })
  
  filtered_points <- reactive({
    req(selected_bus())
    busline <-as.character(selected_bus())
    # Specify the range of columns to check
    columns_to_check <- 26:ncol(final_data)
    matches <- apply(final_data[columns_to_check], 1, function(row) {
      any(busline %in% row)
    })
    filtered <- final_data[matches, ]
    filtered
  })
  
  # Render the category table
  output$freqPlot <- renderPlot({
    clicked_busline <- reactiveVal(NULL)
    ggplot(plot_data, aes(x = busline, y = frequency)) +  
      geom_bar(stat = "identity", width = 0.8, fill = "darkorange") +  
      scale_x_discrete(expand = c(0, 0)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      labs(x = "Bus Line", y = "Frequency/h", title = "Bus Line Frequencies")
  })
  
  
  # Render the Leaflet map
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = final_data$X_KOORD, lat = final_data$Y_KOORD, 
                       radius = 3, fillOpacity = 1,
                       popup = final_data$popup) %>%
      addLegend("bottomright", colors = "blue", labels = "Stations", title = "Legend")
    
    
  })
  
  # Observe the filtered points and update the map
  observe({
    points <- filtered_points()
    leafletProxy("leafmap", data = points) %>%
      clearMarkers() %>%
      addCircleMarkers(~X_KOORD, ~Y_KOORD, popup = ~popup) %>%
      fitBounds(lng1 = min(points$X_KOORD), lat1 = min(points$Y_KOORD),
                lng2 = max(points$X_KOORD), lat2 = max(points$Y_KOORD))
  }) 
  
  observeEvent(input$station, {
    selected_station <- final_data$CHSTNAME[which(final_data$CHSTNAME == input$station)]
    if (!is.null(selected_station)) {
      station_coords <- final_data[final_data$CHSTNAME == selected_station, c("X_KOORD", "Y_KOORD")]
      leafletProxy("leafmap") %>%
        addCircleMarkers(lng = station_coords$X_KOORD, lat = station_coords$Y_KOORD, color = "darkorange", 
                         fillColor = "darkorange", fillOpacity = 1, layerId = "selectedStationMarker",
                         popup = final_data[final_data$CHSTNAME == selected_station, "popup"]) %>%
        setView(lng = station_coords$X_KOORD, lat = station_coords$Y_KOORD, zoom = 14)
    }
  })
  
  observeEvent(input$resetButton, {
    leafletProxy("leafmap") %>%
      clearMarkers() %>%
      addCircleMarkers(lng = final_data$X_KOORD, lat = final_data$Y_KOORD, 
                       radius = 3, fillOpacity = 1, popup = final_data$popup) %>%
      fitBounds(lng1 = min(final_data$X_KOORD), lat1 = min(final_data$Y_KOORD),
                lng2 = max(final_data$X_KOORD), lat2 = max(final_data$Y_KOORD))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
