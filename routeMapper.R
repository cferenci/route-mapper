library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(googleway)
library(fontawesome)

#### server
server <- function(input, output, session) {
  
  ##Google API Key
  
  api_key <- "AIzaSyBo4AHjlO0qlcbDMX0i_WyAgxzQlAWlmDM"
  
  ##render Google map
  
  output$map <- renderGoogle_map({
    
    #set map to santa monica, eventually want geolocate
    latlongSM <- c(34.0195, -118.4912)
    
    google_map(key = api_key, event_return_type = "list", location = latlongSM, zoom = 15)
    
  })

  
  lat_long <- reactiveValues(originLocationDF = data.frame(lat = c(), long = c()))
  
  observeEvent(
    input$map_map_click, {
      
      
      #create origin lat/lon
      originLat <- input$map_map_click$lat
      originLon <- input$map_map_click$lon
      
      #print(input$map_map_click)
      
      #update startingAddress input value
      updateTextInput(session, "startingAddress", value = paste(originLat, originLon, sep = ", "))
      
      lat_long$originLocationDFnew <- data.frame(lat = originLat, lon = originLon)
      
      lat_long$originLocationDF <- bind_rows(lat_long$originLocationDF,
                                             lat_long$originLocationDFnew)
    
      lat_long$originLocationDF <- tail(lat_long$originLocationDF, n=2)
      
      print(lat_long$originLocationDF)
      
      #update google map view and add markers
      google_map_update(map_id="map", data = lat_long$originLocationDFnew) %>%
        add_markers(update_map_view = FALSE)
      
      
    }
    
    google_directions()
    
  )
  
  output$example <- renderTable(lat_long$originLocationDF)
  
  
  
}

#### user interface
ui <- fluidPage(
  
  titlePanel("Route Creator"),

  
    
  mainPanel(
    fluidRow(h3("Create Route"),
             
             column(4,
                    
                    textInput(inputId = "startingAddress", label = "Origin", value = "Origin Location..."),
                    
                    textInput(inputId = "endingAddress", label = "Destination"),
                    
                    #radioButtons(inputId = "routeType", label = "Select Route Type", choices = list("Most greenspace" = 1, "Least Polluted Route" = 2, "Most Efficient Route" = 3), selected = 1),
                    
                    actionButton("centerMaponAddress", "Create Route"),
                    
                    
             ), #end box
             
             column(8,
                    
                    google_mapOutput(outputId = "map"), 
                    tableOutput("example")
                    
             ),)
    

        )#end dashbooard body
        
      ) #end dashboardPage


shinyApp(ui = ui, server = server)