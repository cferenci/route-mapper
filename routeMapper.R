library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(googleway)
#library(fontawesome)

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
      
      lat_long$originLocationDFnew <- data.frame(lat = originLat, lon = originLon)
      
      lat_long$originLocationDF <- bind_rows(lat_long$originLocationDF,
                                             lat_long$originLocationDFnew)
      
      lat_long$originLocationDFhead <- head(lat_long$originLocationDF, 2)
      
      updateTextInput(session, "startingAddress", value = paste(round(lat_long$originLocationDFhead[1, 1], 2), 
                                                                round(lat_long$originLocationDFhead[1,2], 2), sep = ", "))
      
      if(nrow(lat_long$originLocationDF) != 1){
      updateTextInput(session, "endingAddress", value = paste(round(lat_long$originLocationDFhead[2, 1], 2), 
                                                              round(lat_long$originLocationDFhead[2,2], 2), sep = ", "))
      }
      
    

      #update google map view and add markers
      if(nrow(lat_long$originLocationDF) <= 2 ){
      google_map_update(map_id="map", data = lat_long$originLocationDFnew) %>%
        add_markers(update_map_view = FALSE)
      }
      
    }
    
    #google_directions()
    
    
  )
  
  #clear markers

  observeEvent(input$clearMarkers,{
     google_map_update(map_id="map") %>%
       clear_markers()
    
    updateTextInput(session, "startingAddress",
                    value = paste("Origin Location..."))
    
    updateTextInput(session, "endingAddress",
                    value = paste("Destination..."))
   
    session$reload() 
  }
)
  
  output$example <- renderTable(lat_long$originLocationDFhead)
  
  
  
}

#### user interface
ui <- tags$html(
  
    #html head
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
    
    #BEGIN CONTENT
  
    #start body
    tags$body(
      
      #Header Navigation
      tags$nav(class = "navbar sticky-top navbar-dark bg-dark",
               tags$a(class = "navbar-brand", href="#", "RouteR"),
               
               tags$div(class="navbar-nav justify-content-end",
                        
                        tags$a(class="nav-item", href="#", "Test")
                        ,
                        
                        tags$a(class="nav-item", href="#", "Trends")
                        ,
                        
                        tags$a(class="nav-item btn btn-success", href="#", "Create New Route")
                        )
      ),
               
      
      tags$div(class="container-fluid",
               
               tags$div(class = "row",
                 
                 tags$div(class = "col-4 pt-3",
                        
                        h3("Create Route"),
                        
                        textInput(inputId = "startingAddress", label = "Origin", value = "Origin Location..."),
                        
                        textInput(inputId = "endingAddress", label = "Destination", "Destination..."),
                        
                        #radioButtons(inputId = "routeType", label = "Select Route Type", choices = list("Most greenspace" = 1, "Least Polluted Route" = 2, "Most Efficient Route" = 3), selected = 1),
                        
                        actionButton("centerMaponAddress", "Create Route", class = "btn-primary"),
                        
                        actionLink("clearMarkers", "Clear Markers")
                        
                        
                 ), #endcolumn
                 
                 tags$div(class="col-8",
                        
                        google_mapOutput(outputId = "map")
                        
                 )#endcolumn
                 
               )#endRow
      ),#endTabPanel
      
      tabPanel("My Data",
               p("Summary / History of Data"))
      
    )
    
)##end body
    
    
              
    
        

shinyApp(ui = ui, server = server)