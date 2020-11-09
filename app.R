library(shiny)
library(shiny.semantic)
library(data.table)
library(leaflet)
library(shinyjs)

ship <- fread("data/ships.csv")
ship[, DATETIME := as.POSIXct(DATETIME, "%Y-%m-%d %H:%M:%S")]

ui <- semanticPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  shinyjs::useShinyjs(),
  
  h1(class = "ui header", "Ship Path"),
  actionButton("browser", "browser"),
  tags$script("$('#browser').hide();"),
  
  ShipSelctUI(id = "ShipSelect", selectype = ship[, unique(ship_type)]),

  div(class = "ui raised segment",
      div(a(class="ui green ribbon label", "Map"),
          leafletOutput("mapship"))
      )
  )

server <- shinyServer(function(input, output, session) {

  observeEvent(input$browser,{
    browser()
  })
  
# JS function -------------------------------------------------------------

  observe({shinyjs::toggleState("ShipSelect-nam", shipselec$typ() != "")})
  
# Observers ---------------------------------------------------------------

  shipselec <- callModule(ShipSelct, "ShipSelect", date = ship)
  
# Reactives ---------------------------------------------------------------

  dat <- reactive({
    
    validate(need(shipselec$typ() != '', "Please select a boat type"))
    validate(need(shipselec$nam() != '', "Please select a boat name"))
    
    df <- distnM(x = ship, time = "DATETIME", 
                 shipT = shipselec$typ(), 
                 shipN = shipselec$nam(), ret = "last",
                 units = input$un, 
                 up_units = as.numeric(input$up))
    
    validate(need(!is.null(df), "The route with the selected criteria does not exist."))
    
    # Coordinates and Notes
    metr <- sprintf("%s meters duration %s %s", df[, round(dist, 2)], df$tm, input$un)
    po <- rbind(df[, .(LAT, LON, Not = sprintf("Route Start %s", metr))], 
                df[, .(LAT = LAT2, LON = LON2, Not = sprintf("Route Stop %s", metr))])
    
    return(list(df = df, po = po))
  })

# Output ------------------------------------------------------------------

  output$mapship <- renderLeaflet({
    
    leaflet(dat()$po) %>%
      addTiles() %>%  
      addMarkers(lng = ~ LON, lat = ~ LAT, label = ~ Not,
                 labelOptions = labelOptions(noHide = T, 
                                             style = list("color" = "red"))) %>% 
      addPolylines(lat = as.numeric(dat()$df[, .(LAT, LAT2)]), 
                   lng = as.numeric(dat()$df[, .(LON, LON2)])) 
    
  })
  
})

shinyApp(ui = ui, server = server)

