#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(kableExtra)
source("utils.R")
library(dplyr)


# Define UI (interface server) for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "World Trade Flows"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      h1(paste0("Supply Chain: World Info on Semiconductors Industry"), align="center", 
         style = "font-family: 'Arial'; font-si16pt"),
      h4(paste0("Please select the phase, products, and number of partners")),
      #Create a select list input control, "phase" debe ser parte del input de server
      column(width = 4, selectInput("phase", "Phase of the Supply Chain",
                                    choices = c("Back end" = "Back end", 
                                                "Front end" = "Front end"), width = NULL)),
      column(width = 4, selectInput("fraction", "Top X partners",
                                    choices = c("10" = 10, 
                                                "50" = 50,
                                                "100" = 100,
                                                "250" = 250,
                                                "500" = 500
                                                ), width = NULL)),
      #products debe ser parte del output de server
      column(width = 4, uiOutput("products")),
      #Use leafletOutput() to create a UI element, and renderLeaflet() to render the map widget.
      h2(paste0("Map: Export flows")),
      column(width = 12, box(leafletOutput("map"), width = NULL)),
      h2(paste0("Trade Information Table")),
      column(width = 12, box(tableOutput("statstable"), width = NULL))
             
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #It is similar to a list, but with special capabilities for reactive programming
  values_react <- reactiveValues()
  
  #Cambiar nombres de los paises y ordenar database
  trade_db <- read_csv("db_trade_f.csv") %>% arrange_db()
  
  
  #Create a reactive observer
  observe({
    values_react$phase <- input$phase
    values_react$fraction <- input$fraction
    values_react$filtered_db <- trade_db %>% filter(PHASE == values_react$phase)
    values_react$lst_uprod <- unique(values_react$filtered_db[, "cmdCode"])
  })
  
  
  output$products <- renderUI({
    selectInput(inputId="choose_product",
                #titulo del botón
                label="Product",
                choices = values_react$lst_uprod, width = NULL)
  })
  
  output$map <- renderLeaflet({
    filtered_by_product <- values_react$filtered_db %>% filter(cmdCode == input$choose_product) %>%
      #mutate() adds new variables and preserves existing ones
      mutate(percent_value = TradeValue / sum(TradeValue) * 100, 
             percent_quant = TradeQuantity / sum(TradeQuantity) * 100)
    filtered_by_product <- head(arrange(filtered_by_product, desc(TradeValue)), n = input$fraction)
    countries <- get_geo_nodes(filtered_by_product)
    nodes_import <- graph.data.frame(filtered_by_product, directed=TRUE, countries)
    network_import <- get.data.frame(nodes_import, "both")
    vert_import <- network_import$vertices
    coordinates(vert_import) <- ~longitude + latitude
    edges_import <- network_import$edges
    edges_import <- lapply(1:nrow(edges_import), function(i) {
      as(rbind(vert_import[vert_import$name == edges_import[i, "from"], ],
               vert_import[vert_import$name == edges_import[i, "to"], ]),
         "SpatialLines")
    })
    
    for (i in seq_along(edges_import)) {
      #When the feature IDs need to be changed in SpatialLines* or SpatialPolygons*
      #objects, these methods may be used
      edges_import[[i]] <- spChFIDs(edges_import[[i]], as.character(i))
    }
    edges_import <- do.call(rbind, edges_import)
    
    #AddMarkers
    holder_coordinates <- matrix(NA, nrow = length(edges_import), ncol = 2)
    
    for (i in seq_along(edges_import)) {
      holder_coordinates[i,1] = edges_import@lines[[i]]@Lines[1][[1]]@coords[2]
      holder_coordinates[i,2] = edges_import@lines[[i]]@Lines[1][[1]]@coords[4]
    }
    
    holder_coordinates <- as.data.frame(holder_coordinates)
    colnames(holder_coordinates) <- c("Long", "Lat")
    ##
    
    leaflet(vert_import) %>%
      addTiles() %>%
      addCircles(data = vert_import, radius = 1000, weight = 10, color = "navy", label = vert_import$name )%>%
      addPolylines(data = edges_import, weight = filtered_by_product$percent_value, label = filtered_by_product$percent_value)%>%
      addMarkers(holder_coordinates$Long, holder_coordinates$Lat,
                 icon = list(
                   iconUrl = 'marker.png',
                   iconSize = c(25, 25)))
  })
  
  output$statstable <- function() {
    filtered_by_product <- values_react$filtered_db %>% filter(cmdCode == input$choose_product) %>%
      #mutate() adds new variables and preserves existing ones
      mutate(percent_value = TradeValue / sum(TradeValue) * 100, 
             percent_quant = TradeQuantity / sum(TradeQuantity) * 100)
    stats_table <- gen_stats_table(filtered_by_product) %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
    
}



# Run the application 
shinyApp(ui = ui, server = server)
