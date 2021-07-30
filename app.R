
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
library(stringr)
library(leaflet)


# Define UI (interface server) for application that draws a histogram

header <- 
  dashboardHeader( title = HTML("Mapping Supply Chains"), 
                   disable = FALSE, 
                   titleWidth  = 550)

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='https://www.gob.mx/se/',
                                             tags$img(src='se_logo.png'),
                                             target = '_blank')


siderbar <- 
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     id = 'sidebar', 
                     style = "position: relative; overflow: visible;",
                     #first tab: general statistics for all the supply chains
                     menuItem("Statistics of Supply Chains", tabName = "all_statistics",
                              icon = icon('chart-line'), badgeColor = 'green'),
                     #second tab: map and specific information of the Semiconductors Supply Chain
                     menuItem("Semiconductors Supply Chain", tabName = "semiconductors",
                              icon = icon('microchip'), badgeColor = 'green'),
                     #third tab: map and specific information of the Batteries Supply Chain
                     menuItem("Batteries Supply Chain", tabName = "batteries",
                              icon = icon('battery-full'), badgeColor = 'green'),
                     #fourth tab: map and specific information of the Pharmaceutical Supply Chain
                     menuItem("Pharmaceutical Supply Chain", tabName = "pharma",
                              icon = icon('file-prescription'), badgeColor = 'green')
                   ))

body <- dashboardBody(
  tags$head(
    tags$script("document.title = 'Mexico: Supply Chain Info'"),
    tags$style(HTML('
                    /* logo */
                    .skin-blue .main-header .logo {
                    background-color: #696969;
                    }
                    /* logo when hovered */
                    .skin-blue .main-header .logo:hover {
                    background-color: #696969;
                    }
                    /* navbar (rest of the header) */
                    .skin-blue .main-header .navbar {
                    background-color: #696969;
                    }
                    /* active selected tab in the sidebarmenu */
                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                    background-color: #696969;
                    }
                    ')
    ),
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") )),
  tabItems(
    tabItem(tabName = 'semiconductors',
            fluidRow(
              h1(paste0("Supply Chain: World Info on Semiconductors Industry"), align="center", 
                 style = "font-family: 'Arial'; font-si16pt"),
              h4(paste0("Select the phase, products, and number of partners")),
              #Create a select list input control, "phase" debe ser parte del input de server
              column(width = 4, selectInput("phase", "Phase of the Supply Chain",
                                            choices = c("Back end" = "Back end", 
                                                        "Front end" = "Front end"), width = NULL)),
              #products debe ser parte del output de server
              column(width = 4, uiOutput("products")),
              column(width = 4, selectInput("fraction", "Top X partners",
                                            choices = c("10" = 10, 
                                                        "25" = 25,
                                                        "100" = 100,
                                                        "250" = 250,
                                                        "500" = 500
                                            ), width = NULL)),
              #Use leafletOutput() to create a UI element, and renderLeaflet() to render the map widget.
              h2(paste0("Map: Export flows")),
              column(width = 12, box(leafletOutput("map"), width = NULL)),
              h2(paste0("Trade Information Table")),
              column(width = 12, box(tableOutput("statstable"), width = NULL))
              
            ))
  ))

ui <- dashboardPage(header, siderbar, body)



server <- function(input, output) {
  values_react <- reactiveValues()
  
  #Cambiar nombres de los paises y ordenar database
  trade_db <- read_csv("db_trade_f.csv") %>% arrange_db()
  trade_db <- add_description(trade_db)
  
  
  #Create a reactive observer
  observe({
    values_react$phase <- input$phase
    values_react$fraction <- input$fraction
    values_react$filtered_db <- trade_db %>% filter(PHASE == values_react$phase)
    values_react$lst_uprod <- unique(values_react$filtered_db[, "codes_descrip"])
  })
  
  
  output$products <- renderUI({
    selectInput(inputId="choose_product",
                #titulo del botón
                label="Product",
                choices = values_react$lst_uprod, width = NULL)
  })
  
  output$map <- renderLeaflet({
    filtered_by_product <- values_react$filtered_db %>% filter(codes_descrip == input$choose_product) %>%
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
      holder_coordinates[i,1] = edges_import@lines[[i]]@Lines[1][[1]]@coords[1]
      holder_coordinates[i,2] = edges_import@lines[[i]]@Lines[1][[1]]@coords[3]
    }
    
    holder_coordinates <- as.data.frame(holder_coordinates)
    colnames(holder_coordinates) <- c("Long", "Lat")
    ##
    
    content <- paste(round(filtered_by_product$percent_value, 2), "Exporter:", filtered_by_product$From, sep = " ")
    
    leaflet(vert_import) %>%
      addTiles() %>%
      addCircles(data = vert_import, radius = 1000, weight = 10, color = "navy", label = vert_import$name )%>%
      addPolylines(data = edges_import, weight = filtered_by_product$percent_value, label = content, color = filtered_by_product$is_duplicated)%>%
      addMarkers(holder_coordinates$Long, holder_coordinates$Lat,
                 icon = list(
                   iconUrl = 'marker.png',
                   iconSize = c(25, 25)))
  })
  
  output$statstable <- function() {
    filtered_by_product <- values_react$filtered_db %>% filter(codes_descrip == input$choose_product) %>%
      #mutate() adds new variables and preserves existing ones
      mutate(percent_value = TradeValue / sum(TradeValue) * 100, 
             percent_quant = TradeQuantity / sum(TradeQuantity) * 100)
    #added to modify the number of relationships in the table
    filtered_by_product <- head(arrange(filtered_by_product, desc(TradeValue)), n = input$fraction)
    stats_table <- gen_stats_table(filtered_by_product) %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
}


shinyApp(ui, server)
