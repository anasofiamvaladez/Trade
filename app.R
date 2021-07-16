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
source("../utils.R")
library(dplyr)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Trade flow of products in the Supply Chain"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            selectInput("phase", "Step of the Supply Chain",
                        choices = c("Back end" = "Back end", 
                                    "Front end" = "Front end")),
            box(uiOutput("products")),
            box(leafletOutput("map"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    values_react <- reactiveValues()
    trade_db <- read_csv("../db_trade_f.csv") %>% arrange_db()
    countries_geo <- get_geo_nodes(trade_db)
    
    observe({
        values_react$phase <- input$phase
        values_react$filtered_db <- trade_db %>% filter(PHASE == values_react$phase)
        values_react$lst_uprod <- unique(values_react$filtered_db[, "cmdCode"])
    })

    
    output$products <- renderUI({
        selectInput(inputId="choose_product",
                    label="Select product",
                    choices = values_react$lst_uprod)
    })
    
    output$map <- renderLeaflet({
        filtered_by_product <- values_react$filtered_db %>% filter(cmdCode == input$choose_product) %>%
            mutate(percent_value = TradeValue / sum(TradeValue) * 100, 
                   percent_quant = TradeQuantity / sum(TradeQuantity) * 100)
        nodes_import <- graph.data.frame(filtered_by_product, directed=TRUE, countries_geo)
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
            edges_import[[i]] <- spChFIDs(edges_import[[i]], as.character(i))
        }
        edges_import <- do.call(rbind, edges_import)
        
        leaflet(vert_import) %>%
            addTiles() %>%
            addCircles(data = vert_import, radius = 1000, weight = 10, color = "navy", label = vert_import$name )%>%
            addPolylines(data = edges_import, weight = filtered_by_product$percent_value, label = filtered_by_product$percent_value)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
