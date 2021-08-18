

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
    tabItem(tabName = 'all_statistics',
            h1(paste0("Supply Chain of the Semiconductors Industry"), align ="center", 
               style = "font-family: 'Arial'; font-si16pt"),
            h2(paste0("Data overview"), align="center", 
               style = "font-family: 'Arial'; font-si16pt"),
            h3(paste0("Semiconductors Supply Chain by Phase"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              valueBoxOutput('be_box'), 
              valueBoxOutput('fe_box')
            ),
            h3(paste0("10 most exported products of the Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("table_all_products"), width = NULL))
            ),
            h3(paste0("Top 10 exporting countries: Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("table_all_countries_ex"), width = NULL))
            ),
            h3(paste0("Top 10 importing countries: Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("table_all_countries_im"), width = NULL))
            ),
            h3(paste0("10 most exported products of the Back End Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("be_products"), width = NULL))
            ),
            h3(paste0("Top 10 exporting countries: Back End Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("be_countries_ex"), width = NULL))
            ),
            h3(paste0("Top 10 importing countries: Back End Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("be_countries_im"), width = NULL))
            ),
            h3(paste0("10 most exported products of the Front End Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("fe_products"), width = NULL))
            ),
            h3(paste0("Top 10 exporting countries: Front End Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("fe_countries_ex"), width = NULL))
            ),
            h3(paste0("Top 10 importing countries: Front End Semiconductors Supply Industry"), align = "justify", 
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              column(width = 12, box(tableOutput("fe_countries_im"), width = NULL))
            )
    ),
    tabItem(tabName = 'semiconductors',
            h1(paste0("Supply Chain: World Info on Semiconductors Industry"), align="center", 
               style = "font-family: 'Arial'; font-si16pt"),
            h2(paste0("Select the phase, products, and number of partners"), align = "justify",
               style = "font-family: 'Arial'; font-si16pt"),
            fluidRow(
              #Create a select list input control, "phase" debe ser parte del input de server
              column(width = 4, selectInput("phase", "Phase of the Supply Chain",
                                            choices = c("Back end" = "Back end", 
                                                        "Front end" = "Front end"), width = NULL)),
              column(width = 4, uiOutput("broad")),
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
              h2(paste0("Map: Export flows"), align = 'justify', 
                 style = "font-family: 'Arial'; font-si16pt"),
              column(width = 12, box(leafletOutput("map"), width = NULL)),
              h4(paste0("Green: only one of the two countries in the relation is a exporter. Red: Both countries export
                        to each other.")),
              h2(paste0("Trade Information Table"), align = 'justify', 
                 style = "font-family: 'Arial'; font-si16pt"),
              column(width = 12, box(tableOutput("statstable"), width = NULL)),
              h2(textOutput("product_text"), align = 'justify', 
                 style = "font-family: 'Arial'; font-si16pt"),
              column(width = 6),
              column(width = 6, uiOutput("topten_country")),
              h4(textOutput("info_text")),
              h2(paste0("10 Main exporters & Mexico"), align = 'justify', 
                 style = "font-family: 'Arial'; font-si16pt"),
              h4(paste0("Mexico may be part of the top 10 or just be there for comparison purposes"),
                 align = 'justify', style = "font-family: 'Arial'; font-si14pt"),
              column(width = 6, box(plotOutput("visual"), width = NULL)),
              column(width = 6, box(tableOutput("country_info"), width = NULL)),
              column(width = 6),
              column(width = 6, uiOutput("topten_country_imp")),
              h2(paste0("10 Main importers & Mexico"), align = 'justify', 
                 style = "font-family: 'Arial'; font-si16pt"),
              h4(paste0("Mexico may be part of the top 10 or just be there for comparison purposes"),
                 align = 'justify', style = "font-family: 'Arial'; font-si14pt"),
              column(width = 6, box(plotOutput("visual2"), width = NULL)),
              column(width = 6, box(tableOutput("country_info_imp"), width = NULL)),
              h2(paste0("10 more specialized countries & Mexico"), align = 'justify', 
                 style = "font-family: 'Arial'; font-si16pt"),
              column(width = 6, box(plotOutput("visual_bars"), width = NULL))
            ))
  ))

ui <- dashboardPage(header, siderbar, body)



server <- function(input, output) {
  #General industry information
  trade_db <- read_csv("db_trade_f.csv") %>% arrange_db()
  trade_db <- add_description(trade_db)
  
  #create two databases: back and front end
  back_end <- trade_db %>% filter(PHASE == "Back end")
  front_end <- trade_db %>% filter(PHASE == "Front end")
  
  #create boxes to show the value of the phase of the production 
  output$fe_box <- renderValueBox({
    valueBox(
      VB_style( paste0( '$',format(sum(front_end$TradeValue)/1000000,big.mark=','), " m" ),  "font-size: 60%;"  ),
      VB_style( paste0("Front End Phase (", round(sum(front_end$TradeValue)/sum(trade_db$TradeValue)*100,1) ,"%)")  ), 
      icon = icon('front', lib = 'glyphicon'), #icon("sign-in"),
      color = "navy",
      width = 6
    )
  })
  
  output$be_box <- renderValueBox({
    valueBox(
      VB_style( paste0( '$',format(sum(back_end$TradeValue)/1000000,big.mark=','), " m" ),  "font-size: 60%;"  ),
      VB_style( paste0("Back End Phase (", round(sum(back_end$TradeValue)/sum(trade_db$TradeValue)*100,1) ,"%)")  ), 
      icon = icon('back', lib = 'glyphicon'), #icon("sign-in"),
      color = "navy", 
      width = 6
    )
  })
  
  output$table_all_products <- function() {
    summarize_by(trade_db, quo(codes_descrip), "TradeValue")
  }
  
  output$table_all_countries_ex <- function() {
    summarize_by(trade_db, quo(From), "TradeValue")
  }
  
  output$table_all_countries_im <- function() {
    summarize_by(trade_db, quo(To), "TradeValue")
  }
  
  output$be_products <- function() {
    back_end_pro <- trade_db %>% filter(PHASE == "Back end")
    summarize_by(back_end_pro, quo(codes_descrip), "TradeValue")
  }
  
  output$be_countries_ex <- function() {
    back_end_pro <- trade_db %>% filter(PHASE == "Back end")
    summarize_by(back_end_pro, quo(From), "TradeValue")
  }
  
  output$be_countries_im <- function() {
    back_end_pro <- trade_db %>% filter(PHASE == "Back end")
    summarize_by(back_end_pro, quo(To), "TradeValue")
  }
  
  output$fe_products <- function() {
    front_end_pro <- trade_db %>% filter(PHASE == "Front end")
    summarize_by(front_end_pro, quo(codes_descrip), "TradeValue")
  }
  
  output$fe_countries_ex <- function() {
    front_end_pro <- trade_db %>% filter(PHASE == "Front end")
    summarize_by(front_end_pro, quo(From), "TradeValue")
  }
  
  output$fe_countries_im <- function() {
    front_end_pro <- trade_db %>% filter(PHASE == "Front end")
    summarize_by(front_end_pro, quo(To), "TradeValue")
  }
  
  #semiconductors section
  values_react <- reactiveValues()
  
  #Cambiar nombres de los paises y ordenar database
  
  
  #Create a reactive observer
  observe({
    values_react$phase <- input$phase
    values_react$fraction <- input$fraction
    values_react$filtered_db <- trade_db %>% filter(PHASE == values_react$phase)
    #values_react$lst_uprod <- unique(values_react$filtered_db[, "codes_descrip"])
    values_react$lst_broad <- unique(values_react$filtered_db[, "naics_description"])
    values_react$broad <- input$broad
    values_react$selected_product <- input$choose_product
  })

  observeEvent(input$broad, {
    options_filtered = values_react$filtered_db %>% filter(naics_description == values_react$broad)
    values_react$lst_uprod = unique(options_filtered[, "codes_descrip"])
  })

  
  observeEvent(input$choose_product, {
    values_react$top_ten <- gen_top_ten(values_react$filtered_db, values_react$selected_product, From) %>%
      select(Country)
    values_react$top_ten_imp <- gen_top_ten(values_react$filtered_db, values_react$selected_product, To) %>%
      select(Country)
  })
  
  output$topten_country <- renderUI({
    selectInput(inputId="topten_country",
                #titulo del botón
                label="Select country",
                choices = values_react$top_ten, width = NULL)
  })
  
  #lo mismo para importaciones
  
  output$topten_country_imp <- renderUI({
    selectInput(inputId="topten_country_imp",
                #titulo del botón
                label="Select country",
                choices = values_react$top_ten_imp, width = NULL)
  })
  
  output$broad <- renderUI({
    selectInput(inputId="broad",
                #titulo del botón
                label="Broad Product",
                choices = values_react$lst_broad, width = NULL)
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
      addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(noWrap = TRUE))%>%
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
      mutate(percent_value = round(TradeValue / sum(TradeValue) * 100, 2), 
             percent_quant = TradeQuantity / sum(TradeQuantity) * 100)
    #added to modify the number of relationships in the table
    filtered_by_product <- head(arrange(filtered_by_product, desc(TradeValue)), n = input$fraction)
    stats_table <- gen_stats_table(filtered_by_product) %>%
      knitr::kable("html") %>% 
      kable_styling("striped", full_width = F)
  }
  
  output$visual <- renderPlot({
    gen_graph(values_react$filtered_db, input$choose_product)
  })
  
  output$visual2 <- renderPlot({
    gen_graph_imp(values_react$filtered_db, input$choose_product)
  })
  
  output$visual_bars <- renderPlot({
  gen_graph_bars(values_react$filtered_db, input$choose_product)
  })
  
  output$country_info <- function() {
    country_table <- gen_country_info(values_react$filtered_db, 
                                      input$topten_country, input$choose_product, 'exporter')
    country_table %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>% 
      kable_paper() %>%
      scroll_box(width = "500px", height = "400px")
  }
  
  output$country_info_imp <- function() {
    country_table_imp <- gen_country_info(values_react$filtered_db, 
                                          input$topten_country_imp, input$choose_product, 'importer')
    country_table_imp %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>% 
      kable_paper() %>%
      scroll_box(width = "500px", height = "400px")
  }
  
  output$product_text <- renderText({
    paste0("Top ten export and import countries of product: ", input$choose_product)
  })
  
  output$info_text <- renderText({
    country_table <- gen_country_info(values_react$filtered_db, 
                                      input$topten_country, input$choose_product)
    filtered_by_product <- values_react$filtered_db %>% filter(codes_descrip == input$choose_product)
    paste0(input$topten_country, " exports to ", nrow(country_table), 
           " countries. The total value of the exports is USD$", format(sum(country_table$TradeValue), big.mark = ','), 
           " which represents ", round(sum(country_table$TradeValue)/sum(filtered_by_product$TradeValue)*100, 3), 
           "% of the total word exports of product ", input$choose_product, ".")
  })
}


shinyApp(ui, server)
