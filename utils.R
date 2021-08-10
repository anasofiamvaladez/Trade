


#You have loaded plyr after dplyr - this is likely to cause problems.
#If you need functions from both plyr and dplyr, please load plyr first, then dplyr:

library(tidyverse)
library(plyr)
library(dplyr)
library(igraph)
library(maps)
library(geosphere)
library(comtradr)
library(leaflet)
library(maptools)
library(sp)
library(png)
library(treemap)
library(ggplot2)
library(treemapify)

official_names <- c("Bosnia Herzegovina", "Br. Virgin Isds",  "Cabo Verde", 
                    "Central African Rep.", "Christmas Isds", "Dem. People's Rep. of Korea", 
                    "Dominican Rep.", "Holy See (Vatican City State)", "Lao People's Dem. Rep.", 
                    "Norfolk Isds", "Rep. of Korea", "Russian Federation", "Solomon Isds", 
                    "Turks and Caicos Isds", "Viet Nam", "Bolivia (Plurinational State of)", 
                    "USA", "United Rep. of Tanzania", "Rep. of Moldova", "Wallis and Futuna Isds", 
                    "Cocos Isds", "Czechia", "Fmr Sudan", "Cayman Isds", "Br. Indian Ocean Terr.", 
                    "Falkland Isds (Malvinas)", "China, Hong Kong SAR", "China, Macao SAR", 
                    "Brunei Darussalam", "Cook Isds", "Marshall Isds", "FS Micronesia", 
                    "Fr. South Antarctic Terr.", "United States Minor Outlying Islands", 
                    "Saint BarthÃ©lemy", "CuraÃ§ao", "CÃ´te d'Ivoire")

names <- c("Bosnia and Herzegovina", "British Virgin Islands",  "Cape Verde", 
           "Central African Republic", "Christmas Island", "North Korea", 
           "Dominican Republic", "Vatican City", "Laos", "Norfolk Island", 
           "South Korea", "Russia", "Solomon Islands", "Turks and Caicos Islands", 
           "Vietnam", "Bolivia", "United States", "Tanzania", "Moldova", 
           "Wallis and Futuna", "Cocos Islands", "Czech Republic", "Sudan", 
           "Cayman Islands", "British Indian Ocean Territory", "Falkland Islands [Islas Malvinas]",
           "Hong Kong", "Macao", "Brunei", "Cook Islands", "Marshall Islands", "Micronesia",
           "French Southern Territories", "United States", "Saint Barthélemy",
           "Curaçao", "Côte d'Ivoire")



COUNTRY_EQ <- data.frame(official_names, names, stringsAsFactors=FALSE)
COORDINATES_C <- read_csv("lat_lon_country.csv")



#function that changes the names of countries from official to usual, filters
#for world, computes the share of the us value/and volume by country, and
#arranges the order of the colums


arrange_db <- function(data, country_names=COUNTRY_EQ) {
  for (i in 1:nrow(country_names)) {
    data$From[data$From == country_names$official_names[i]] <- country_names$names[i]
    data$To[data$To == country_names$official_names[i]] <- country_names$names[i]
  }
  
  #delete World, create percentages, and arrange the order
  final_db <- data %>%
    dplyr::filter(From != "World") %>%
    dplyr::filter(To != "World") %>%
    relocate(From, .before = pfCode) %>%
    relocate(To, .after = From)
  return(final_db)
} 

#get the list of unique countries that export the product and its location
get_geo_nodes <- function(data, location_file = COORDINATES_C) {
  countries_from <- unique(data[, "From"]) %>%
    #new_name = old_name
    dplyr::rename(country = From)
  countries_to <- unique(data[, "To"]) %>%
    dplyr::rename(country = To)
  countries <- rbind(countries_from, countries_to) %>%
    arrange(country)
  countries <- unique(countries[, "country"])
  
  geo_nodes_countries <- merge(countries, location_file, by.x = "country", 
                               by.y = "name")
}

gen_stats_table <- function(data_base) {
  final_db <- data_base %>% 
    select(From, To, codes_descrip, cmdDescE,Naic_descrip, TradeValue, percent_value, COMPLEXITY) %>% 
    #arrange(desc(percent_value)) %>%
    #slice_head(n=10) %>%s
    dplyr::rename(
      Export_country = From,
      Import_country = To,
      Code_descripton = codes_descrip,
      HS_description = cmdDescE,
      NAIC_description = Naic_descrip,
      US_value = TradeValue,
      US_val_percent = percent_value,
      Complexity_index = COMPLEXITY
    )
  c_change <- c(6)
  final_db[c_change] <- lapply(final_db[c_change], formatC, big.mark= ',', decimal.mark =".", format = "f", digits = 2)
  return(final_db)
}

add_description <- function(data_base) {
  final_db <- data_base %>% 
    unite(codes_descrip, cmdCode, cmdDescE, sep = " ", remove = FALSE)
  return(final_db)
}

gen_top_ten <- function(data_base, product, importer_or_exporter) {
  importer_or_exporter = enquo(importer_or_exporter)
  top <- data_base %>%
    filter(codes_descrip == product) %>%
    mutate(percent_value = TradeValue / sum(TradeValue) * 100,
           percent_quant = TradeQuantity / sum(TradeQuantity) * 100) %>%
    select( !!importer_or_exporter, percent_value) %>% group_by(!!importer_or_exporter) %>%
    summarise_at(vars(percent_value), list(percent_sum=sum)) %>%
    arrange(-percent_sum) %>%
    dplyr::rename(From = !!importer_or_exporter)
  
  top_ten <- top %>%
    slice_head(n=10)
  
  if ("Mexico" %in% top_ten$From) {
    
    top_ten <- top_ten %>%
      mutate(per_cumsum = round(cumsum(percent_sum)), 3) %>%
      dplyr::rename(Country = From)
    
    return(top_ten)
  }
  
  else {
    mexico <- top %>%
      filter(From == "Mexico")
    
    top_ten <- top %>%
      slice_head(n=9)
    
    top_ten <- rbind(mexico, top_ten) %>%
      mutate(per_cumsum = round(cumsum(percent_sum)), 3) %>%
      dplyr::rename(Country = From)
    
    return(top_ten)
    
  }
}


gen_graph <- function(data_base, product) {
  
  top_ten <- gen_top_ten(data_base, product, From)
  top_ten <- top_ten %>% mutate_if(is.numeric, ~round(., 2))
  
  square <- ggplot(top_ten, aes(area = percent_sum, fill = Country, label = paste(Country, percent_sum, sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre", size = 10,
                      grow = TRUE) 
  
  square
}

gen_graph_imp <- function(data_base, product) {
  
  top_ten <- gen_top_ten(data_base, product, To)
  top_ten <- top_ten %>% mutate_if(is.numeric, ~round(., 2))
  
  square <- ggplot(top_ten, aes(area = percent_sum, fill = Country, label = paste(Country, percent_sum, sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre", size = 10,
                      grow = TRUE) 
  
  square
}

gen_country_info <- function(database, country, product, importer_or_exporter) {
  
  if (importer_or_exporter == 'exporter') {
      country <- database %>% filter(From == country) %>%
          filter(codes_descrip == product) %>%
          mutate(percent_country = round((TradeValue / sum(TradeValue) * 100), 3)) %>%
          select(To, TradeValue, percent_country) %>%
          dplyr::rename(Country = To) %>%
          arrange(desc(percent_country))
          return(country)  
    
  } else {
    
      country <- database %>% filter(To == country) %>%
        filter(codes_descrip == product) %>%
        mutate(percent_country = round((TradeValue / sum(TradeValue) * 100), 3)) %>%
        select(From, TradeValue, percent_country) %>%
        dplyr::rename(Country = From) %>%
        arrange(desc(percent_country))
      return(country)
    
  }
}


#function taken from https://github.com/rstudio/shiny-gallery/blob/master/nz-trade-dash/helper_funs.R
VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p( msg , style = style )
}






