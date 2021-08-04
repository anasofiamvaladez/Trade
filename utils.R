
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
  return(final_db)
}

add_description <- function(data_base) {
  final_db <- data_base %>% 
    unite(codes_descrip, cmdCode, cmdDescE, sep = " ", remove = FALSE)
  return(final_db)
}

gen_top_ten <- function(data_base, product) {
  
    top <- data_base %>%
    filter(codes_descrip == product) %>%
    mutate(percent_value = TradeValue / sum(TradeValue) * 100,
           percent_quant = TradeQuantity / sum(TradeQuantity) * 100) %>%
    select(From, percent_value) %>% group_by(From) %>%
    summarise_at(vars(percent_value), list(percent_sum=sum)) %>%
    arrange(-percent_sum)
    
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
  
  top_ten <- gen_top_ten(data_base, product)
  
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.border = element_blank(), panel.grid=element_blank(),
      axis.ticks = element_blank(), plot.title=element_text(size=14, face="bold")
    )
  
  pie <- ggplot(top_ten, aes(x = "", y=percent_sum, fill = Country)) + 
    geom_col() +
    geom_text(aes(label = paste0(round(percent_sum, 2), "%"), x=1.2), 
              position=position_stack(vjust=0.5)) + 
    blank_theme + 
    theme(axis.text.x=element_blank(), 
          plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5)) +
    labs(fill="Country", 
         x=NULL, 
         y=NULL, 
         title="Share of exports",
         subtitle="among 10 top countries",
         caption="Source: UN COMTRADE")
  
  pie + 
    coord_polar(theta = "y") + 
    scale_fill_brewer(palette="Spectral")
  
}

gen_country_info <- function(database, country, product) {
  country <- database %>% filter(From == country) %>%
    filter(codes_descrip == product) %>%
    mutate(percent_country = round((TradeValue / sum(TradeValue) * 100), 3)) %>%
    select(To, percent_country) %>%
    dplyr::rename(Country = To) %>%
    arrange(desc(percent_country))
  return(country)  
}




