library(shiny)
library(dplyr)
library(stringr)
library(htmltools)
library(shinydashboard)
library(leaflet)

hu_df = read.csv('./hurricane_data.csv', stringsAsFactors = FALSE)

hu_df = hu_df %>% mutate(color = case_when(intensity == 'HU' ~ 'red',
                                           intensity == 'TS' ~ 'orange',
                                           intensity == 'TD' ~ 'yellow'))

year_list = unique(hu_df[,'year'])

popup_string_setup = function(name, storm_num, intensity_string, max_wind, min_pressure){
  #print(typeof(x))
  if(name == 'Unnamed') name = paste(name,storm_num, sep = ':')
  ret_val = paste0('Name = ', as.character(name),'<br>Intensity = ', intensity_string)
  if(!is.na(max_wind)) ret_val = paste0(ret_val, '<br>Max Wind = ', max_wind)
  if(!is.na(min_pressure)) ret_val = paste0(ret_val, '<br>Min Pressure = ', min_pressure)
  return(ret_val)
}

hu_df['popup_string'] = mapply(popup_string_setup, hu_df$name, hu_df$storm_num, 
                               hu_df$intensity_string, hu_df$max_wind, hu_df$min_pressure)