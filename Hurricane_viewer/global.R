library(shiny)
library(dplyr)
library(stringr)
library(htmltools)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(ggthemes)

hu_df = read.csv('./hurricane_data.csv', stringsAsFactors = FALSE)

hu_df = hu_df %>% mutate(color = case_when(intensity == 'HU' ~ 'red',
                                           intensity == 'TS' ~ 'orange',
                                           intensity == 'TD' ~ 'yellow',
                                           TRUE ~ 'gray'))

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

max_intensity = function(intensity, max_wind) {
  if ('HU' %in% intensity) {
    storm_max_wind = max(max_wind)
    ret_val = case_when(storm_max_wind >= 137 ~ 'Cat5',
             (storm_max_wind >= 113 & storm_max_wind < 137) ~ 'Cat4',
             (storm_max_wind >=  96 & storm_max_wind < 113) ~ 'Cat3',
             (storm_max_wind >=  83 & storm_max_wind <  96) ~ 'Cat2',
             (storm_max_wind >=  64 & storm_max_wind <  83) ~ 'Cat1')
  } else if ('TS' %in% intensity) ret_val = 'TS'
  else ret_val = 'TD'
  return(ret_val)
}

temp = hu_df %>% 
  group_by(year, storm_num) %>%
  summarise(max_intensity = max_intensity(intensity, max_wind))

temp_since_1967 = hu_df %>% 
  filter(year > 1967) %>%
  group_by(year, storm_num) %>%
  summarise(max_intensity = max_intensity(intensity, max_wind))
