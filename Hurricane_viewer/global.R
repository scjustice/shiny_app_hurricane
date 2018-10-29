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

year_list = 1851:2017
year_list_1967 = c('All', 1967:2017)

name_list = (hu_df %>% 
  group_by(year, storm_num)  %>% 
  select(name, year, storm_num) %>% 
  distinct() %>% 
  summarise(new_name = if_else(name == 'Unnamed', 
                               paste0('Unnamed:', storm_num, ' (', year, ')'), 
                               paste0(name, ' (', year,')' ))) %>% 
  ungroup() %>%
  distinct() %>%
  select(new_name))[[1]]

name_list = sort(name_list)

name_list_1967 = (hu_df %>% 
                    filter(year > 1967) %>%
                    group_by(year, storm_num)  %>% 
                    select(name, year, storm_num) %>% 
                    distinct() %>% 
                    summarise(new_name = if_else(name == 'Unnamed', 
                                                 paste0('Unnamed:', storm_num, ' (', year, ')'), 
                                                 paste0(name, ' (', year,')' ))) %>% 
                    ungroup() %>%
                    distinct() %>%
                    select(new_name))[[1]]

name_list_1967 = sort(name_list_1967)

intensity_list = c('Hurricane: Cat5',
                   'Hurricane: Cat4',
                   'Hurricane: Cat3',
                   'Hurricane: Cat2', 
                   'Hurricane: Cat1',
                   'Tropical Storm',
                   'Tropical Depression')

get_popup_string = function(name, storm_num, intensity_string, max_wind, min_pressure){
  if(name == 'Unnamed') name = paste(name,storm_num, sep = ':')
  ret_val = paste0('Name = ', as.character(name),'<br>Intensity = ', intensity_string)
  if(!is.na(max_wind)) ret_val = paste0(ret_val, '<br>Max Wind = ', max_wind)
  if(!is.na(min_pressure)) ret_val = paste0(ret_val, '<br>Min Pressure = ', min_pressure)
  return(ret_val)
}

new_name_helper = function(input_test, name, storm_num, year){
  if (input_test) {
    new_name = if_else(name == 'Unnamed', paste0(name,':', storm_num),name)
  }else {
    if(name == 'Unnamed') new_name = paste0(name,':', storm_num, ' (', year, ')')
    else new_name = paste0 (name, ' (', year,')')
  }
  return(new_name)
}

hu_df['popup_string'] = mapply(get_popup_string, hu_df$name, hu_df$storm_num, 
                               hu_df$intensity_string, hu_df$max_wind, hu_df$min_pressure)


