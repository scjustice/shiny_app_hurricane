library(shiny)
library(dplyr)
library(stringr)

hu_df = read.csv('./hurricane_data.csv', stringsAsFactors = FALSE)

temp = hu_df %>% filter(year==1851 & storm_num %in% c(1,2))
year_list = unique(hu_df[,'year'])

set_previous_layer_list = function(x) {
  print('In set_previous_layer_list')
  print(layer_list)
  previous_layer_list = x
} 
  

get_previous_layer_list = function(){
  print('In get_previous_layer_list')
  print(previous_layer_list)
  return(previous_layer_list)
} 