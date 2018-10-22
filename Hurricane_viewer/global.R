library(shiny)
library(dplyr)
library(stringr)

hu_df = read.csv('./hurricane_data.csv', stringsAsFactors = FALSE)
previous_layer_list = list()

temp = hu_df %>% filter(year==1851 & storm_num %in% c(1,2))
year_list = unique(hu_df[,'year'])