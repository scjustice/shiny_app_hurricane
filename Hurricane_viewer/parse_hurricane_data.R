library(stringr)
library(lubridate)
library(readr)
library(dplyr)

h_lines = read_lines('./hurdat2-1851-2017-050118.txt')

parse_hu_df = data.frame(name = character(),
                   storm_num = numeric(),
                   year = numeric(),
                   date_time = double(),
                   intensity = character(),
                   intensity_string = character(),
                   longitude = numeric(),
                   latitude = numeric(),
                   max_wind = numeric(),
                   min_pressure = numeric(),
                   max_radii_34kt_ne = numeric(),
                   max_radii_34kt_se = numeric(),
                   max_radii_34kt_sw = numeric(),
                   max_radii_34kt_nw = numeric(),
                   max_radii_50kt_ne = numeric(),
                   max_radii_50kt_se = numeric(),
                   max_radii_50kt_sw = numeric(),
                   max_radii_50kt_nw = numeric(),
                   max_radii_64kt_ne = numeric(),
                   max_radii_64kt_se = numeric(),
                   max_radii_64kt_sw = numeric(),
                   max_radii_64kt_nw = numeric(),
                   stringsAsFactors=FALSE)

i = 1
count = 1
while (i <= length(h_lines)) {
  curline = h_lines[i]
  if (grepl('^AL', curline)){
    match_string = str_match(curline, '^AL(\\d{2})\\d{4},\\s+(\\S+),\\s+(\\d+)')
    storm_num = match_string[2]
    name = paste0(substring(match_string[3], 1,1), tolower(substring(match_string[3],2)))
    num_rows = as.numeric(match_string[4])
    
    for (j in (i+1):(i+num_rows)) {
      curline = h_lines[j]
      # match date info
      match_string = str_match(curline, '^(\\d{4})(\\d{2})(\\d{2}), (\\d{2})(\\d{2}),')
      year = match_string[2]
      month = match_string[3]
      day = match_string[4]
      hour = match_string[5]
      minute = match_string[6]
      time = str_interp('${month}/${day}/${year} ${hour}:${minute}')
      # match intensity, longitude, latitude, max wind, and min pressure
      match_string = str_match(curline, ', (\\w{2}),\\s+(\\S+),\\s+(\\S+),\\s+(-?\\d+),\\s+(-?\\d+),')
                                 
      intensity = match_string[2]
        
      lat_test = substr(match_string[3],nchar(match_string[3]),nchar(match_string[3]))
      lat = substr(match_string[3],1,nchar(match_string[3])-1)
      latitude = ifelse(lat_test == 'N', as.numeric(lat), as.numeric(lat)*-1)
      
      long_test = substr(match_string[4],nchar(match_string[4]),nchar(match_string[4]))
      long = substr(match_string[4],1,nchar(match_string[4])-1)
      longitude = ifelse(long_test == 'E', as.numeric(long), as.numeric(long)*-1)
      
      max_wind = ifelse(match_string[5] == -99, NaN, as.numeric(match_string[5]))
      min_pressure = ifelse(match_string[6] == '-999', NaN, as.numeric(match_string[6]))
      
      intensity_string = case_when(intensity == 'HU' ~ paste0('Hurricane: ',
                                      case_when(max_wind >= 137 ~ 'Cat5',
                                               (max_wind >= 113 & max_wind < 137) ~ 'Cat4',
                                               (max_wind >=  96 & max_wind < 113) ~ 'Cat3',
                                               (max_wind >=  83 & max_wind <  96) ~ 'Cat2',
                                               (max_wind >=  64 & max_wind <  83) ~ 'Cat1')), 
                                   intensity == 'TS' ~ 'Tropical Storm', 
                                   intensity == 'TD' ~ 'Tropical Depression',
                                   TRUE ~ paste0('Other:', intensity))
      # match the max radii values
      match_string = str_match(curline, ',\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s*$')
      
      max_radii_34kt_ne = ifelse(match_string[2] == '-999', NaN, as.numeric(match_string[2]))
      max_radii_34kt_se = ifelse(match_string[3] == '-999', NaN, as.numeric(match_string[3]))
      max_radii_34kt_sw = ifelse(match_string[4] == '-999', NaN, as.numeric(match_string[4]))
      max_radii_34kt_nw = ifelse(match_string[5] == '-999', NaN, as.numeric(match_string[5]))
      
      max_radii_50kt_ne = ifelse(match_string[6] == '-999', NaN, as.numeric(match_string[6]))
      max_radii_50kt_se = ifelse(match_string[7] == '-999', NaN, as.numeric(match_string[7]))
      max_radii_50kt_sw = ifelse(match_string[8] == '-999', NaN, as.numeric(match_string[8]))
      max_radii_50kt_nw = ifelse(match_string[9] == '-999', NaN, as.numeric(match_string[9]))
      
      max_radii_64kt_ne = ifelse(match_string[10] == '-999', NaN, as.numeric(match_string[10]))
      max_radii_64kt_se = ifelse(match_string[11] == '-999', NaN, as.numeric(match_string[11]))
      max_radii_64kt_sw = ifelse(match_string[12] == '-999', NaN, as.numeric(match_string[12]))
      max_radii_64kt_nw = ifelse(match_string[13] == '-999', NaN, as.numeric(match_string[13]))
      
      temp_df = data.frame(name = name,
                           year = as.numeric(year),
                           storm_num = as.numeric(storm_num),
                           date_time = strptime(time, '%m/%d/%Y %H:%M', tz='UTC'),
                           intensity = intensity,
                           intensity_string = intensity_string,
                           longitude = longitude,
                           latitude = latitude,
                           max_wind = max_wind,
                           min_pressure = min_pressure,
                           max_radii_34kt_ne = max_radii_34kt_ne,
                           max_radii_34kt_se = max_radii_34kt_se,
                           max_radii_34kt_sw = max_radii_34kt_sw,
                           max_radii_34kt_nw = max_radii_34kt_nw,
                           max_radii_50kt_ne = max_radii_50kt_ne,
                           max_radii_50kt_se = max_radii_50kt_se,
                           max_radii_50kt_sw = max_radii_50kt_sw,
                           max_radii_50kt_nw = max_radii_50kt_nw,
                           max_radii_64kt_ne = max_radii_64kt_ne,
                           max_radii_64kt_se = max_radii_64kt_se,
                           max_radii_64kt_sw = max_radii_64kt_sw,
                           max_radii_64kt_nw = max_radii_64kt_nw,
                           stringsAsFactors = FALSE)
      
      parse_hu_df = rbind(parse_hu_df, temp_df)
    }
    i = j+1
    if (i > count) {
      print(str_interp('Percentage parsed: ${as.integer(i/length(h_lines)*100)}%'))
      count = count + length(h_lines)/10
    }
  }
}

get_max_intensity = function(intensity, max_wind) {
  if ('HU' %in% intensity) {
    storm_max_wind = max(max_wind)
    ret_val = case_when(storm_max_wind >= 137 ~ 'Hurricane: Cat5',
                        (storm_max_wind >= 113 & storm_max_wind < 137) ~ 'Hurricane: Cat4',
                        (storm_max_wind >=  96 & storm_max_wind < 113) ~ 'Hurricane: Cat3',
                        (storm_max_wind >=  83 & storm_max_wind <  96) ~ 'Hurricane: Cat2',
                        (storm_max_wind >=  64 & storm_max_wind <  83) ~ 'Hurricane: Cat1')
  } else if ('TS' %in% intensity) ret_val = 'Tropical Storm'
  else ret_val = 'Tropical Depression'
  return(ret_val)
}

parse_hu_df = parse_hu_df %>% 
              group_by(year, storm_num) %>%
              mutate(max_intensity = get_max_intensity(intensity, max_wind))
              

#hu_df_filtered = hu_df %>% filter(intensity %in% c('TS','TD','HU'))
write.csv(parse_hu_df,file='data/hurricane_data.csv',row.names = FALSE)

# Difference between times
intensity_list = c('Hurricane: Cat5',
                   'Hurricane: Cat4',
                   'Hurricane: Cat3',
                   'Hurricane: Cat2', 
                   'Hurricane: Cat1',
                   'Tropical Storm',
                   'Tropical Depression')

#parse_hu_df = read.csv('./data/hurricane_data.csv', stringsAsFactors = FALSE)

hu_prop_df = data.frame()

for (cur_year in 1851:2017) {
  temp_df = parse_hu_df %>% 
    filter(year == cur_year & intensity_string %in% intensity_list) %>% 
    select(year, storm_num, name, intensity_string, date_time)
  print(paste0('Cur_year = ', cur_year))
  for (cur_storm in unique(temp_df$storm_num)){
    temp_dict = list('Hurricane: Cat5' = 0, 'Hurricane: Cat4' = 0, 'Hurricane: Cat3' = 0,
                     'Hurricane: Cat2' = 0, 'Hurricane: Cat1' = 0, 'Tropical Storm' = 0,
                     'Tropical Depression' = 0)
    temp_matrix = temp_df %>% filter(storm_num == cur_storm) %>% as.matrix.data.frame()
    name = temp_matrix[1,'name']
    cur_intensity = temp_matrix[1,'intensity_string']
    start_time = temp_matrix[1,'date_time']
    if (nrow(temp_matrix) == 1){
      temp_dict[cur_intensity] = temp_dict[[cur_intensity]] + 6
    } else{
      for ( i in 2:nrow(temp_matrix)) {
        if (temp_matrix[i, 'intensity_string'] != cur_intensity) {
          temp_dict[[cur_intensity]] = temp_dict[[cur_intensity]] + 
            as.numeric(difftime(temp_matrix[i,'date_time'],start_time), unit= 'hours')
          cur_intensity = temp_matrix[i,'intensity_string']
          start_time = temp_matrix[i,'date_time']
        } else if (i == nrow(temp_matrix)) {
          temp_dict[[cur_intensity]] = temp_dict[[cur_intensity]] + 
            as.numeric(difftime(temp_matrix[i,'date_time'], start_time), unit= 'hours')
        }
      }
    }
    hu_prop_df = rbind(hu_prop_df, data.frame(year=cur_year, storm_num = cur_storm, 
                                              name = name, temp_dict))
  }
}

hu_prop_df = hu_prop_df %>% select(year, storm_num, name,
                               cat5_hours = Hurricane..Cat5,
                               cat4_hours = Hurricane..Cat4, 
                               cat3_hours = Hurricane..Cat3, 
                               cat2_hours = Hurricane..Cat2, 
                               cat1_hours = Hurricane..Cat1, 
                               ts_hours = Tropical.Storm, 
                               td_hours = Tropical.Depression)

write.csv(hu_prop_df,file='data/hurricane_proportion_data.csv',row.names = FALSE)
