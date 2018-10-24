library(stringr)
library(lubridate)
library(readr)
library(dplyr)

h_lines = read_lines('./hurdat2-1851-2017-050118.txt')

hu_df = data.frame(name = character(),
                   storm_num = numeric(),
                   year = numeric(),
                   time = double(),
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
      intensity_string = case_when(intensity == 'HU' ~ 'Hurricane', 
                                   intensity == 'TS' ~ 'Tropical Storm', 
                                   intensity == 'TD' ~ 'Tropical Depression')
        
      lat_test = substr(match_string[3],nchar(match_string[3]),nchar(match_string[3]))
      lat = substr(match_string[3],1,nchar(match_string[3])-1)
      latitude = ifelse(lat_test == 'N', as.numeric(lat), as.numeric(lat)*-1)
      
      long_test = substr(match_string[4],nchar(match_string[4]),nchar(match_string[4]))
      long = substr(match_string[4],1,nchar(match_string[4])-1)
      longitude = ifelse(long_test == 'E', as.numeric(long), as.numeric(long)*-1)
      
      max_wind = ifelse(match_string[5] == -99, NaN, as.numeric(match_string[5]))
      min_pressure = ifelse(match_string[6] == '-999', NaN, as.numeric(match_string[6]))
      
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
                           time = strptime(time, '%m/%d/%Y %H:%M', tz='UTC'),
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
      
      hu_df = rbind(hu_df, temp_df)
    }
    i = j+1
    if (i > count) {
      print(str_interp('Percentage parsed: ${as.integer(i/length(h_lines)*100)}%'))
      count = count + length(h_lines)/10
    }
  }
}
hu_df_filtered = hu_df %>% filter(intensity %in% c('TS','TD','HU'))
write.csv(hu_df_filtered,file='./hurricane_data.csv',row.names = FALSE)
