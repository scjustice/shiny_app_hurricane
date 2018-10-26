shinyServer(function(input, output, session){
  
  #output$current_time = renderPrint(({input$animate}))
  
  output$my_map <- renderLeaflet({
    map = leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(-70, 30, zoom = 5)
  })
  
  rv = reactiveValues(cur_df = NULL, group_list = NULL, date_list = NULL)
  #rv$group_list = list()
  #rv$cur_df = data_frame()
  
  session$onSessionEnded(stopApp)
  
  observeEvent(input$animate, 
  {
    output$current_time = renderPrint(({input$tabs}))
    if(nchar(input$inputYear) != 0) {
      cur_df = rv$cur_df
      print(paste0('Cur_df is dataframe of size ', paste0(dim(cur_df),collapse = ':')))
      rv$date_list = unique(rv$cur_df[,'time'])
      #output$current_time = renderPrint({rv$date_list[input$animate]})
      #output$current_time = renderPrint({input$tabs})
    } else {
      print('input Year not set yet')
    }
  })
  
  observeEvent(input$inputYear,{
    output$year <- renderPrint({ input$inputYear })
    if (nchar(input$inputYear) != 0){
      year_df = hu_df %>%
        filter(year == input$inputYear)
      rv$cur_df = year_df
      print(paste0('Setting cur_df to dataframe of size ', paste0(dim(year_df),collapse = ':')))
    } else return()
    
    if (input$tabs == 'Data'){
      cur_df = rv$cur_df 
      print('tab is Data')
      print(paste0('Setting cur_df to dataframe of size ', paste0(dim(cur_df),collapse = ':')))
      name_list = cur_df %>%
        filter(!is.na(max_wind)) %>%
        select(name, storm_num) %>%
        arrange(storm_num) %>% distinct() %>%
        mutate(new_name = if_else(name == 'Unnamed', paste0(name,':',storm_num),name)) %>% 
        select(new_name) %>%
        as.list()
      temp_df = cur_df %>% 
        filter(!is.na(max_wind)) %>%
        mutate(new_name = if_else(name == 'Unnamed', paste0(name,':',storm_num),name)) %>% 
        group_by(new_name) %>% 
        summarise(storm_max_wind = max(max_wind),
                  max_intensity = max_intensity(intensity, max_wind))
      
      temp_df$new_name = factor(temp_df$new_name,
                                levels = name_list$new_name)
      temp_df$max_intensity = factor(temp_df$max_intensity,
                                     levels = c('TS','TD','Cat1','Cat2','Cat3','Cat4','Cat5'))
      output$plot = renderPlot(
        ggplot(data = temp_df) + geom_bar(aes(x=new_name, y=storm_max_wind, 
                                fill=max_intensity), stat = 'identity') +
        theme_economist() + theme(axis.text.x = element_text(angle = 90,size = rel(1.3), vjust = 0.5)) +
        labs(x = 'Storm Name', y='Max Wind Speed (knots)') +
        theme(axis.title = element_text(size = rel(1.5), margin = 20))
                                  
          
      )
    } else {
      proxy = leafletProxy('my_map')
      
      # Remove all shapes previously drawn on the proxy map
      clearShapes(proxy)
      
      # Reset list of groups
      rv$group_list = list()
      #rv$date_list = unique(hu_df[year == input$inputYear, 'time'])
      rv$cur_date = 1
      
      
      for (i in unique(year_df$storm_num)){
        groupId = paste0(input$inputYear,':',i)
        rv$group_list = append(rv$group_list,groupId)
        temp_df = year_df %>% filter(storm_num == i) 
  
        proxy %>% addPolylines(data=temp_df,lng = ~longitude, lat = ~latitude, 
                               group = groupId) %>%
        addCircles(data = temp_df, ~longitude, ~latitude, group = groupId, 
                   color = ~color, popup = ~popup_string, radius = 11000,
                   fillOpacity= 0.6)
      }
      #output$group_list = renderPrint({isolate(rv$group_list)})
    }
    
    
  })
})