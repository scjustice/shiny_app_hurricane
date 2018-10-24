shinyServer(function(input, output, session){
  output$year <- renderPrint({ input$inputYear })
  
  output$my_map <- renderLeaflet({
    map = leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(-70, 30, zoom = 5)
  })
  
  rv = reactiveValues()
  rv$group_list = list()
  
  session$onSessionEnded(stopApp)
  
  observeEvent(input$animate, {
    output$current_time = renderPrint({input$animate})
  })
  
  observeEvent(input$inputYear,{
    year_df = hu_df %>%
      filter(year == input$inputYear)
    proxy = leafletProxy('my_map')
    
    # Remove all shapes previously drawn on the proxy map
    clearShapes(proxy)
    
    # Reset list of groups
    rv$group_list = list()
    #rv$date_list = unique(hu_df[year == input$inputYear, 'time'])
    rv$cur_date = 1
    
    
    for (i in unique(year_df$storm_num)){
      groupId = paste0(input$inputYear,':',i)
      rv$group_list = append(isolate(rv$group_list),groupId)
      temp_df = year_df %>% filter(storm_num == i) 

      proxy %>% addPolylines(data=temp_df,lng = ~longitude, lat = ~latitude, 
                             group = groupId) %>%
      addCircles(data = temp_df, ~longitude, ~latitude, group = groupId, 
                 color = ~color, popup = ~popup_string, radius = 11000)
    }
    #output$group_list = renderPrint({isolate(rv$group_list)})
  })
})