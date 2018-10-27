shinyServer(function(input, output, session){
  
  #output$current_time = renderPrint(({input$animate}))
  
  output$data_type <- renderPrint({ rv$cur_filter })

  output$my_map <- renderLeaflet({
    map = leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(-70, 30, zoom = 5)
  })
  
  rv = reactiveValues(cur_df = NULL, 
                      group_list = NULL, 
                      date_list = NULL, 
                      cur_filter = 'year',
                      cur_list = year_list,
                      cur_input = 1851
                      )

  
  get_menu_input = reactive({
    validate(
      need(input$menuInput != "", 'Please make selection for analysis')
    )
    return(input$menuInput)
    
  })
  
  session$onSessionEnded(stopApp)
  output$menu_select = renderUI({
    selectizeInput("menuInput", 
                   "Menu Input", choices = c('All',rv$cur_list),
                   multiple = TRUE, options = list(maxItems = 5))
  })
  
  observeEvent(input$radio, {
    rv$cur_filter = case_when(input$radio == 1 ~ 'year',
                              input$radio == 2 ~ 'name',
                              input$radio == 3 ~ 'intensity_string')
    if (input$option_1967) {
      if (input$radio == 1) rv$cur_list = year_list_1967
      else if (input$radio == 2) rv$cur_list = name_list_1967
      else rv$cur_list = intensity_list
    } else {
      if (input$radio == 1) rv$cur_list = year_list
      else if (input$radio == 2) rv$cur_list = name_list
      else rv$cur_list = intensity_list
    }

    if (input$radio != 3){
      print(paste0('length of list is ', length(rv$cur_list)))
      output$menu_select = renderUI({
        selectizeInput("menuInput", 
                       "Menu Input", choices = c('All',rv$cur_list),
                       multiple = TRUE, options = list(maxItems = 5))
      }) 
    } else {
      print(paste0('length of list is ', length(rv$cur_list)))
      output$menu_select = renderUI({
        selectInput("menuInput", 
                    "Menu Input", choices = c('All',rv$cur_list))
      })
    }
  })
  

  
  get_name_list = function(cur_df, isMaxWind){
    if (!is.null(rv$cur_df)){
      temp_df = rv$cur_df %>% filter(if (isMaxWind) !is.na(max_wind) else !is.na(min_pressure))
      temp_df['new_name'] = mapply(new_name_helper, (length(get_menu_input)==1), 
                                   temp_df$name, temp_df$storm_num, temp_df$year)
      }
  }
  
  render_wind_data = reactive({
    print('In render_wind_data')
    if (!is.null(rv$cur_df)) {
      cur_df = rv$cur_df %>% filter(!is.na(max_wind))
      cur_df['new_name'] = mapply(new_name_helper, (length(get_menu_input())==1), 
                                  cur_df$name, cur_df$storm_num, cur_df$year)
      
      name_list = unique(cur_df$new_name)
      
      temp_df = cur_df %>% 
        group_by(year, new_name, storm_num, max_intensity) %>%
        summarise(storm_max_wind = max(max_wind))
      
      temp_df$new_name = factor(temp_df$new_name,
                                levels = name_list)
      return(temp_df)
    }
  })
  
  render_pressure_data = reactive({
    print('In render_pressure_data')
    if (!is.null(rv$cur_df)) {
      cur_df = rv$cur_df %>% filter(!is.na(min_pressure))
      cur_df['new_name'] = mapply(new_name_helper, (length(get_menu_input())>1), 
                                  cur_df$name, cur_df$storm_num, cur_df$year)
      
      name_list = unique(cur_df$new_name)
      
      temp_df = cur_df %>% 
        group_by(year, new_name, storm_num, max_intensity) %>%
        summarise(storm_min_pressure = min(min_pressure))
      
      temp_df$new_name = factor(temp_df$new_name,
                                levels = name_list)
      return(temp_df)
    }
  })
  
  render_map_proxy = reactive({
    proxy = leafletProxy('my_map')
    
    # Remove all shapes previously drawn on the proxy map
    clearShapes(proxy)
    
    # Reset list of groups
    rv$group_list = list()
    #rv$date_list = unique(hu_df[year == get_menu_input(), 'time'])
    rv$cur_date = 1
    
    cur_df = rv$cur_df
    for (i in unique(cur_df$storm_num)){
      groupId = paste0(get_menu_input(),':',i)
      rv$group_list = append(rv$group_list,groupId)
      temp_df = cur_df %>% filter(storm_num == i) 
      
      proxy %>% addPolylines(data=temp_df,lng = ~longitude, lat = ~latitude, 
                             group = groupId) %>%
        addCircles(data = temp_df, ~longitude, ~latitude, group = groupId, 
                   color = ~color, popup = ~popup_string, radius = 11000,
                   fillOpacity= 0.6)
    }
    
  })
  
  observeEvent(input$tabs, {
    print('Working on tabs')
    print(paste0('Cur input = ', rv$cur_input))
      if (input$tabs == 'Data'){
          print('Working on Data')
          output$plot1 = renderPlot(
            ggplot(data = render_wind_data()) + geom_bar(aes(x=new_name, y=storm_max_wind, 
                                                        fill=max_intensity), stat = 'identity') +
              theme_economist() + theme(axis.text.x = element_text(angle = 90,
                                                                   size = rel(1.3), vjust = 0.5)) +
              labs(x = paste0('Storm Name ', '(', get_menu_input(), ')'), y='Maximum Wind Speed (knots)') +
              theme(axis.title = element_text(size = rel(1.5), margin = 20))
          )
          output$plot2 = renderPlot(
            ggplot(data = render_pressure_data()) + geom_bar(aes(x=new_name, y=storm_min_pressure, 
                                                             fill=max_intensity), stat = 'identity') +
              theme_economist() + theme(axis.text.x = element_text(angle = 90,
                                                                   size = rel(1.3), vjust = 0.5)) +
              labs(x = paste0('Storm Name ', '(', get_menu_input(), ')'), y='Minimum Pressure (millibar)') +
              theme(axis.title = element_text(size = rel(1.5), margin = 20))
          )
      } else render_map_proxy()
  })
  
  observeEvent(input$menuInput,{
    output$menu_items <- renderPrint({ get_menu_input() })
    print('In observe menu')
      rv$cur_input = get_menu_input()
      rv$cur_df = hu_df %>%
        filter(eval(as.name(rv$cur_filter)) %in% get_menu_input())
      print(paste0('Setting cur_df to dataframe of size ', paste0(dim(rv$cur_df),collapse = ':')))
    
    if (input$tabs == 'Map') render_map_proxy()

      #output$group_list = renderPrint({isolate(rv$group_list)})

  })
  
  observeEvent(input$animate, {
    output$current_time = renderPrint(({input$tabs}))
      cur_df = rv$cur_df
      rv$date_list = unique(rv$cur_df[,'time'])
      #output$current_time = renderPrint({rv$date_list[input$animate]})
      #output$current_time = renderPrint({input$tabs})
  })
  
  observeEvent(input$option_1967, {
    if (input$option_1967) {
      print('1967 option has been set to TRUE')
        
    }
  })
  
})