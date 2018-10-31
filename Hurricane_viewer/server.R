shinyServer(function(input, output, session){
  
  output$menu_choice <- renderPrint({ input$menuInput })
  output$ss_image = renderImage({
    return(list(src='data/ss_table.PNG', contentType = "image/png",
                width = 660, height = 400))}, deleteFile = FALSE)
  
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
                      option_1967 = FALSE,
                      cur_data_select = 1,
                      cur_map_select = 1,
                      cur_date_list = NULL,
                      cur_date_index = 1)

  
  get_menu_input = reactive({
    validate(
      need(input$menuInput != "", 'Please make selection for analysis')
    )
    return(input$menuInput)
  })
  
#  get_cur_date_index = reactive({
#    validate(
#      need(input$animate_index != '', 'Please select an input')
#    )
#      return(input$animate_index)
#  })
  
  get_cur_df = reactive({
    validate(
      need(!is.null(rv$cur_df), 'Please make selection for analysis')
    )
    return(rv$cur_df)
  })
  
  set_cur_df = reactive({
    rv$cur_df = hu_df %>%
      filter_(filter_helper())
  })
  
  get_cur_filter = function(){
    if(rv$cur_filter == 'name') {}
    
  }
  
  
#  output$menu_select = renderUI({
#    selectizeInput("menuInput", 
#                   "Menu Input", choices = rv$cur_list,
#                   multiple = TRUE, options = list(maxItems = 10))
#  })
  
  get_option_1967 = reactive({
    return(input$option_1967)
  })
  
  
  render_menu_select = reactive({
    if (get_option_1967()) {
      if (input$radio == 1) rv$cur_list = year_list_1967
      else rv$cur_list = name_list_1967
    } else {
      if (input$radio == 1) rv$cur_list = year_list
      else rv$cur_list = name_list
    }
    intro_string = ifelse(input$radio == 1, 'Select Year', 'Select Name')
    output$menu_select = renderUI({
      selectizeInput("menuInput", 
                     intro_string, choices = rv$cur_list,
                     multiple = TRUE, options = list(maxItems = 10))
    }) 
  })
  
#  render_animate = reactive({
#    output$animate = renderUI({
#      sliderInput('animate_index', 'Time to animate', min = rv$cur_date_index, 
#                  max = length(rv$cur_date_list), value = 1, step = 1, 
#                  animate = animationOptions(interval = 300)
#                  )
#      
#    })
#  })
    
  
  observeEvent(input$radio, {
    rv$cur_filter = case_when(input$radio == 1 ~ 'year',
                              input$radio == 2 ~ 'name')
    render_menu_select()
  })
  
  get_name_list = function(cur_df, isMaxWind){
      temp_df = get_cur_df() %>% filter(if (isMaxWind) !is.na(max_wind) else !is.na(min_pressure))
      temp_df['new_name'] = mapply(new_name_helper, (length(get_menu_input)==1), 
                                   temp_df$name, temp_df$storm_num, temp_df$year)
  }
  
  render_left_plot = reactive({
      if(input$radio == 2){
        temp_df = get_cur_df() %>% 
          filter(intensity_string %in% intensity_list) %>%
          group_by(year, name) %>% mutate(new_name = paste0(name, ' (',year,')'))
        temp_df$intensity_string = factor(temp_df$intensity_string, levels = intensity_list)
        g = ggplot(data = temp_df) + geom_bar(aes(x = new_name, fill = intensity_string)) +
          theme_economist() + theme(axis.text.x = element_text(angle = 90, 
                                                               size = rel(1.3), vjust = 0.5)) +
          labs(x = 'Name (Year)', y = 'Observations', fill = 'Intensity') +
          theme(axis.title = element_text(size = rel(1.5), margin = 20), 
                axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))
      } else if( 'All' %in% get_menu_input()){
        temp_df = hu_df %>% filter(year >= 1967) %>% 
        select(year,storm_num, max_intensity) %>% 
          group_by(year, storm_num) %>% distinct() %>% 
          ungroup() %>% group_by(year, max_intensity) %>% 
          summarise(count = n())
        temp_df$max_intensity = factor(temp_df$max_intensity, levels = intensity_list)
        g = ggplot(data = temp_df) + geom_line(aes(x = year, y=count, 
                                                   color= max_intensity, group=max_intensity), size=1.5) +
          geom_point(aes(x = year, y=count, color = max_intensity), size = 3) + 
          theme_economist() + labs(x = 'Year', y = 'Max Intensity Count', color = 'Max Intensity') +
          theme(axis.title = element_text(size = rel(1.5), margin = 20), 
                axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5)) 
      } else if (length(get_menu_input()) < 3){
        cur_df = get_cur_df() %>% filter(!is.na(max_wind))
        cur_df['new_name'] = mapply(new_name_helper, (length(get_menu_input())==1), 
                                    cur_df$name, cur_df$storm_num, cur_df$year)
        name_list = unique(cur_df$new_name)
        temp_df = cur_df %>% 
          group_by(year, new_name, storm_num, max_intensity) %>%
          summarise(storm_max_wind = max(max_wind))
        temp_df$new_name = factor(temp_df$new_name,
                                  levels = name_list)
        temp_df$max_intensity = factor(temp_df$max_intensity, levels = intensity_list)
        g = temp_df %>% ggplot()  + geom_bar(aes(x=new_name, y=storm_max_wind, 
                                                         fill=max_intensity), stat = 'identity') +
        theme_economist() + theme(axis.text.x = element_text(angle = 90,
                                  size = rel(1.3), vjust = 0.5)) +
        labs(x = 'Storm Name', y='Maximum Wind Speed (knots)', fill = 'Max Intensity') +
          theme(axis.title = element_text(size = rel(1.5), margin = 20), 
                axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))  
      } else {
        temp_df = get_cur_df() %>% 
          select(year,storm_num, max_intensity) %>% 
          group_by(year, storm_num) %>% distinct() %>% 
          ungroup() %>% group_by(year, max_intensity) %>% 
          summarise(count = n())
        temp_df$max_intensity = factor(temp_df$max_intensity, levels = intensity_list)
        temp_df$year = factor(temp_df$year, levels = c(1851:2017))
        g = ggplot(data = temp_df) + geom_line(aes(x = year, y=count,
                                                   color= max_intensity, group=max_intensity), size=1.5) +
          geom_point(aes(x = year, y=count, color = max_intensity), size = 3) + 
          theme_economist() + labs(x = 'Year', y = 'Max Intensity Count', color='Max_intensity') +
          theme(axis.title = element_text(size = rel(1.5), margin = 20), 
                axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))
        
      }
      return(g)
  })
  
  filter_helper = function(){
    if(rv$cur_filter == 'year') return('year %in% get_menu_input()')
    else {
      ret_list = list()
      for (cur_input in get_menu_input()) {
        match_string = str_match(cur_input, '(\\w+) \\((\\d+)')
        ret_list  = append(ret_list, paste0('(name == \'', match_string[2], 
                                            '\' & year == ', match_string[3], ')'))                        
      }
    return(paste(ret_list, collapse = ' | '))
    }
  }
  
  render_right_plot = reactive({
    if(input$radio == 2){
      temp_df = get_cur_df() %>% 
        filter(!is.na(min_pressure)) %>% 
        group_by(year, name, max_intensity) %>% 
        summarise(storm_min_pressure = min(min_pressure)) %>%
        mutate(new_name = paste0(name, ' (',year,')'))
      temp_df$max_intensity = factor(temp_df$max_intensity, levels = intensity_list)
      g = ggplot(data = temp_df) + geom_bar(aes(x = new_name, y = storm_min_pressure, fill= max_intensity),
                                            stat = 'identity') + theme_economist() + 
        labs(x = 'Name (Year)', y = 'Min Pressure', fill = 'Max Intensity') +
        theme(axis.title = element_text(size = rel(1.5), margin = 20), 
              axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))
    } else if ('All' %in% get_menu_input()){
        cur_df = hu_prop_df %>% 
          select(c('year',
                   'Hurricane: Cat5' = cat5_prop, 
                   'Hurricane: Cat4' = cat4_prop, 
                   'Hurricane: Cat3' = cat3_prop, 
                   'Hurricane: Cat2' = cat2_prop, 
                   'Hurricane: Cat1' = cat1_prop, 
                   'Tropical Storm' = ts_prop, 
                   'Tropical Depression' = td_prop)) %>%
          filter(year >= 1967) %>% 
          gather(key, value, -year) 
        cur_df$key = factor(cur_df$key, levels = intensity_list)
        g = ggplot(data=cur_df, aes(x=year, y=value, fill=key, group=key)) + geom_bar(stat='identity') +
          theme_economist() +labs(x = 'Year', y='Intensity Proportional to Year', fill='Intensity') +
          theme(axis.title = element_text(size = rel(1.5), margin = 20), 
                axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))
      } else if (length(get_menu_input()) < 3){
        cur_df = get_cur_df() %>% filter(!is.na(min_pressure))
        cur_df['new_name'] = mapply(new_name_helper, (length(get_menu_input())==1), 
                                    cur_df$name, cur_df$storm_num, cur_df$year)
        name_list = unique(cur_df$new_name)
        temp_df = cur_df %>% 
          group_by(year, new_name, storm_num, max_intensity) %>%
          summarise(storm_min_pressure = min(min_pressure))
        temp_df$max_intensity = factor(temp_df$max_intensity, levels = intensity_list)
        temp_df$new_name = factor(temp_df$new_name,
                                  levels = name_list)
        g = temp_df %>% ggplot()  + geom_bar(aes(x=new_name, y=storm_min_pressure, 
                                                 fill=max_intensity), stat = 'identity') +
          theme_economist() + labs(x = 'Storm Name', 
                                   y='Minimum Pressure (millibars)', fill = 'Max Intensity') +
          theme(axis.title = element_text(size = rel(1.5), margin = 20), 
                axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))
      } else {
        cur_df = hu_prop_df %>% 
          select(c('year',
                   'Hurricane: Cat5' = cat5_prop, 
                   'Hurricane: Cat4' = cat4_prop, 
                   'Hurricane: Cat3' = cat3_prop, 
                   'Hurricane: Cat2' = cat2_prop, 
                   'Hurricane: Cat1' = cat1_prop, 
                   'Tropical Storm' = ts_prop, 
                   'Tropical Depression' = td_prop)) %>% 
          filter(year %in% get_menu_input()) %>% 
          gather(key, value, -year) 
        cur_df$key = factor(cur_df$key, levels = intensity_list)
        plot_year = unique(cur_df$year)
        cur_df$year = factor(cur_df$year, levels = 1967:2017)
        g = ggplot(data=cur_df, aes(x=year, y=value, fill=key, group=key)) + geom_bar(stat='identity') +
          theme_economist() + labs(x = 'Year', y='Intensity Proportional to Year',
                                   fill = 'Intensity') +
          theme(axis.title = element_text(size = rel(1.5), margin = 20), 
                axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))
      }
      return(g)
  })
  
  
  render_map_proxy = reactive({
    proxy = leafletProxy('my_map')
    
    # Remove all shapes previously drawn on the proxy map
    clearShapes(proxy)
    
    # Reset list of groups
    rv$group_list = list()

    if (input$radio == 1){
      for (i in get_menu_input()){
        for (j in unique((get_cur_df())$storm_num)){
          groupId = paste0(i,':',j)
          rv$group_list = append(rv$group_list,groupId)
          temp_df = get_cur_df() %>% filter(eval(as.name(rv$cur_filter)) == i & 
                                              storm_num == j) 
          
          proxy %>% addPolylines(data=temp_df,lng = ~longitude, lat = ~latitude, 
                                 group = groupId) %>%
            addCircles(data = temp_df, ~longitude, ~latitude, group = groupId, 
                       color = ~color, popup = ~popup_string, radius = 11000,
                       fillOpacity= 0.6)
        }
      }
    } else {
      temp_df = get_cur_df() %>% mutate(new_name = paste0(name, ' (',year,')'))
      for (i in unique(temp_df$new_name)) {
        groupId = i
        rv$group_list = append(rv$group_list,groupId)
        temp2_df = temp_df %>% filter(new_name == i)
        proxy %>% addPolylines(data=temp2_df,lng = ~longitude, lat = ~latitude, 
                               group = groupId) %>%
          addCircles(data = temp2_df, ~longitude, ~latitude, group = groupId, 
                     color = ~color, popup = ~popup_string, radius = 11000,
                     fillOpacity= 0.6)
      }
    }
  })
  
  observeEvent(input$radio, {
    render_menu_select()
    rv$cur_df = NULL
    if (input$tabs == 'Data') rv$cur_data_select = input$radio
    else rv$cur_map_select = input$radio
  })
  
  observeEvent(input$tabs, {
    #print('Working on tabs')
    if (input$tabs == 'Data'){
        output$plot1 = renderPlot(
          render_left_plot()
        )
        output$plot2 = renderPlot(
          render_right_plot()
        )
    } else if (input$tabs == 'Map') {
#      if (is.null(get_cur_df)){
#          output$animate = renderUI({
#            sliderInput('animate_index', 'Time to animate', min = 1, 
#                        max = 1, value = 1, step = 1, 
#                        animate = FALSE
#            )
#        })
#      }
      render_map_proxy()
#      render_animate()
    } else {
      output$ss_table = renderImage({list(src="image/ss_table.png", contentType = 'image/png')})
    }
  })
  
  observeEvent(input$menuInput,{
    output$menu_items <- renderPrint({ get_menu_input() })
    rv$cur_input = get_menu_input()
    set_cur_df()
#    rv$cur_df = hu_df %>%
#      filter_(filter_helper())
#    if (get_menu_input() == '') rv$cur_df = NULL
#    rv$cur_date_list = unique(get_cur_df()$date_time)
    if (input$tabs == 'Map') {
      if(is.null(input$menuInput)){
        proxy = leafletProxy('my_map')
        # Remove all shapes previously drawn on the proxy map
        clearShapes(proxy)
      }
      render_map_proxy()
#      render_animate()
    }

  })
  
#  observeEvent(input$animate_index, {
#    output$animate_time = renderPrint(({rv$cur_date_index}))
#      cur_df = get_cur_df()
#      rv$date_index = input$animate_index
#      
#  })
  
  observeEvent(input$option_1967, {
      rv$option_1967 = input$option_1967
      render_menu_select()
      proxy = leafletProxy('my_map')
      # Remove all shapes previously drawn on the proxy map
      clearShapes(proxy)
#      render_map_proxy()
  })
  
  session$onSessionEnded(stopApp)
  
})