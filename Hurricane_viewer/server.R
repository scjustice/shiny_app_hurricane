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
      temp_df = hu_prop_df %>% 
        filter_(filter_helper()) %>% 
        mutate(new_name = paste0(name, ' (',year,')')) %>%
        select (year, new_name,
                'Hurricane: Cat5' = cat5_hours,
                'Hurricane: Cat4' = cat4_hours,
                'Hurricane: Cat3' = cat3_hours,
                'Hurricane: Cat2' = cat2_hours,
                'Hurricane: Cat1' = cat1_hours,
                'Tropical Storm' = ts_hours,
                'Tropical Depression' = td_hours) %>%
        gather(key, value, -year, -new_name) 
      temp_df$key = factor(temp_df$key, levels = intensity_list)
      g = ggplot(data = temp_df) + geom_bar(aes(x = new_name, y = value, fill= key, group = key),
                                            stat = 'identity') + 
        labs(x = 'Name (Year)', y = 'Time (Hours)', fill = 'Max Intensity') 
      } else if( 'All' %in% get_menu_input()){
        temp_df = hu_df %>% filter(year >= 1967) %>% 
        select(year,storm_num, max_intensity) %>% 
          group_by(year, storm_num) %>% distinct() %>% 
          ungroup() %>% group_by(year, max_intensity) %>% 
          summarise(count = n())
        temp_df$max_intensity = factor(temp_df$max_intensity, levels = intensity_list)
        g = ggplot(data = temp_df) + geom_line(aes(x = year, y=count, 
                                                   color= max_intensity, group=max_intensity), size=1.5) +
          geom_point(aes(x=year, y = count, color = max_intensity, group=max_intensity), size=3) +
          labs(x = 'Year', y = 'Count', color = 'Max Intensity')  
      } else if (length(get_menu_input()) < 3){
        cur_df = get_cur_df() %>% filter(intensity_string == max_intensity)
        cur_df['new_name'] = mapply(new_name_helper, (length(get_menu_input())==1), 
                                    cur_df$name, cur_df$storm_num, cur_df$year)
        cur_df[is.na(cur_df$max_wind),'max_wind'] = 30
        name_list = unique(cur_df$new_name)
        temp_df = cur_df %>% 
          group_by(year, new_name, storm_num, max_intensity) %>%
          summarise(storm_max_wind = max(max_wind, na.rm = TRUE))
        temp_df$new_name = factor(temp_df$new_name,
                                  levels = name_list)
        temp_df$max_intensity = factor(temp_df$max_intensity, levels = intensity_list)
        x_label = ifelse(length(get_menu_input()) == 1, 'Name', 'Name (Year)')
        g = temp_df %>% ggplot()  + geom_bar(aes(x=new_name, y=storm_max_wind, 
                                                         fill=max_intensity), stat = 'identity') +
        labs(x = x_label, y='Max Wind Speed (knots)', fill = 'Max Intensity') 
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
          labs(x = 'Year', y = 'Count', color='Max_intensity')
      }
      g = g + theme_economist() + theme(axis.title = element_text(size = rel(1.5), 
                                                                  margin = 20), 
                                        axis.text.x = element_text(angle = 90, size = rel(1.3), 
                                                                   vjust = 0.5))
      return(g)
  })
  
  filter_helper = function(){
    if(rv$cur_filter == 'year') return('year %in% get_menu_input()')
    else {
      ret_list = list()
      for (cur_input in get_menu_input()) {
        match_string = str_match(cur_input, '(\\w+) \\((\\d+)')
        filter_term = paste0('(name == \'', match_string[2], 
                             '\' & year == ', match_string[3], ')')
        ret_list  = append(ret_list, filter_term) 
      }
    #print(paste0('Filter helper filter = ',paste(ret_list, collapse = ' | ') ))
    return(paste(ret_list, collapse = ' | '))
    }
  }
  
  render_right_plot = reactive({
    if ('All' %in% get_menu_input()){
        cur_df = hu_prop_df %>% 
          filter(year >= 1967) %>%
           group_by(year) %>%
          summarise(year_total = sum(cat5_hours, cat4_hours, cat3_hours, 
                                     cat2_hours, cat1_hours, td_hours, ts_hours),
                    divisor = ifelse(input$option_prop, year_total, max(storm_num)),
                    'Hurricane: Cat5' = sum(cat5_hours)/divisor, 
                    'Hurricane: Cat4' = sum(cat4_hours)/divisor,
                    'Hurricane: Cat3' = sum(cat3_hours)/divisor,
                    'Hurricane: Cat2' = sum(cat2_hours)/divisor,
                    'Hurricane: Cat1' = sum(cat1_hours)/divisor,
                    'Tropical Storm' = sum(ts_hours)/divisor,
                    'Tropical Depression' = sum(td_hours)/divisor)  %>%
          gather(key, value, -year, -year_total, -divisor) 
        cur_df$key = factor(cur_df$key, levels = intensity_list)
        y_label = ifelse(input$option_prop, 'Proportion', 'Average Time (Hours)')
        
        g = ggplot(data=cur_df, aes(x=year, y=value, fill=key, group=key)) + geom_bar(stat='identity') +
          labs(x = 'Year', y=y_label, fill='Intensity') 
        if (input$option_prop) g = g + theme(axis.title.y=element_blank())
      } else if (length(get_menu_input()) < 3){
        temp_df = hu_prop_df %>% 
          filter_(filter_helper())
        temp_df['new_name'] = mapply(new_name_helper, length(get_menu_input()) == 1, 
                                     temp_df$name, temp_df$storm_num, temp_df$year)
        name_list = unique(temp_df$new_name)
        temp_df['total'] = mapply(sum, temp_df$cat5_hours, temp_df$cat4_hours, temp_df$cat3_hours,
                                  temp_df$cat2_hours, temp_df$cat1_hours, temp_df$ts_hours,
                                  temp_df$td_hours)
        temp_df$divisor = mapply(function(x,y) {ifelse(x, y, 1)},input$option_prop, temp_df$total)
        y_label = ifelse(input$option_prop, 'Proportion', 'Time (Hours)')
        x_label = ifelse(length(get_menu_input()) == 1, 'Name', 'Name (Year)')
        temp_df = temp_df %>%
          transmute(year, new_name, 'Hurricane: Cat5' = cat5_hours/divisor,
                    'Hurricane: Cat4' = cat4_hours/divisor,
                    'Hurricane: Cat3' = cat3_hours/divisor,
                    'Hurricane: Cat2' = cat2_hours/divisor,
                    'Hurricane: Cat1' = cat1_hours/divisor,
                    'Tropical Storm' = ts_hours/divisor,
                    'Tropical Depression' = td_hours/divisor) %>%
          gather(key, value, -year, -new_name) 
        temp_df$key = factor(temp_df$key, levels = intensity_list)
        temp_df$new_name = factor(temp_df$new_name, levels = name_list)
        g = ggplot(data = temp_df) + geom_bar(aes(x = new_name, y = value, 
                                                  fill= key, group = key), stat = 'identity') + 
          labs(x = x_label, y = y_label, fill = 'Max Intensity') 
        if (input$option_prop) g = g + theme(axis.title.y=element_blank())
      } else {
        cur_df = hu_prop_df %>% 
          filter(year %in% get_menu_input()) %>%
          group_by(year) %>% 
          summarise(year_total = sum(cat5_hours, cat4_hours, cat3_hours, 
                                     cat2_hours, cat1_hours, td_hours, ts_hours),
                    divisor = ifelse(input$option_prop, year_total, max(storm_num)),
                    'Hurricane: Cat5' = sum(cat5_hours)/divisor, 
                    'Hurricane: Cat4' = sum(cat4_hours)/divisor,
                    'Hurricane: Cat3' = sum(cat3_hours)/divisor,
                    'Hurricane: Cat2' = sum(cat2_hours)/divisor,
                    'Hurricane: Cat1' = sum(cat1_hours)/divisor,
                    'Tropical Storm' = sum(ts_hours)/divisor,
                    'Tropical Depression' = sum(td_hours)/divisor)  %>%
          gather(key, value, -year, -year_total, -divisor) 
        cur_df$key = factor(cur_df$key, levels = intensity_list)
        cur_df$year = factor(cur_df$year, levels = 1967:2017)
        y_label = ifelse(input$option_prop, 'Proportion', 'Average Time (Hours)')
        g = ggplot(data=cur_df, aes(x=year, y=value, fill=key, group=key)) + geom_bar(stat='identity') +
          labs(x = 'Year', y=y_label, fill = 'Intensity') 
      }
      g = g + theme_economist() +
        theme(axis.title = element_text(size = rel(1.5), margin = 20), 
              axis.text.x = element_text(angle = 90, size = rel(1.3), vjust = 0.5))
      if (input$option_prop) g = g + theme(axis.title.y=element_blank())
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
  
  observeEvent(input$option_prop, {
    render_right_plot()
  })
  session$onSessionEnded(stopApp)
  
})