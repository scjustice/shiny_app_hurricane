library(shiny)

shinyServer(function(input, output, session){
  output$year <- renderPrint({ input$inputYear })
  
  output$my_map <- renderLeaflet({
    map = leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(-80, 30, zoom = 4)
  })
    
  observeEvent(input$inputYear,{
    temp = hu_df %>%
      filter(year == input$inputYear)
    proxy = leafletProxy('my_map')
    
    for (i in layer_list) {
      proxy %>% removeShape(layerId = i)
      layer_list = list()
    }
    
    for (i in unique(temp$storm_num)){
      layerId = str_interp('${input$inputYear}:${i}')
      append(layer_list,layerId)
      proxy %>% addPolylines(data = temp[temp$storm_num ==i, ], ~longitude, ~latitude, layerId = layerId)
      #addMarkers(lng = temp[1, 'longitude'], lat = temp[1, 'latitude'], group = 'storm_num')
    }
                 
  #      addProviderTiles("Esri.WorldStreetMap")
  })
})