shinyUI(dashboardPage(
  dashboardHeader(title = 'Hurricane Viewer'),
  dashboardSidebar(
    sidebarMenu( id = 'tabs',
      menuItem('Data', tabName = 'Data', icon= icon("signal")),
      menuItem("Map", tabName = 'Map', icon = icon("calendar")),
      menuItem("Info", tabName = 'Info', icon = icon("info")),
      
               # Input directly under menuItem
      hr(),
      radioButtons("radio", label = h4("Analyze by:"),
                   choices = list("Year" = 1, "Name" = 2), 
                   selected = 1),
      uiOutput('menu_select'),
      checkboxInput('option_1967','Since 1967 only'),
      hr()
#      uiOutput('animate'),
#        condition = 'input["tabs"] == "Map"',
#        fluidRow(
#          sliderInput('animate', 'Time to animate', min = 0, max = 3, value = 0, step = 1, animate = TRUE )
#        )
#      fluidRow(column(12, verbatimTextOutput("menu_choice")))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Map',
        fillPage(
          title = 'Hurricane Map',
          #h3('Hurricane Map'),
          leafletOutput("my_map", height = 700)
        )
      ),
      tabItem(tabName = 'Data',
        fillPage(
          title = 'Hurricane Data',
          box(
            plotOutput('plot1')
          ),
          conditionalPanel(
            condition = "input['option_1967'] == true",
            box(
              plotOutput('plot2')
            )
          )
        )
      ),
      tabItem(tabName = 'Info',
              fillPage(
                h2('Saffir-Simpson Scale'),
                title = 'Hurricane Info',
                img(src='images/ss_table.PNG', contentType = "image/png",
                      width = 660, height = 400),
                br(),
                br(),
                h4(p('Developed by '),a(href ='mailto:sean.justice@gmail.com','Sean Justice')),
                br(),
                h4(p('Dataset provided by the National Hurricane Center '),
                   a(href='https://www.nhc.noaa.gov/data/','Link'))
                
              ))
    )
  )
))