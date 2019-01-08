shinyUI(dashboardPage(
  dashboardHeader(title = 'Hurricane Viewer'),
  dashboardSidebar(
    sidebarMenu( id = 'tabs',
      menuItem("Info", tabName = 'Info', icon = icon("info")),
      menuItem('Data', tabName = 'Data', icon= icon("signal")),
      menuItem("Map", tabName = 'Map', icon = icon("calendar")),
               # Input directly under menuItem
      hr(),
      conditionalPanel(
        condition = 'input["tabs"] != "Info"',
        radioButtons("radio", label = h4("Analyze by:"),
                     choices = list("Year" = 1, "Name" = 2), 
                     selected = 1),
        uiOutput('menu_select'),
        checkboxInput('option_1967','Since 1967 only'),
        hr()
      )
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
            condition = "input['option_1967'] == true && input['radio'] == 1",
            box(
              plotOutput('plot2')
            ),
            fluidRow(
              column(6, offset = 6, checkboxInput('option_prop','Show Proportional'))
            )
          )
        )
      ),
      tabItem(tabName = 'Info',
              fillPage(
                h1('Visualizing Hurricane Data'),
                h3('Objective:'),
                h4(p('I wanted to use R and the shiny framework to create a shiny app for exploring the hurricane 
                     data and examining the trends of storm intensity to see if there has been any increase in 
                     intensity.\nMore details can be found on this',a(href='https://nycdatascience.com/blog/student-works/visualizing-hurricane-data-with-shiny/',
                   'blog post'))),
                br(),
                h3('Saffir-Simpson Scale'),
                title = 'Hurricane Info',
                img(src='images/saffir-simpson-scale.PNG', contentType = "image/png",
                      width = 440, height = 270),
                br(),
                br(),
                h4(p('Developed by '),a(href ='https://github.com/scjustice/shiny_app_hurricane',
                                        'Sean Justice')),
                br(),
                h4(p('Dataset provided by the National Hurricane Center '),
                   a(href='https://www.nhc.noaa.gov/data/','Link'))
               ))
    )
  )
))