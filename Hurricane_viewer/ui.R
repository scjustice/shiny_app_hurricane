shinyUI(dashboardPage(
  dashboardHeader(title = 'Hurricane Viewer'),
  dashboardSidebar(
    sidebarMenu( id = 'tabs',
      menuItem('Data', tabName = 'Data', icon= icon("signal")),
      menuItem("Map", tabName = 'Map', icon = icon("calendar")),
               # Input directly under menuItem
      hr(),
      radioButtons("radio", label = h4("Analyze by:"),
                   choices = list("Year" = 1, "Name" = 2), 
                   selected = 1),
      uiOutput('menu_select'),
      checkboxInput('option_1967','Since 1967 only'),
      hr(),
      conditionalPanel(
        condition = 'input["tabs"] == "Map"',
        fluidRow(
          sliderInput('animate', 'Time to animate', min = 0, max = 3, value = 0, animate = TRUE )
        )
      ),
      fluidRow(column(12, verbatimTextOutput("animate_time")))
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
      )
    )
  )
))