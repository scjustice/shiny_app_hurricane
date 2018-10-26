shinyUI(dashboardPage(
  dashboardHeader(title = 'Hurricane Viewer'),
  dashboardSidebar(
    sidebarMenu( id = 'tabs',
      menuItem('Data', startExpanded = TRUE, tabName = 'Data', icon= icon("signal")),
      menuItem("Map", tabName = 'Map', icon = icon("calendar")),
               # Input directly under menuItem
      selectInput("inputYear", "Input Year", choices = append(c(''),year_list)),
      hr(),
      fluidRow(
        column(12, verbatimTextOutput("year"))
      ),
      hr(),
      fluidRow(
        sliderInput('animate', 'Time to animate', min = 0, max = 3, value = 0 )
      ),
      hr(),
      fluidRow(
        column(12, verbatimTextOutput("current_time"))
      )         
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
          h3('Data plots go here'),
          box(
            plotOutput('plot')
          )
          
        )
      )
    )
  )
))