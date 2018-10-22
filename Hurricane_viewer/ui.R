library(shiny)
library(shinydashboard)
library(leaflet)

shinyUI(dashboardPage(
  dashboardHeader(title = 'Hurricane Viewer'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Year", startExpanded = TRUE,icon = icon("calendar"),
               # Input directly under menuItem
        selectInput("inputYear", "Input Year", choices = append(c(''),year_list)),
        hr(),
        fluidRow(
          column(12, verbatimTextOutput("year")),
        fluidRow(
          column(12, verbatimTextOutput("layerIds"))
        )
        
        )
      )         
    )
  ),
  dashboardBody(
    fluidRow(
      title = 'Current Hurricane',
      leafletOutput("my_map")
    )
  )
))