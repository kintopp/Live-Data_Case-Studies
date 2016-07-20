library(shiny)
library(leaflet)

shinyUI(fluidPage(
  # fluidRow(
  #   column(wellPanel(uiOutput("show_timeslider_UI"),
  #          uiOutput("time_period_of_interest_UI")) ,
  #
  #          # wellPanel(uiOutput("marker_mouseover")),
  #          width = 4),
  #   column(leafletOutput("irelandmap"), width = 8)
  # ),
  wellPanel(
    uiOutput("show_timeslider_UI"),
    uiOutput("time_period_of_interest_UI")
  ),
  leafletOutput("irelandmap", height = "600px"),
  
  
  conditionalPanel(
    "typeof input.irelandmap_marker_click !== 'undefined'",
    h1("Individuals born at the selected address:"),
    dataTableOutput("marker_click")
  )
))
# fluidPage("foo"))
