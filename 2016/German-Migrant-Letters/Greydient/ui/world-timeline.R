## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

fluidPage(
    HTML(
      "<h2>Writing home: how German immigrants found their place in the US</h2>"
    ),
  #     fluidRow(
  #     column(HTML("Filter letters by date sent?"), width = 3),
  #     column(uiOutput("show_timeslider_UI"), width = 2)
  #     ),
  wellPanel(
    uiOutput("show_letters_where_receive_unknown_UI"),
  uiOutput("show_timeslider_UI"),
  # uiOutput("legend_type_UI"),
  fluidRow(column(
    uiOutput("time_period_of_interest_UI"),
    width = 12
  ))
  ),
  # plotlyOutput("world_map", width = "100%", height = "1200px")
  uiOutput("worldmap_via_renderUI")
  )
