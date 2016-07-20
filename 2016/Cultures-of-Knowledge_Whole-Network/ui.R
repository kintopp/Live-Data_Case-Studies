## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================


## ==== Packages to load for server

library(shiny) # Some advanced functionality depends on the shiny package being loaded client-side, including plot.ly
library(visNetwork)
library(networkD3)
library(lubridate)
library(shinyBS)
library(htmltools) # Need for htmlDependency used by custom date picker

## ==== Global Variables (client-side)

library(shinythemes) # Template uses the cerulean theme as it is pretty

shinyUI(fluidPage(
  ## ==== Include google analytics code
  tags$head(includeScript("google-analytics.js")),
  
  ## ==== Automatically include vertical scrollbar
  ## ==== This prevents the app from reloading content when the window is resized which would otherwise result in the
  ## ==== appearance of the scrollbar and the reloading of content. Note that "click data" may still be lost during
  ## ==== resizing, as discussed here https://github.com/rstudio/shiny/issues/937
  tags$style(type = "text/css", "body { overflow-y: scroll; }"),
  
  
  theme = shinytheme("cerulean"),
  
  HTML("<center><h2>Visualizations of the Prosopographical Network of Samuel Hartlib</h2></center>"),
  
  wellPanel(includeMarkdown("ui/whole-network_Intro.Rmd")),
  
  fluidRow(
    column(
      width = 4,
      # uiOutput("visNetwork_wholeNetwork_highlighted_node_UI"),
      wellPanel(
        uiOutput("visNetwork_wholeNetwork_highlighted_node_UI"),
        bsTooltip(
          "visNetwork_wholeNetwork_highlighted_node_UI",
          "Selecting a name from the drop-down menu highlights in orange the selected individual.",
          "right",
          options = list(container = "body", delay= '{"show":"500", "hide":"100"}')
        ),
        uiOutput("visNetwork_wholeNetwork_show_timeslider_UI"),
        bsTooltip(
          "visNetwork_wholeNetwork_show_timeslider_UI",
          "Toggling on the check box makes a date scroll bar appear allowing you to select a specific date (range). The statistics below show the effect of your selection on the network in numbers. Note: About half of the events in the selected dataset are undated. The lifespan of people is not taken into account.",
          "right",
          options = list(container = "body", delay= '{"show":"500", "hide":"100"}')
        ),
        uiOutput("visNetwork_wholeNetwork_dateRangeInput_UI"),
        uiOutput("visNetwork_wholeNetwork_time_period_of_interest_UI"),
        uiOutput("visNetwork_wholeNetwork_HighlightedCategoryUI"),
        bsTooltip(
          "visNetwork_wholeNetwork_HighlightedCategoryUI",
          "Selecting an event or relationship type from the drop-down menu highlights in orange all edges concerned. This function allows you to examine how people were connected. More detailed information can be found in the events table which appears below the graph when you click on a node.",
          "right",
          options = list(container = "body", delay= '{"show":"500", "hide":"100"}')
        ),
        uiOutput("visNetwork_wholeNetwork_ExcludedCategoriesUI"),
        bsTooltip(
          "visNetwork_wholeNetwork_ExcludedCategoriesUI",
          "Selecting an event or relationship type from the drop-down menu removes from the graph the edges concerned as well as the nodes that were connected to the rest of the network by these edges if this event or relationship type was the only one connecting them. The statistics below show the effect of excluding these connections on the network in numbers.",
          "right",
          options = list(container = "body", delay= '{"show":"500", "hide":"100"}')
        ),
        uiOutput("visNetwork_wholeNetwork_NumberOfExcluded")
      ),
      conditionalPanel(
        "typeof input.current_node_id !== 'undefined'",
        uiOutput("visNetwork_wholeNetwork_selected_node_info")
      )
    ),
    column(
      width = 8,
      visNetworkOutput(
        "visNetwork_wholeNetwork",
        width = "100%",
        height = "600px"
      )
    )
  ),
  
  conditionalPanel(
    "typeof input.current_node_id !== 'undefined'",
    HTML(
      "<p>The default columns show the most basic information. Columns with additional information, including the source of the data, can be added to the table by selecting them from the list that appears when you click in the ‘Columns to show’ box. To remove a column, click on the name of the column and press the ‘Delete’ or ‘Backspace’ key. The free-text search box allows you to search for any information within the data concerning the selected individual. You can also search per column in the search boxes below the columns.</p>"
    ),
    uiOutput(
      "visNetwork_whole_network_connected_life_events_columns_to_show_UI"
    ),
    # dataTableOutput("visNetwork_whole_network_selected_node"),
    uiOutput("node_summary_DT_UI")
  )
  
  
  
))