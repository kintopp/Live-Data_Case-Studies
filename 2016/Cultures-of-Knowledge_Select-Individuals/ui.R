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
# library(DT) # for nice tables
library(htmltools) # Need for htmlDependency used by custom date picker
library(shinyBS)

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
  
  wellPanel(includeMarkdown("ui/select-individuals_Intro.Rmd")),
  uiOutput("select_individuals_ui"),
  bsTooltip("select_individuals_ui",
            "Please select or type up to three names into this field to visualise their connections in the network below."),
  fluidRow(
    column(uiOutput("select.individual.1_UI"),width=4),
    column(uiOutput("select.individual.2_UI"),width=4),
    column(uiOutput("select.individual.3_UI"),width=4)),
  fluidRow(
    column(
      uiOutput("selected.individual.network_no_graph"),
      uiOutput("select.individual.network_graph_UI", width = "100%"),
      width = 8
    ),
    column(
      wellPanel(
        uiOutput("visNetwork_selected_individuals_show_timeslider_UI"),
        bsTooltip(
          "visNetwork_selected_individuals_show_timeslider_UI",
          "Toggling on the check box makes a date scroll bar appear allowing you to select a specific date (range). The statistics below show the effect of your selection on the network in numbers. Note: About half of the events in the selected dataset are undated. The lifespan of people is not taken into account.",
          "left",
          options = list(container = "body")
        ),
        uiOutput(
          "visNetwork_selected_individuals_time_period_of_interest_UI"
        ),
        uiOutput("neighbor_degree_UI"),
        bsTooltip("neighbor_degree_UI",
                  "This function allows you to examine by how many degrees of separation the selected individuals are removed from each other and the reach of their individual networks. The statistics below show how many people and organizations from the selected dataset are included within the number of neighbour degrees you chose.",
                  "left"),
        uiOutput("selected_individual_NumberOfExcluded")
      ),
      uiOutput("visNetwork_select_individual_selected_node_info"),
      width = 4
    )
  ),
  conditionalPanel(
    "typeof input.current_node_id !== 'undefined'",
    h3("Events table for selected individual"),
    HTML("The default columns show the most basic information. Columns with additional or more detailed information, including the source of the data, can be added by selecting them from the list that appears when you click in the ‘Columns to show’ box. To remove a column, click on the name of the column and press the ‘Delete’ or ‘Backspace’ key. The free-text search box allows you to search for any information within the data concerning the selected individual. You can also search per column in the search boxes below the columns.
         "),
    uiOutput(
      "visNetwork_selected_individual_connected_life_events_columns_to_show_UI"
    ),
    DT::dataTableOutput("visNetwork_selected_individual_selected_node")
    )
  
))