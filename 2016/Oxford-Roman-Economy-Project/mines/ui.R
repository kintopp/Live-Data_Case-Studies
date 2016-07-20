## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Data Source: local file
## ================================================================================


library(shiny)
library(leaflet)
library(sp)

shinyUI(fillPage(
  ## Drop minor ticks from sliders
  tags$style(type = "text/css", "
             .irs-grid-pol {display: none;}
             "),
  wellPanel(fluidRow(
    column(h2("Mines in OxRep"), width = 4),
    
    column(
      selectInput(
        "selected_map_tile",
        label = "Map Style",
        choices = c(
          "Hydda.Base",
          "OpenTopoMap",
          "Thunderforest.Landscape",
          "Esri.WorldShadedRelief",
          "Esri.OceanBasemap"
        ),
        selected = "Hydda.Base"
      ),
      width = 4
    ),
    column(
      selectInput(
        "plot_marker",
        label = "Plot Markers",
        choices = c("Mine Icon", "Circles")
      ),
      width = 4
    )
  )),
  h6("Map icons provided by https://mapicons.mapsmarker.com/"),
  leafletOutput("mines_map", height = "100%")
  , padding = 10))