## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

## ==== Packages to load for server

library(shiny) # Some advanced functionality depends on the shiny package being loaded client-side, including plot.ly
library(plotly)
library(leaflet)

## ==== Global Variables (client-side)

library(shinythemes) # Template uses the cerulean theme as it is pretty

shinyUI(fluidPage(

## ==== Include google analytics code
  tags$head(includeScript("google-analytics.js")),
  
## ==== Automatically include vertical scrollbar
## ==== This prevents the app from reloading content when the window is resized which would otherwise result in the
## ==== appearance of the scrollbar and the reloading of content. Note that "click data" may still be lost during 
## ==== resizing, as discussed here https://github.com/rstudio/shiny/issues/937
  tags$style(type="text/css", "body { overflow-y: scroll; }"),

  theme = shinytheme("cerulean"),
  
  source("ui/america-map.R", local = TRUE)$value
  )
)