## ==== Packages to load for server

library(shiny) # Some advanced functionality depends on the shiny package being loaded server-side, including plot.ly
library(plotly)
library(scales)
library(dplyr)
library(plyr)
library(lubridate)
library(stringr)

library(maps)
library(mapproj)
library(leaflet)
library(rgdal)
library(GISTools)
library(sp)

## ==== Global Variables (server-side)

## ==== Tab selection variables (these are required to support anchor links, see within shinyServer)
url1 <- url2 <- ""

## ==== shinyServer

shinyServer(function(input, output, session){
  
  if(!file.exists("data/letters_sent_from_usa.RData")){
    source("process-letters.R",local = TRUE)
  } else {
    ## Load letters_sent_from_usa symbol
    load(file = "data/letters_sent_from_usa.RData")
    load(file = "data/location_name_df.RData")
  }
  
  # source("server/data-processing.R",local = TRUE)
  source("server/visualisations-and-ui.R",local = TRUE)
  source("server/leaflet-choropleth.R", local = TRUE)

  
  
})