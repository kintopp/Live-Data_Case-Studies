## ==== Packages to load for server

library(shiny) # Some advanced functionality depends on the shiny package being loaded server-side, including plot.ly
library(plotly)
library(scales)
library(dplyr)
library(plyr)
library(lubridate)
library(stringr)

## ==== Global Variables (server-side)

## ==== Tab selection variables (these are required to support anchor links, see within shinyServer)
url1 <- url2 <- ""

## ==== shinyServer

shinyServer(function(input, output, session){
  
  source("server/data-processing.R",local = TRUE)
  source("server/visualisations-and-ui.R",local = TRUE)

  
})