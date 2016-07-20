library(shiny)
library(leaflet)
library(lubridate)
library(stringr)
library(plyr)
library(scales)

shinyServer(function(input, output) {
  source("data-processing.R", local = TRUE)
  source("visualisation-and-ui.R", local = TRUE)
})