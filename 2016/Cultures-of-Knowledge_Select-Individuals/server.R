  ## ==== Packages to load for server

library(shiny) # Some advanced functionality depends on the shiny package being loaded server-side, including plot.ly
library(reshape2) # for dcast, count and others
library(visNetwork) # for interactive graph
library(plyr) # for data processing
library(dplyr) # for data processing
# library(DT) # for nice tables
library(igraph) # for network analysis
library(ggplot2)
library(networkD3)
library(scales)
library(htmltools) # Need for htmlDependency used by custom date picker 
library(lubridate)
library(shinyBS)
# library(shinyjs) could be useful for hiding content but appears to cause other issues


## ==== Global Variables (server-side)

## ==== Tab selection variables (these are required to support anchor links, see within shinyServer)
url1 <- url2 <- ""

## ==== shinyServer
source("server/data-processing.R",local = TRUE)
shinyServer(function(input, output, session){
  
  
  source("server/shared-data-and-functions.R",local = TRUE)
  source("server/server_select-individuals.R",local = TRUE)

  
})