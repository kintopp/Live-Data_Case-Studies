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
library(plyr)

## Markers provided by https://mapicons.mapsmarker.com/

## ============== Filter shipwrecks by sites ====================================
## ==============================================================================

imported_shipwrecks <-
  read.csv("data/Current OxREP database/shipwrecks.csv",
           stringsAsFactors = F)
shipwrecks <- imported_shipwrecks

sites_df <-
  read.csv("data/Current OxREP database/sites.csv", stringsAsFactors = F)
## Extract Wreck Sites
sites_df <- sites_df[sites_df$sitetype == "Wreck Site",]
## Drop sites with coords 0,0
sites_df <-
  sites_df[sites_df$sitelat != 0 & sites_df$sitelong != 0,]
## Drop NA in locations
sites_df <-
  sites_df[!is.na(sites_df$sitelat) | !is.na(sites_df$sitelong),]


## Drop those sitenames that are not also in the shipwrecks.csv file
sites_df <- sites_df[sites_df$sitename %in% shipwrecks$sitename, ]
## Merge dataframes:
shipwrecks_with_locations <- merge(sites_df, shipwrecks)
## Remove undated:
shipwrecks_with_locations <-
  shipwrecks_with_locations[!is.na(shipwrecks_with_locations$ante_0) &
                              !is.na(shipwrecks_with_locations$ante_0),]

shipwrecks_with_locations_spdf <-
  SpatialPointsDataFrame(coords = shipwrecks_with_locations[, c("sitelong", "sitelat")], data = shipwrecks_with_locations)



## =========================== Date Filter ====================================
## ==============================================================================

class(shipwrecks_with_locations$ante_0)

shipwrecks_with_locations$ante_0

## =========================== Labellers ====================================
## ==============================================================================

shipwreck_labeller <-
  function(locname = NA,
           post_0 = NA,
           ante_0 = NA,
           site_area = NA) {
    paste0(
      # "<p>", Name, "</p>",
      "<p>Location Name: ",
      locname,
      "</p>",
      "<p>Site Area: ",
      site_area,
      "</p>",
      "<p>Date Range: ",
      "from ",
      post_0,
      " to ",
      ante_0,
      "</p>"
    )
  }

oceanIcons <- iconList(ship = makeIcon("shipwreck.png", iconWidth = 12, iconHeight = 18))


iconChooser <- function(selected_icon) {
  switch (plot_marker,
          "Mine Icon" = addMarkers(
            popup = ~ shipwreck_labeller(
              locname = locname,
              post_0 = post_0,
              ante_0 = ante_0,
              site_area = site_area
            ),
            icon = makeIcon(
              "shipwreck.png",
              iconWidth = 18,
              iconHeight = 18
            )
          ),
          "Circles" = addCircles(
            popup = shipwreck_labeller(
              locname = locname,
              post_0 = post_0,
              ante_0 = ante_0,
              site_area = site_area
            )
          ))
}

## =========================== shinyFunction ====================================
## ==============================================================================

shinyServer(function(input, output, session) {
  output$timeslider_UI <- renderUI({
    min_date <- min(shipwrecks_with_locations$ante_0)
    max_date <- max(shipwrecks_with_locations$post_0)
    
    sliderInput(
      "selected_time_period",
      label = "Selected Time Period",
      min = round_any(min_date, 50, f = floor),
      max = round_any(max_date, 50, f = ceiling),
      value = c(min_date, max_date),
      step = 50,
      width = "100%"
    )
  })
  
  output$shipwreck_map <- renderLeaflet({
    shipwreck_points <-
      shipwrecks_with_locations_spdf[shipwrecks_with_locations_spdf$ante_0 >= input$selected_time_period[1] &
                                       shipwrecks_with_locations_spdf$post_0 <= input$selected_time_period[2],]
    
    # map <-
    #   leaflet(data = shipwreck_points) %>% addProviderTiles(input$selected_map_tile)
    # map %>% addCircles(popup = ~ shipwreck_labeller(locname = locname, post_0 = post_0, ante_0 = ante_0, site_area = site_area))
    
    # map <-
    #   leaflet(data = shipwreck_points) %>% addProviderTiles(input$selected_map_tile)
    # map %>% addMarkers(
    #   popup = ~ shipwreck_labeller(
    #     locname = locname,
    #     post_0 = post_0,
    #     ante_0 = ante_0,
    #     site_area = site_area
    #   ),
    #   icon = makeIcon(
    #     "shipwreck.png",
    #     iconWidth = 18,
    #     iconHeight = 18
    #   )
    # )
    
    map <-
      leaflet(data = shipwreck_points, height = "100%") %>% addProviderTiles(input$selected_map_tile)
    
    switch(input$plot_marker,
           "Shipwreck Icon" = {
             map %>% addMarkers(
               popup = ~ shipwreck_labeller(
                 locname = locname,
                 post_0 = post_0,
                 ante_0 = ante_0,
                 site_area = site_area
               ),
               icon = makeIcon(
                 "shipwreck.png",
                 iconWidth = 18,
                 iconHeight = 18
               )
             )
           },
           "Circles" = {
             map %>% addCircles(
               popup = ~ shipwreck_labeller(
                 locname = locname,
                 post_0 = post_0,
                 ante_0 = ante_0,
                 site_area = site_area
               )
             )
           })

    
  })
  
})
