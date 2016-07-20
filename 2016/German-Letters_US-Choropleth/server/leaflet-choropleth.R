## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================
## Data obtained from https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html at the 1:500,000 scale

## =========================== Select Shape Files ===============================
## ==============================================================================

output$selected_shapefiles_UI <-
  renderUI({
    selectInput(
      "selected_shapefiles",
      label = "Select Shape Files",
      choices = list(
        "States" = "contiguous_states",
        "Congressional Districts" = "contiguous_congressional_districts",
        "Counties" = "contiguous_counties"
      )
    )
  })

## =========================== Load shapefiles ====================================
## ==============================================================================
## Note shape file may be dependent on input$var and so is not included in data-processing.R

## Load county borders
contiguous_us_shapefiles <-
  reactive({
    full_us_shapefiles <- readOGR(
      dsn = "data/shapefiles/",
      layer = input$selected_shapefiles,
      verbose = F
    )
    full_us_shapefiles
  })
## Load FIPS codes
fips_codes <- read.csv("data/US-FIPS-Codes.csv")

contiguous_us_state_shapefiles <- {

  states_shapefiles <- readOGR(
    dsn = "data/shapefiles/",
    layer = "contiguous_states",
    verbose = F
  )
  states_shapefiles
}


## =========================== Label States in ====================================
## ==============================================================================



# contiguous_fips_codes <-
#   fips_codes[fips_codes$Contiguous.United.States. == "Y",]
# contiguous_us_shapefiles$State_Name <-
#   mapvalues(
#     contiguous_us_shapefiles$STATEFP,
#     from = contiguous_fips_codes$STATE,
#     to = contiguous_fips_codes$STATE_NAME
#   )

## =========================== Filter Letters ====================================
## ==============================================================================


unaddressed_filtered_letters <- reactive({
  if (is.null(input$show_letters_where_receive_unknown)) {
    return()
  }
  
  if (input$show_letters_where_receive_unknown) {
    letters_sent_from_usa
  } else {
    letters_sent_from_usa <-
      letters_sent_from_usa[letters_sent_from_usa$Sender.LatLong.String != "NA NA" &
                               letters_sent_from_usa$Receiver.LatLong.String != "NA NA",]
    
    letters_sent_from_usa
    
  }
  
})

letters_sent_between_dates <-
  function(start.year = NA,
           end.year = NA,
           data = NA) {
    if (input$show_letters_where_receive_unknown) {
      letters.for.analysis <- data
    } else {
      letters.for.analysis <-
        data[data$Sender.LatLong.String != "NA NA" &
               data$Receiver.LatLong.String != "NA NA",]
    }
    
    letters.for.analysis <- data
    if (input$show_timeslider == TRUE) {
      letters.for.analysis <-
        letters.for.analysis[!is.na(letters.for.analysis$Date),]
      
      letters.for.analysis$Date <- as.Date(letters.for.analysis$Date)
      
      
      
      
      letters.for.analysis <- subset(letters.for.analysis,
                                     Date >= start.year &
                                       Date <= end.year)
      
      letters.for.analysis
      
    } else
      letters.for.analysis
    
  }

## =========================== Convert Send Points  ====================================
## ==============================================================================

send_locations_as_spdf <- reactive({
  letters_for_analysis <-
    unaddressed_filtered_letters()
  
  letters_for_analysis <-
    letters_sent_between_dates(
      start.year = input$time_period_of_interest[1],
      end.year = input$time_period_of_interest[2],
      data = letters_for_analysis
    )
  
  just_send_points <-
    letters_for_analysis[, c("Sender.Location.GIS.Longitude",
                             "Sender.Location.GIS.Latitude")]
  colnames(just_send_points) <- c("longitude", "latitude")

  
  proj4_string <- contiguous_us_shapefiles()@proj4string
  
  send_locations_as_spdf <-
    SpatialPointsDataFrame(
      coords = just_send_points,
      data = just_send_points,
      proj4string = proj4_string
    )
  send_locations_as_spdf
  
})

polygons_with_tallies <- reactive({
  shapefiles <- contiguous_us_shapefiles()
  
  contiguous_counts <-
    poly.counts(pts = send_locations_as_spdf(), polys = shapefiles)
  contiguous_counts_df <- data.frame(contiguous_counts)
  shapefiles@data$Count.of.Send.Locations <-
    contiguous_counts_df$contiguous_counts
  # Return for use
  polygons_with_tallies <- shapefiles
  polygons_with_tallies
})

output$leaflet_choropleth <- renderLeaflet({
  if (is.null(input$show_letters_where_receive_unknown)) {
    return()
  }
  
  if (is.null(input$time_period_of_interest)) {
    return()
  }
  
  us_shape_files <- polygons_with_tallies()
  
  send_locations_as_spdf <- send_locations_as_spdf()
  
  polygon_counts <- us_shape_files$Count.of.Send.Locations
  
  palette <- colorBin(
    c("#cccccc", brewer.pal(5, "YlGnBu")),
    bins = c(0, 1, 5, 10, 20, 50, 350),
    pretty = FALSE,
    # na.color = "#cccccc",
    alpha = TRUE
  )
  
  region_labeller <- function(number_of_points = NA) {
    paste0(# "<p>", state_name, "</p>",
      "<p>Number of letters: ", number_of_points, "</p>")
  }
  
  map <- leaflet(data = us_shape_files) %>% addTiles()
  map <- map %>% addPolygons(
    stroke = TRUE,
    color = "#ffffff",
    smoothFactor = 0.2,
    fillOpacity = 0.8,
    fillColor = ~ palette(Count.of.Send.Locations),
    weight = 1,
    popup = ~ region_labeller(number_of_points = Count.of.Send.Locations)
    # popup = ~region_labeller(state_name = State_Name, number_of_points = var)
  )
  
  map %>% addLegend(
    position = 'topleft',
    ## choose bottomleft, bottomright, topleft or topright
    colors = c("#cccccc", brewer.pal(5, "YlGnBu")),
    labels = c("0", "1-5", "5-10", "10-20", "20-50", "50-350"),
    ## legend labels (only min and max)
    opacity = 0.6,
    ##transparency again
    title = "relative<br>amount"
  ) %>%   ## title of the legend
    addPolygons(
      data = contiguous_us_state_shapefiles,
      stroke = TRUE,
      color = "#000000",
      smoothFactor = 0.2,
      weight = 1,
      fill = FALSE 
    )
  
    
})
