## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

## =========================== Control Elements ====================================
## =================================================================================

output$show_timeslider_UI <- renderUI({
  checkboxInput("show_timeslider",
                label = "Remove undated interactions and filter by date?",
                value = FALSE)
})


output$time_period_of_interest_UI <-
  renderUI({
    if (is.null(input$show_timeslider)) {
      return()
    }

    if (input$show_timeslider == TRUE) {
      dates <- place_of_birth$DoB
      dates <- dates[!is.na(dates)]

      sliderInput(
        "time_period_of_interest",
        "Time period of interest:",
        min = min(dates),
        max = max(dates),
        step = 1,
        value = c(min(dates), max(dates)),
        timeFormat = "%F"
      )
    }
  })

## =========================== Filter Entries by Date ===========================
## ==============================================================================

date_filtered_data <- reactive({
  if (is.null(input$show_timeslider)) {
    return(place_of_birth)
  }
  print("before date filter")

  if (input$show_timeslider == TRUE) {
    earliest_date <-
      input$time_period_of_interest[1]
    latest_date <-
      input$time_period_of_interest[2]

    ## Filter out rows where DateOne.Year is NA or outside of date range
    date_filtered_data <-
      place_of_birth[place_of_birth$DoB >= earliest_date &
                       place_of_birth$DoB <= latest_date, ]
  


    date_filtered_data
  } else
    place_of_birth

})

## =========================== Hover detail ====================================
## ==============================================================================

 output$marker_mouseover <- renderUI({
   HTML(paste0(
     "<p>",
     "marker mouseover lat:",
     input$irelandmap_marker_mouseover$lat,
     "</p>"
   ))
 })

 output$marker_mouseover <- renderUI({

   if(is.null(input$show_timeslider)){
     return()
   }

   date_filtered_data <- date_filtered_data()

   HTML(paste0(
     "<p>",
     "The current:",
     date_filtered_data[date_filtered_data$Latitude == input$irelandmap_marker_click$lat &
                          date_filtered_data$Longitude == input$irelandmap_marker_click$lng,]$PoB..Town.or.Parish.,
     "</p>"
   ))

  # HTML(paste0(
  #   "<p>",
  #   "The current:",
  #   input$irelandmap_marker_mouseover$lat,
  #   "</p>"
  # ))
 })


## =========================== Leaflet Map  ====================================
## ==============================================================================

output$irelandmap <- renderLeaflet({
  if (is.null(input$show_timeslider)) {
    return()
  }

  if (input$show_timeslider &
      is.null(input$time_period_of_interest)) {
    return()
  }

  date_filtered_data <- date_filtered_data()

  m <- leaflet(date_filtered_data) %>%
    addTiles() %>%
    setView(lat = 53.347778,
            lng = -6.259722,
            zoom = 3) %>%
    addCircleMarkers(
      lng = ~ Longitude,
      lat = ~ Latitude,
      radius = rescale(date_filtered_data$latlong.location.tally, to = c(5, 20)),
      popup = paste0(
        "Location Name: ",
        as.character(date_filtered_data$PoB..Town.or.Parish.),
        "<br/>",
        "Births at location: ",
        as.character(date_filtered_data$latlong.location.tally)
      )
    )
  m
})

output$marker_click <- renderDataTable({
  date_filtered_data <- date_filtered_data()

  selected_marker <-
    paste(input$irelandmap_marker_click$lat,
          input$irelandmap_marker_click$lng)

  date_filtered_data[date_filtered_data$Latitude == input$irelandmap_marker_click$lat &
                       date_filtered_data$Longitude == input$irelandmap_marker_click$lng, ]

  # HTML(paste0(
  #   "<p>",
  #   "marker click lat:",
  #   input$irelandmap_marker_click$lat,
  #   "</p>"
  # ))
})



# output$selected_circle <- renderUI({
#   # if (is.null(input$ireland_map_popup_click)) {
#   #   return()
#   # }
#   #
#   
#   HTML(input$irelandpopup_click)
# })