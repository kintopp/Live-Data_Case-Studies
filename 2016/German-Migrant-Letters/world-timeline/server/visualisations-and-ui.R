## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

### ============= Useful Visualisations Tools ========================= ###

## ggplot Color Function from http://stackoverflow.com/a/8197703/1659890
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

### ============= Main Map Section ========================= ###


output$time_period_of_interest_UI <- renderUI({
  
  dates <- letters.for.analysis()$Date[!is.na(letters.for.analysis()$Date)]
  
  if(is.null(input$show_timeslider)){
    return()
  }
  
  
  if(input$show_timeslider == TRUE){
    sliderInput(
      "time_period_of_interest", "Time period of interest:",
      min = min(dates),
      max = max(dates),
      step = 1,
      value = c(min(dates), max(dates)),
      width = "800px",
      timeFormat = "%F"
    )
  }

})

output$show_timeslider_UI <- renderUI({
  checkboxInput("show_timeslider", label = "Filter letters by date sent?", value = TRUE)
})

output$show_letters_where_receive_unknown_UI <- renderUI({
  checkboxInput("show_letters_where_receive_unknown", label = "Include letters where receive location unknown?", value = TRUE)
})

## Decide whether letters without receive are shown:

letters.for.analysis <- reactive({
  
  if(is.null(input$show_letters_where_receive_unknown)){
    return()
  }
  
  if(input$show_letters_where_receive_unknown){
    entries.with.locations
  } else {
    
    entries.with.locations <- entries.with.locations[entries.with.locations$Sender.LatLong.String != "NA NA" &
                                                       entries.with.locations$Receiver.LatLong.String != "NA NA",]
  
    entries.with.locations
  }
  
})

## change size of output map

output$map_size_numeric <- renderText({
  
  if(is.null(input$show_letters_where_receive_unknown)){
    "1400px"
  }
  
  if(input$show_letters_where_receive_unknown){
    "1400px"
  } else {
    "1000px"
  }
  
})


### ==== Location Tallies

location_tallies <- reactive({
  
  if(input$show_timeslider == TRUE){
    subset_entries <- subset(letters.for.analysis(),
                             Date >= as.POSIXct(paste0(input$time_period_of_interest[1],"/01/01")) &
                               Date <= as.POSIXct(paste0(input$time_period_of_interest[2],"/12/31")))
  } else {
    subset_entries <- letters.for.analysis()
  }

  
  all_locations <- unique(c(subset_entries$Sender.LatLong.String, subset_entries$Receiver.LatLong.String))
  ## Drop NA
  all_locations <- all_locations[all_locations != "NA NA"]
  
  ## sent location tallies
  sent_tallies <- table(subset_entries$Sender.LatLong.String)
  sent_tallies <- as.data.frame(sent_tallies)
  ## use mapvalues to replace names with tallies
  sent_tallies_vec <- mapvalues(all_locations, from = sent_tallies$Var1, to = sent_tallies$Freq)
  ## use string length > 10 to send latlog.strings to 0
  sent_tallies_vec[nchar(sent_tallies_vec) > 10] <- 0
  ## convert to numeric:
  sent_tallies_vec <- as.numeric(sent_tallies_vec)

  ## receive location tallies
  receive_tallies <- table(subset_entries$Receiver.LatLong.String)
  receive_tallies <- as.data.frame(receive_tallies)
  ## use mapvalues to replace names with tallies
  receive_tallies_vec <- mapvalues(all_locations, from = receive_tallies$Var1, to = receive_tallies$Freq)
  ## use string length > 10 to send latlog.strings to 0
  receive_tallies_vec[nchar(receive_tallies_vec) > 10] <- 0
  ## convert to numeric:
  receive_tallies_vec <- as.numeric(receive_tallies_vec)
  
  
  lat_vec <- sapply(strsplit(all_locations, " "), "[[", 1)
  
  lon_vec <- sapply(strsplit(all_locations, " "), "[[", 2)
  
  ### Get location names
  
  location_name_vec <- as.character()
  look.up.location <- function(lat_long){
    name <- location_name_df[location_name_df$LatLong == lat_long,"Location.Name"]
    name <- unique(as.character(name))[1]
    location_name_vec <<- append(location_name_vec, name)
  }
  
  invisible(lapply(all_locations, function(x) look.up.location(x)))
  
  good.names <- data.frame("name" = location_name_vec, "latlong" = all_locations)

  
  # Spit into lat and long for plotting
  location_tallies <- data.frame("lat" = lat_vec,
                                 "lon" = lon_vec,
                                 "Letters.Sent" = sent_tallies_vec,
                                 "Letters.Received" = receive_tallies_vec,
                                 "Name" = good.names$name)
  
  
  
  ## =====  Get letter series for each location - as a send location
  ## create empty vector
  letter.series.per.location <- as.character()
  
  get.letter.series.for.location <- function(location){
    
    send <- paste(location$lat, location$lon)
    
    entries <- letters.for.analysis()[letters.for.analysis()$Sender.LatLong.String == send, ]
    letter.series.for.route <- as.character(entries$Letter.Series)
    
    if (nrow(entries) == 0) {
      letter.series.per.location <<-
        append(letter.series.per.location, "none sent")
    } else {
      if (length(unique(letter.series.for.route)) > 1) {
        
#         switch (input$legend_type,
#           "Location" = letter.series.per.location <<-
#             append(letter.series.per.location, "multiple series"),
#           "Letter Series" = letter.series.per.location <<-
#             append(letter.series.per.location, paste0(unique(letter.series.for.route), collapse = "", sep = "<br>"))
#         )
        
        joined.strings <- paste0(unique(letter.series.for.route), collapse = "", sep = "<br>")
        
        joined.strings <- strtrim(joined.strings,nchar(joined.strings)-4)
        
        letter.series.per.location <<-
          append(letter.series.per.location, joined.strings)
        
      } else {
        letter.series.per.location <<-
          append(letter.series.per.location, unique(letter.series.for.route))
      }
    }
  }
  ## populate letter.series.per.location vector
  for (i in 1:nrow(location_tallies)) {
    get.letter.series.for.location(location_tallies[i,])
  }
  
  ## Add letter.series.per.location to location_tallies
  location_tallies$Letter.Series <-letter.series.per.location
  
  ## Order dataframe by letter series
  location_tallies <- location_tallies[order(location_tallies$Letter.Series),]

  # Return object
  location_tallies
})

### ===== Route Tallies

route_tallies <- reactive({
  
  if(input$show_timeslider == TRUE){
    subset_entries <- subset(letters.for.analysis(),
                             Date >= as.POSIXct(paste0(input$time_period_of_interest[1],"/01/01")) &
                               Date <= as.POSIXct(paste0(input$time_period_of_interest[2],"/12/31")))
  } else {
    subset_entries <- letters.for.analysis()
  }

  ## Drop routes where latlong strings are "NA NA"
  subset_entries <- subset_entries[subset_entries$Receiver.LatLong.String != "NA NA" & 
                                     subset_entries$Sender.LatLong.String != "NA NA",]
  
  send_receive_pairs <- data.frame("send" = subset_entries$Sender.LatLong.String,
                                   "receive" = subset_entries$Receiver.LatLong.String, stringsAsFactors = FALSE)
  unique_routes <- send_receive_pairs[!duplicated(apply(send_receive_pairs,1,function(x) paste(sort(x),collapse=''))),]

  
  ## ==== Route Tallies
  
  route_tallies <- table(paste(send_receive_pairs$send,send_receive_pairs$receive))
  route_tallies <- as.data.frame(route_tallies)
  # as.character(var1) for string splitting
  route_tallies$Var1 <- as.character(route_tallies$Var1)

  ### ============= Split route tallies back to send/receive locations
  route_tallies <- data.frame("send.lat" = sapply(strsplit(route_tallies$Var1, " "), "[[", 1),
                              "send.lon" = sapply(strsplit(route_tallies$Var1, " "), "[[", 2),
                              "receive.lat" = sapply(strsplit(route_tallies$Var1, " "), "[[", 3),
                              "receive.lon" = sapply(strsplit(route_tallies$Var1, " "), "[[", 4),
                              "Freq" = route_tallies$Freq)
  
  ## =====  Get letter series for each route
  
  ## create empty vector
  letter.series.per.route <- as.character()
  
  get.letter.series <- function(route){
    
    send <- paste(route$send.lat, route$send.lon)
    receive <- paste(route$receive.lat, route$receive.lon)
    
    
    entries <<- letters.for.analysis()[letters.for.analysis()$Sender.LatLong.String == send &
                                         letters.for.analysis()$Receiver.LatLong.String == receive, ]
    letter.series.for.route <- as.character(entries$Letter.Series)
    
    if(length(unique(letter.series.for.route)) > 1){
      letter.series.per.route <<- append(letter.series.per.route, "multiple series")
    } else {
      letter.series.per.route <<- append(letter.series.per.route, unique(letter.series.for.route))
    }
  }
  
  ## populate letter.series.per.route vector
  for (i in 1:nrow(route_tallies)) {
    get.letter.series(route_tallies[i,])
  }
  ## Add letter.series.per.route to route_tallies
  route_tallies$Letter.Series <-letter.series.per.route
  ## A unique id is required per trace
  route_tallies$ID <- 1:nrow(route_tallies)

  # as.numeric for plotting
  route_tallies$send.lat <- as.numeric(as.character(route_tallies$send.lat))
  route_tallies$send.lon <- as.numeric(as.character(route_tallies$send.lon))
  route_tallies$receive.lat <- as.numeric(as.character(route_tallies$receive.lat))
  route_tallies$receive.lon <- as.numeric(as.character(route_tallies$receive.lon))
  
  # Return object
  route_tallies
})


output$world_map <- renderPlotly({
  
  
  if (is.null(input$show_timeslider)) {
    return()
  }
  
  if (input$show_timeslider &
      is.null(input$time_period_of_interest)) {
    return()
  }
  
  route_tallies <- route_tallies()
  location_tallies <- location_tallies()
  
  geo_layout <- list(
    scope = "world",
    showland = TRUE,
    landcolor = toRGB("gray85"),
    #   subunitwidth = 1,
    #   countrywidth = 1,
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white"),
    showlakes = TRUE,
    lakecolor = "#999999"
  )
  
  
  ## locations first
  plot_ly(
    location_tallies, lon = lon, lat = lat, marker = list(size = rescale(
      Letters.Sent + Letters.Received, to = c(7,20)
    )),
    type = "scattergeo", locationmode = "country",
    text = paste0(
      "Location Name: ",Name,"<br>",
      "Letters sent from location: ",Letters.Sent,"<br>",
      "Letters received at location: ",Letters.Received,"<br>",
      "Letter Series: ",Letter.Series
    ),
    hoverinfo = "text",inherit = FALSE,
    group = Letter.Series, showlegend = TRUE
  ) %>%
    add_trace(
      data = route_tallies, lon = list(send.lon, receive.lon),
      lat = list(send.lat, receive.lat),
      # text = paste0("Messages addressed to/from location: ",Freq,"<br>"),
      type = 'scattergeo',
      # locationmode = 'country names', # Used for identifying places
      inherit = FALSE,
      group = ID,
      # hoverinfo = "text",
      mode = 'lines',
      line = list(
        width = 0.4, color = "#6db8de", opacity = 1
      ),
      marker = list(size = rescale(Freq, to = c(7,20)), opacity = 0.4), showlegend = FALSE
    ) %>%
    layout(
      #          title = "The ‘New’ Germans: Rethinking Integration by understanding the <br>
      #          Historical Experience of German Migrants in the US",
      geo = geo_layout,
      legend = list(x = 1.1, y = 0.5),
      legend = list(xanchor = "right",
                    yanchor = "top")
    )
})

scaled_height <- reactive({
  ## Get location_tallies to scale height of output
  location_tallies <- location_tallies()
  
  # print(length(unique(location_tallies$Letter.Series)))
  
  ## Get number of unique letter series
  unique_letter_series <- unique(location_tallies$Letter.Series)
  
  ## Add number of <br> to calculate number of lines in legend
  lines_in_legend <-
    sum(grepl("<br>",unique(unique_letter_series))) + length(unique_letter_series)
  
  ## There are a maximum of 61 letter series combinations which fit well with height 1200px
  ## Minimum height of 400 is sensible
  if(lines_in_legend * 20 > 400){
    scaled_height <- paste0(20 * lines_in_legend + 40,"px")
  } else {
    scaled_height <- 400
  }
  
  scaled_height
})

output$worldmap_via_renderUI <- renderUI({
  if(is.null(input$time_period_of_interest)){
    return()
  }
  
  plotlyOutput("world_map", width = "100%", height = scaled_height())
})

