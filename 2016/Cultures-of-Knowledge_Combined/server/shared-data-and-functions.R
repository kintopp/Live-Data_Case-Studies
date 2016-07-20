## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================

## ================ People With Connections ===========================
## ====================================================================

people_with_connections <- reactive({
  ## Only include people who are in the multiparty events!
  people.with.connections <-
    unique(
      c(
        multiparty.interactions$Primary.Participant.Emlo_ID,
        multiparty.interactions$Secondary.Participant.Emlo_ID
      )
    )
  
  people.with.connections <-
    subset(people.df, iperson_id %in% people.with.connections)
  
  # ## IF timeline enabled, filter out individuals who are do not appear in life events with DateOne.Year values
  # if (!is.null(input$visNetwork_selected_individual_show_timeslider)) {
  #   events.with.dates <-
  #     multiparty.interactions[!is.na(multiparty.interactions$DateOne.Year), ]
  #   people.with.dates <-
  #     unique(
  #       c(
  #         events.with.dates$Primary.Participant.Emlo_ID,
  #         events.with.dates$Secondary.Participant.Emlo_ID
  #       )
  #     )
  #   
  #   people.with.connections <-
  #     subset(people.with.connections,
  #            iperson_id %in% people.with.dates)
  #   
  # }
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <-
    as.list(unlist(as.character(people.with.connections$iperson_id)))
  names(values.list) <- labels.list
  # return object
  values.list
})


## ====================================================================
## ===================== Shared Data ==================================

start.dates <-
  multiparty.interactions$DateOne.Year[!is.na(multiparty.interactions$DateOne.Year)]
start.dates <- start.dates[start.dates > 1000]

end.dates <-
  multiparty.interactions$DateOne.Year[!is.na(multiparty.interactions$DateOne.Year)]
end.dates <- end.dates[end.dates > 1000]

usefulCols_life_events <-
  c(
    "Primary.Participant.Name",
    "Primary.Participant.Role",
    "Secondary.Participant.Name",
    "Secondary.Participant.Role",
    "Event.or.Relationship.Type",
    "Category",
    "DateOne.Year",
    "DateOne.Month",
    "DateOne.Day",
    "DateOne.Uncertainty",
    "DateTwo.Year",
    "DateTwo.Month",
    "DateTwo.Day",
    "DateTwo.Uncertainty",
    "Date.Type",
    "Location.Details",
    "Location.Region",
    "Location.Country",
    "Textual.Source.Source",
    "Primary.Participant.Emlo_ID",
    "Secondary.Participant.Emlo_ID",
    "Event.Name.or.Description",
    "Location.Name",
    "Location.Details",
    "Location.Type.Ahead",
    "Location.Region",
    "Location.Country",
    "Location.Type",
    "Who.Entered.Date",
    "Whose.Notes",
    "Additional.Notes"
  )

### ========= show/hide advanced options for the whole network

# shinyjs::onclick("toggleDateOptions",
#                  shinyjs::toggle(id = "dateOptions", anim = TRUE))

# shinyjs::onclick("toggleAdditionalOptions",
#                  shinyjs::toggle(id = "additionalOptions", anim = TRUE))

### ========= Exclude entries without a date

# output$include_interactions_without_dates_UI <- renderUI({
#   checkboxInput(inputId = "include_interactions_without_dates", label = "Include interactions without dates?", value = FALSE)
# })

### ========= Category/Type Selection UI Elements

all_event_types <-
  reactive(levels(multiparty.interactions$Event.or.Relationship.Type))
all_event_categories <-
  reactive(levels(multiparty.interactions$Category))

### ====================================== Filter By Date  ====================================================
### ===========================================================================================================

## Function for filtering interactions by date
filter_interactions <- reactive({
  if (is.null(input$visNetwork_wholeNetwork_ExcludedCategory)) {
    return()
  }
  
  selected.interactions <- multiparty.interactions
  #  Test suite
  #   visNetwork_wholeNetwork_ExcludedCategory <- "PeerRelationships"
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <-
    selected.interactions[selected.interactions$Event.or.Relationship.Type != input$visNetwork_wholeNetwork_ExcludedCategory, ]
  
  if (is.null(input$visNetwork_wholeNetwork_show_timeslider)) {
    return(selected.interactions)
  }
  
  if (input$visNetwork_wholeNetwork_show_timeslider == TRUE) {
    slider_date_list <-
      input$visNetwork_wholeNetwork_time_period_of_interest
    earliest_year <-
      year(input$visNetwork_wholeNetwork_time_period_of_interest[1])
    latest_year <-
      year(input$visNetwork_wholeNetwork_time_period_of_interest[2])
    
    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <-
      selected.interactions[{
        selected.interactions$DateOne.Year >= earliest_year
      } %in% TRUE &
      {
        selected.interactions$DateOne.Year <= latest_year
      } %in% TRUE , ]
    # ## Filter out rows where DateTwo.Year is greater than the max date allowd
    # selected.interactions <-
    #   selected.interactions[selected.interactions$DateTwo.Year <= latest_year |
    #                           is.na(selected.interactions$DateTwo.Year), ]
    ## Return data
    selected.interactions
  } else {
    selected.interactions
  }
})

### ====================================== Generate Edges Function ============================================
### ===========================================================================================================

network.edges.function <- function(selected.interactions) {
  edges.of.network <- data.frame(
    "Primary.Emlo_ID" = character(),
    "Secondary.Emlo_ID" = character(),
    "EcclesiasticalActivities" = character(),
    "EducationalActivities" = numeric(),
    "FamilyRelationships" = numeric(),
    "HierarchicalRelationships" = numeric(),
    "LearnedActivities" = numeric(),
    "MajorLifeEvents" = numeric(),
    "PeerRelationships" = numeric(),
    "PoliticalActivities" = numeric(),
    "ProfessionalActivities" = numeric(),
    "SocialStatusChange" = numeric()
  )
  # Find unique connections (relying on EMLO_ID)
  unique_Connections <-
    unique(selected.interactions[, c("Primary.Participant.Emlo_ID",
                                     "Secondary.Participant.Emlo_ID")])
  # Split into a list for easier usage in lapply operations later!
  unique_Connections <-
    split(unique_Connections, seq(nrow(unique_Connections)))
  
  
  tally.connections <- function(partners) {
    paired.events <-
      selected.interactions[selected.interactions$Primary.Participant.Emlo_ID == partners[[1]] &
                              selected.interactions$Secondary.Participant.Emlo_ID == partners[[2]], ]
    tally.event.type <-
      data.frame(data = factor(
        as.vector(paired.events[, "Category"]),
        levels = levels(selected.interactions$Category)
      ))
    tally.event.type <- as.data.frame(table(tally.event.type))
    totalConnections <- sum(tally.event.type$Freq)
    # Use dcast to make df wide
    tally.event.type <-
      dcast(
        tally.event.type,
        formula = . ~ tally.event.type ,
        value.var = "Freq",
        fill = NULL
      )
    # Drop the first column which contains useless data
    tally.event.type[1] <- NULL
    tally.event.type$Primary.Emlo_ID <- partners[[1]]
    tally.event.type$Secondary.Emlo_ID <- partners[[2]]
    tally.event.type$Total.Connections <- totalConnections
    tally.event.type
  }
  
  invisible(lapply(unique_Connections, function(x) {
    new.edge <- tally.connections(x)
    edges.of.network <<- rbind(edges.of.network, new.edge)
  }))
  
  # Remove edges where the secondary emlo id is NA
  edges.of.network <-
    edges.of.network[!is.na(edges.of.network$Secondary.Emlo_ID), ]
  
  # Return edges.of.network to the symbol whole.network_edges.of.network
  edges.of.network
}

### ====================================== Find Connections to Selected Individual ============================
### ===========================================================================================================

