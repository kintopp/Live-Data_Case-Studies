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

## ================ Restricted Date Picker ============================
## ====================================================================
## From http://stackoverflow.com/a/32171132/1659890

mydateInput <-
  function(inputId,
           label,
           value = NULL,
           min = NULL,
           max = NULL,
           format = "yyyy-mm-dd",
           startview = "month",
           weekstart = 0,
           language = "en",
           minviewmode = "months",
           width = NULL) {
    # If value is a date object, convert it to a string with yyyy-mm-dd format
    # Same for min and max
    if (inherits(value, "Date"))
      value <- format(value, "%Y-%m-%d")
    if (inherits(min,   "Date"))
      min   <- format(min,   "%Y-%m-%d")
    if (inherits(max,   "Date"))
      max   <- format(max,   "%Y-%m-%d")
    
    htmltools::attachDependencies(
      tags$div(
        id = inputId,
        class = "shiny-date-input form-group shiny-input-container",
        style = if (!is.null(width))
          paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        tags$input(
          type = "text",
          # datepicker class necessary for dropdown to display correctly
          class = "form-control datepicker",
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-date-min-view-mode` = minviewmode,
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = value
        )
      ),
      datePickerDependency
    )
  }

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

datePickerDependency <- htmlDependency(
  "bootstrap-datepicker",
  "1.0.2",
  c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/datepicker.css"
)


## ====================================================================
## ===================== Shared Data ==================================

start.dates <-
  multiparty.interactions$DateOne.Year[!is.na(multiparty.interactions$DateOne.Year)]
start.dates <- start.dates[start.dates > 1000]

end.dates <-
  multiparty.interactions$DateOne.Year[!is.na(multiparty.interactions$DateOne.Year)]
end.dates <- end.dates[end.dates > 1000]


usefulCols_life_events <- colnames(life.events.df)

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
  if (is.null(input$visNetwork_selected_individual_show_timeslider)) {
    return()
  }
  
  if (is.null(input$select_individuals)) {
    return()
  }
  
  selected.interactions <- multiparty.interactions

  # ## Drop excluded categoties from multiparty interactions
  # selected.interactions <-
  #   selected.interactions[selected.interactions$Event.or.Relationship.Type != input$visNetwork_wholeNetwork_ExcludedCategory, ]
  
  if (is.null(input$visNetwork_selected_individual_show_timeslider)) {
  }
  

  if (input$visNetwork_selected_individual_show_timeslider == TRUE) {
    
    slider_date_list <-
      input$visNetwork_selected_individual_time_period_of_interest
    earliest_year <-
      year(input$visNetwork_selected_individual_time_period_of_interest[1])
    latest_year <-
      year(input$visNetwork_selected_individual_time_period_of_interest[2])
    
    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <-
      selected.interactions[{
        selected.interactions$DateOne.Year >= earliest_year
      } %in% TRUE &
      {
        selected.interactions$DateOne.Year <= latest_year
      } %in% TRUE , ]
    ## Filter out rows where DateTwo.Year is greater than the max date allowd
    selected.interactions <-
      selected.interactions[selected.interactions$DateTwo.Year <= latest_year |
                              is.na(selected.interactions$DateTwo.Year), ]
    
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

connections_to_selected_individual <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- filter_interactions()
  
  # Drop levels that are empty (as a result of above subsetting)
  selected.interactions <- droplevels(selected.interactions)
  
  # Append a column with the URLS

  selectedIndividual <-
    as.numeric(input$current_node_id$nodes[[1]])
  
  # Get edges of network
  edges <- visNetwork_wholeNetwork_edges()
  
  connectedIndividuals <-
    c(as.character(edges[edges$source.emlo.id == selectedIndividual, "target.emlo.id"]),
      as.character(edges[edges$target.emlo.id == selectedIndividual, "source.emlo.id"]))
  
  # Create an empty data.frame with life.event.columns
  connected_life_events <- selected.interactions[0,]
  # Function to extract connected events
  get.connected.life.events <-
    function(selectedNode, connectedNode) {
      connections <-
        rbind(selected.interactions[selected.interactions$Primary.Participant.Emlo_ID == selectedNode &
                                      selected.interactions$Secondary.Participant.Emlo_ID == connectedNode,],
              selected.interactions[selected.interactions$Primary.Participant.Emlo_ID == connectedNode &
                                      selected.interactions$Secondary.Participant.Emlo_ID == selectedNode,])
      connected_life_events <<-
        rbind(connected_life_events, connections)
    }
  # lapply function
  invisible(lapply(connectedIndividuals, function(x)
    get.connected.life.events(selectedIndividual, x)))
  # return
  connected_life_events
})