## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================

## ================================================================================
## ===================== Whole Network Visualisation ==============================


### ====================================== UI Elements =====================================
### ========================================================================================


output$visNetwork_wholeNetwork_show_timeslider_UI <- renderUI({
  checkboxInput("visNetwork_wholeNetwork_show_timeslider",
                label = "Remove undated interactions and filter by date?",
                value = FALSE)
})


output$visNetwork_wholeNetwork_time_period_of_interest_UI <-
  renderUI({
    if (is.null(input$visNetwork_wholeNetwork_show_timeslider)) {
      return()
    }
    
    if (input$visNetwork_wholeNetwork_show_timeslider == TRUE) {
      dates <-
        c(multiparty.interactions$DateOne.Year,
          multiparty.interactions$DateTwo.Year)
      dates <- dates[!is.na(dates)]
      
      # Remove an incorrect date
      dates <- dates[dates > 1000]
      
      sliderInput(
        "visNetwork_wholeNetwork_time_period_of_interest",
        "Time period of interest:",
        min = as.POSIXct(paste0(min(dates) - 1, "-01-01")),
        max = as.POSIXct(paste0(max(dates), "-01-01")),
        step = 1,
        value = as.POSIXct(c(
          paste0(min(dates), "-01-01"), paste0(max(dates), "-01-01")
        )),
        timeFormat = "%Y"
      )
    }
  })

output$visNetwork_wholeNetwork_HighlightedCategoryUI <- renderUI({
  selectInput(
    'visNetwork_wholeNetwork_highlightedCategory',
    'Event/Relation Type to highlight',
    choices = c("None", all_event_categories()),
    selected = "None",
    multiple = FALSE
  )
})

output$visNetwork_wholeNetwork_ExcludedCategoriesUI <- renderUI({
  if (is.null(input$visNetwork_wholeNetwork_highlightedCategory)) {
    return()
  }
  
  selectInput(
    'visNetwork_wholeNetwork_ExcludedCategory',
    'Event/Relation Type to exclude',
    choices = c("None",
                all_event_types()),
    multiple = FALSE
  )
})

output$visNetwork_wholeNetwork_NumberOfExcluded <- renderUI({
  if (is.null(input$visNetwork_wholeNetwork_highlightedCategory)) {
    return()
  }
  
  if (is.null(input$visNetwork_wholeNetwork_ExcludedCategory)) {
    return()
  }
  
  ## if timeslider is to be shown but the controller variable is null do not return anything
  if (input$visNetwork_wholeNetwork_show_timeslider &
      is.null(input$visNetwork_wholeNetwork_time_period_of_interest)) {
    return()
  }
  
  selected.interactions <- filter_interactions()
  
  multiparty.people <-
    unique(
      c(
        multiparty.interactions$Primary.Participant.Emlo_ID,
        multiparty.interactions$Secondary.Participant.Emlo_ID
      )
    )
  
  selected.people <-
    unique(
      c(
        selected.interactions$Primary.Participant.Emlo_ID,
        selected.interactions$Secondary.Participant.Emlo_ID
      )
    )
  
  HTML(
    paste0(
      "<p>Included Interactions: ",
      nrow(selected.interactions),
      "</p>",
      "<p>Included People/Organisations: ",
      length(selected.people),
      "</p>",
      "<p>Excluded Interactions: ",
      nrow(multiparty.interactions) - nrow(selected.interactions),
      "</p>",
      "<p>Excluded People/Organisations: ",
      length(multiparty.people) - length(selected.people),
      "</p>"
    )
  )
})

output$visNetwork_wholeNetwork_highlighted_node_UI <- renderUI({
  ## If not loaded yet, stop
  if (is.null(input$visNetwork_wholeNetwork_show_timeslider)) {
    return()
  }
  
  if (is.null(input$visNetwork_wholeNetwork_highlightedCategory)) {
    return()
  }
  
  if (is.null(input$visNetwork_wholeNetwork_ExcludedCategory)) {
    return()
  }
  ## if timeslider is to be shown but the controller variable is null do not return anything
  if (input$visNetwork_wholeNetwork_show_timeslider &
      is.null(input$visNetwork_wholeNetwork_time_period_of_interest)) {
    return()
  }
  
  edges <- visNetwork_wholeNetwork_edges()
  
  if (is.null(edges)) {
    return()
  }
  
  visNetwork_nodes <- visNetwork_wholeNetwork_nodes()
  
  labels.list <- as.character(visNetwork_nodes$Person.Name)
  values.list <-
    as.list(unlist(as.character(visNetwork_nodes$emlo_id)))
  
  names(values.list) <- labels.list
  
  selectInput(
    "highlighted.node",
    label = "Highlight node",
    choices = values.list,
    selected = as.character(values.list[1]),
    multiple = FALSE
  )
})

### ====================================== Filter By Date  ====================================================
### ===========================================================================================================

## Function for filtering interactions by date
whole_network_filter_interactions <- reactive({
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

### ====================================== Connected Individuals ============================================
### =========================================================================================================


whole_network_connections_to_selected_individual <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- filter_interactions()
  
  # Drop levels that are empty (as a result of above subsetting)
  selected.interactions <- droplevels(selected.interactions)
  
  # Append a column with the URLS
  
  selectedIndividual <-
    as.numeric(input$current_node_id_whole_network$nodes[[1]])
  
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
  print(paste0("shared connections_to_selected_individual - nrow: ",nrow(connected_life_events)))
  connected_life_events
})

### ====================================== Generate Network Data =====================================
### ==================================================================================================

visNetwork_wholeNetwork_nodes <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- whole_network_filter_interactions()
  
  ## Apply network.edges.function to selected.interactions
  edges <- network.edges.function(selected.interactions)
  ## Get nodes from edges
  nodes.of.network <-
    unique(c(edges$Primary.Emlo_ID, edges$Secondary.Emlo_ID))
  
  ### ==== Start Experiment
  
  test_nodes_in_people.df <-
    subset(people.df, iperson_id %in% nodes.of.network)$iperson_id
  test_nodes_not_in_people.df <-
    setdiff(nodes.of.network, test_nodes_in_people.df)
  
  ### ==== End Experiment
  
  ## Only include individuals in the people.df data set
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  visNetwork_nodes <- data.frame(
    "Person.Name" = nodes$Person.Name,
    "Surname" = nodes$Surname,
    "emlo_id" = nodes$iperson_id,
    "color" = mapvalues(nodes$iperson_id %in% non_people_in_people_df, from = c(TRUE, FALSE), to = c("#a1d76a","#7570b3"))
  )
  ## Return for use
  
  visNetwork_nodes
})

visNetwork_wholeNetwork_edges <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- whole_network_filter_interactions()
  
  ## Apply network.edges.function to selected.interactions
  edges <- network.edges.function(selected.interactions)
  ## Get nodes from edges
  nodes.of.network <-
    unique(c(edges$Primary.Emlo_ID, edges$Secondary.Emlo_ID))
  
  ## Subset people.df by nodes in edges
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  source_vector <- as.numeric(
    mapvalues(
      edges$Primary.Emlo_ID,
      from = nodes$iperson_id,
      to = 0:(nrow(nodes) - 1),
      warn_missing = FALSE
    )
  )
  
  target_vector <- as.numeric(
    mapvalues(
      edges$Secondary.Emlo_ID,
      from = nodes$iperson_id,
      to = 0:(nrow(nodes) -
                1),
      warn_missing = FALSE
    )
  )
  
  visNetwork_edges <-
    data.frame(
      "source" = source_vector,
      "target" = target_vector,
      "source.emlo.id" = as.numeric(edges$Primary.Emlo_ID),
      "target.emlo.id" = as.numeric(edges$Secondary.Emlo_ID),
      ## Times the total number of connections by 10 and add 1 if of the highlighted category type
      ## Allows for testing off oddness for colour and size for the edge width
      "Value" =   if (input$visNetwork_wholeNetwork_highlightedCategory == "None") {
        20 * edges$Total.Connections
      } else {
        20 * edges$Total.Connections + edges[, c(input$visNetwork_wholeNetwork_highlightedCategory)]
      },
      "EdgeColor" = if (input$visNetwork_wholeNetwork_highlightedCategory == "None") {
        rep("#7570b3", nrow(edges))
      } else {
        mapvalues(edges[, c(input$visNetwork_wholeNetwork_highlightedCategory)] > 0, c(TRUE, FALSE), c("#d95f02", "#7570b3"))
      }
    )
  ## return for use
  
  
  visNetwork_edges
})


### ====================================== Visualise Entire Network ========================
### ========================================================================================

## show warning if no edges to display
output$whole.network_no_graph <- renderUI({
  ## If not loaded yet, stop
  if (is.null(input$visNetwork_wholeNetwork_ExcludedCategory)) {
    return()
  }
  
  visN_edges <- visNetwork_wholeNetwork_edges()
  ## If graph.union.fail then the visN_edges is null
  if (is.null(visN_edges)) {
    wellPanel(
      "There are no known interactions between individuals in the dataset, subject to the current filter conditions."
    )
  }
})


output$visNetwork_wholeNetwork <- renderVisNetwork({
  ## If not loaded yet, stop
  
  if (is.null(input$visNetwork_wholeNetwork_show_timeslider)) {
    return()
  }
  
  if (is.null(input$visNetwork_wholeNetwork_highlightedCategory)) {
    return()
  }
  
  if (is.null(input$visNetwork_wholeNetwork_ExcludedCategory)) {
    return()
  }
  ## if timeslider is to be shown but the controller variable is null do not return anything
  if (input$visNetwork_wholeNetwork_show_timeslider &
      is.null(input$visNetwork_wholeNetwork_time_period_of_interest)) {
    return()
  }
  
  if(is.null(input$highlighted.node)){
    return()
  }
  
  visNetwork_edges <- visNetwork_wholeNetwork_edges()
  visNetwork_nodes <- visNetwork_wholeNetwork_nodes()
  
  ## Create df for visNetwork
  visN_nodes <- data.frame(
    "id" = visNetwork_nodes$emlo_id,
    "title" = as.character(visNetwork_nodes$Person.Name),
    "label" = as.character(visNetwork_nodes$Surname),
    "color" = as.character(visNetwork_nodes$color)
  )
  
  visN_nodes$color <- as.character(visN_nodes$color)
  
  visN_edges <- data.frame(
    "from" = visNetwork_edges$source.emlo.id,
    "to" = visNetwork_edges$target.emlo.id,
    "color" = visNetwork_edges$EdgeColor,
    "value" = rescale(visNetwork_edges$Value, to = c(2, 10))
  )
  
  ## =========================== edge label dump: remove ====================================
  ## ==============================================================================
  
  # "title" = HTML(paste0(
  #   "<p>","Connected Individuals:","</p>",
  #   "<p>", people.df[people.df$iperson_id == as.numeric(edges$Primary.Emlo_ID), "Person.Name"],
  #   " and ",
  #   people.df[people.df$iperson_id == as.numeric(edges$Primary.Emlo_ID), "Person.Name"],
  #   "</p>",
  #   "<p>","Total Connections:",
  #   edges$Total.Connections,"</p>"
  # )),
  # "title" = paste0(
  #   "<p>","Connected Individuals:","</p>",
  #   "<p>", people.df[people.df$iperson_id == as.numeric(edges$Primary.Emlo_ID), "Person.Name"] %>% as.vector(),
  #   " and ",
  #   people.df[people.df$iperson_id == as.numeric(edges$Secondary.Emlo_ID), "Person.Name"] %>% as.vector(),
  #   "</p>",
  #   "<p>","Total Connections:",
  #   edges$Total.Connections,"</p>"
  # )
  
  # "title" = paste("foo",people.df[people.df$iperson_id == edges$Primary.Emlo_ID, "Person.Name"])
  
  ## =========================== end ====================================
  ## ==============================================================================
  
  ## Drop duplicate node:
  visN_nodes <- visN_nodes[!duplicated(visN_nodes$id), ]
  
  visN_nodes[visN_nodes$id == input$highlighted.node, ]$color <-
    "#d95f02"
  
  ## Make background colour vector
  node.background.color <- rep("#d95f02", nrow(visN_nodes))
  ## Set highlighted.node to be #d95f02
  node.background.color[visN_nodes$id == input$highlighted.node] <-
    "#d95f02"
  
  ## Drop edges with nodes not in the node list
  non.conflicting.nodes <-
    intersect(unique(c(visN_edges$from, visN_edges$to)), visN_nodes$id)
  visN_edges <-
    subset(visN_edges,
           from %in% non.conflicting.nodes &
             to %in% non.conflicting.nodes)
  
  ## =========== Use igraph to remove loops and nodes with edge degree == 0 ===========
  ## ==================================================================================
  ## Generate igraph
  igraph.for.computation <-
    graph.data.frame(visN_edges[,c(1,2)], visN_nodes[,1], directed = FALSE)
  ## Simplify to remove self-loops
  igraph.for.computation <- simplify(igraph.for.computation)
  
  isolated_nodes <- V(igraph.for.computation)$name[which(degree(igraph.for.computation) == 0)]
  
  ## Drop isolated nodes
  visN_nodes <- visN_nodes[!visN_nodes$id %in% isolated_nodes,]
  
  ## Make network
  visNetwork(visN_nodes, visN_edges) %>%
    visNodes(color = list(border = "darkblue"), size = 10) %>%
    visIgraphLayout(randomSeed = 1) %>%
    # visEdges(value = round(rescale(visNetwork_edges$Value, to = c(2,10)))) %>%
    # visEdges(width = 4) %>%
    visInteraction(
      tooltipDelay = 0.2,
      hideEdgesOnDrag = FALSE,
      dragNodes = FALSE,
      dragView = TRUE,
      zoomView = TRUE
    ) %>%
    visOptions(highlightNearest = TRUE) %>% visLayout(hierarchical = FALSE) %>%
    # visInteraction(navigationButtons = TRUE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id_whole_network', nodes);
              ;}")
  
  })

### ====================================== Selected Individuals =====================================
### ==================================================================================================

output$visNetwork_wholeNetwork_selected_node_info <- renderUI({
  if (is.null(input$current_node_id_whole_network)) {
    return()
  }
  
  selected.person.name <-
    people.df[people.df$iperson_id == as.numeric(input$current_node_id_whole_network$nodes[[1]]), "Person.Name"]
  selected.person.name <-
    selected.person.name[!is.na(selected.person.name)]
  
  selected_emlo_id <-
    as.numeric(input$current_node_id_whole_network$nodes[[1]])
  
  # Load connected individuals
  connected_life_events <- whole_network_connections_to_selected_individual()
  
  wellPanel(HTML(
    paste0(
      "<p><strong>Selected Person/Organisation: ",
      "<a target='_blank' href=http://emlo.bodleian.ox.ac.uk/profile?type=person&id=",
      selected_emlo_id,
      ">",
      selected.person.name,
      "</a></strong></p>",
      "<p>Number of Unique Connections: ",
      length(setdiff(unique(
        c(
          connected_life_events$Primary.Participant.Emlo_ID,
          connected_life_events$Secondary.Participant.Emlo_ID
        )
      ), selected_emlo_id)),
      "</p>",
      "<p>Scroll down for more information about ",
      selected.person.name,
      "'s connections",
      "</p>",
      sep = ""
    )
  ))
  
  
})

output$visNetwork_whole_network_connected_life_events_columns_to_show_UI <-
  renderUI({
    fluidRow(column(tagList(
      selectInput(
        'whole_network_connected_life_events_Cols',
        'Columns to show:',
        usefulCols_life_events,
        selected = c(
          "Category",
          "Event.or.Relationship.Type",
          "Primary.Participant.Name",
          "Primary.Participant.Role",
          "Secondary.Participant.Name",
          "Secondary.Participant.Role",
          "DateOne.Year",
          "DateTwo.Year",
          "Date.Type",
          "Location.Type.Ahead"
        ),
        multiple = TRUE
      ),
      tags$style(
        type = "text/css",
        "select#whole_network_connected_life_events_Cols + .selectize-control{width: 700px}"
      )
    ), width = 12))
  })

connected_individuals_events <- reactive({
  if (!is.igraph(visNetwork_select_individuals_neighboring_nodes())) {
    return()
  }
  
  # Load connected individuals
  connected_life_events <- whole_network_connections_to_selected_individual()
  
  connected_life_events$Primary.Participant.Name <-
    paste0(
      "<a target='_blank' href=http://emlo.bodleian.ox.ac.uk/profile?type=person&id=",
      connected_life_events$Primary.Participant.Emlo_ID,
      ">",
      connected_life_events$Primary.Participant.Name,
      "</a>"
    )
  
  connected_life_events$Secondary.Participant.Name <-
    paste0(
      "<a target='_blank' href=http://emlo.bodleian.ox.ac.uk/profile?type=person&id=",
      connected_life_events$Secondary.Participant.Emlo_ID,
      ">",
      connected_life_events$Secondary.Participant.Name,
      "</a>"
    )
  
  # Drop empty rows:
  connected_life_events <-
    connected_life_events[!!rowSums(!is.na(connected_life_events)), ]
  # Return only selected columns
  connected_life_events <-
    connected_life_events[, input$whole_network_connected_life_events_Cols, drop = FALSE]
  # Replace "." with " " in colnames
  colnames(connected_life_events) <-
    gsub("[.]", " ", colnames(connected_life_events))
  # Convert factors to characters for sorting:
  factor_columns <- sapply(connected_life_events, is.factor)
  connected_life_events[factor_columns] <-
    lapply(connected_life_events[factor_columns], as.character)
  
  # Find position of names to make these columns non-orderable
  name_columns <-
    which(
      colnames(connected_life_events) %in% c("Primary Participant Name", "Secondary Participant Name")
    )
  
  # Return to datatable
  connected_life_events
})

output$visNetwork_whole_network_selected_node <-
  renderDataTable({
    connected_individuals_events()
    
  }, escape = FALSE,
  # rownames = FALSE,
  # filter = list(position = 'top', clear = FALSE),
  # only make name column non-orderable
  options = if (length(which(
    colnames(connected_individuals_events()) %in% c("Primary Participant Name", "Secondary Participant Name")
  )) > 0) {
    list(
      dom = 'ft',
      columnDefs = list(list(
        targets = which(
          colnames(connected_individuals_events()) %in% c("Primary Participant Name", "Secondary Participant Name")
        ) - 1,
        orderable = FALSE
      )),
      search = list(
        regex = TRUE,
        caseInsensitive = FALSE
      )
    )
  })
