## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================


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
      
      # Remove an invalid dates
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
  
  visN_nodes <- visNetwork_wholeNetwork_nodes()
  
  HTML(
    paste0(
      "<p>Included Interactions: ",
      nrow(selected.interactions),
      "</p>",
      "<p>Included People/Organisations: ",
      length(multiparty.people),
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
    # label = HTML('<span class="glyphicon glyphicon-info-sign fa-5x" aria-hidden="true" font-size="50px"></span>'),
    # label = uiOutput("tooltip_highlight_node"),
    choices = c(list("None" = "None"), values.list),
    selected = "None",
    multiple = FALSE
  )
})

### ====================================== Generate Network Data =====================================
### ==================================================================================================

visNetwork_wholeNetwork_nodes <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- filter_interactions()
  
  ## Apply network.edges.function to selected.interactions
  edges <- network.edges.function(selected.interactions)
  ## Get nodes from edges
  nodes.of.network <-
    unique(c(edges$Primary.Emlo_ID, edges$Secondary.Emlo_ID))
  
  ## Only include individuals in the people.df data set
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  visNetwork_nodes <- data.frame(
    "Person.Name" = nodes$Person.Name,
    "Surname" = nodes$Surname,
    "emlo_id" = nodes$iperson_id,
    "color" = mapvalues(
      nodes$iperson_id %in% non_people_in_people_df,
      from = c(TRUE, FALSE),
      to = c("#a1d76a", "#7570b3")
    )
  )
  ## Return for use
  visNetwork_nodes
})

visNetwork_wholeNetwork_edges <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- filter_interactions()
  
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
      "total.connections" = edges$Total.Connections,
      ## Times the total number of connections by 10 and add 1 if of the highlighted category type
      ## Allows for testing off oddness for colour and size for the edge width
      "Value" =   if (input$visNetwork_wholeNetwork_highlightedCategory == "None") {
        20 * edges$Total.Connections
      } else {
        20 * edges$Total.Connections + 10 * edges[, c(input$visNetwork_wholeNetwork_highlightedCategory)]
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

edge_labeler <- function(from = NA,
                         to = NA,
                         connections = NA) {
  paste0(
    "<p>",
    "Connected Individuals:",
    "</p>",
    "<p>",
    people.df[people.df$iperson_id == from, "Person.Name"],
    " and ",
    people.df[people.df$iperson_id == to, "Person.Name"],
    "</p>",
    "<p>",
    "Total Connections:",
    connections,
    "</p>"
  )
}

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
    "width" = rescale(visNetwork_edges$Value, to = c(4, 12))
  )
  
  ## Drop duplicate node:
  visN_nodes <- visN_nodes[!duplicated(visN_nodes$id), ]
  
  ## Make background colour vector
  node.background.color <- rep("#d95f02", nrow(visN_nodes))
  ## Set highlighted.node to be #d95f02
  
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
    graph.data.frame(visN_edges[, c(1, 2)], visN_nodes[, 1], directed = FALSE)
  ## Simplify to remove self-loops
  igraph.for.computation <- simplify(igraph.for.computation)
  
  isolated_nodes <-
    V(igraph.for.computation)$name[which(degree(igraph.for.computation) == 0)]
  
  ## Drop isolated nodes
  visN_nodes <- visN_nodes[!visN_nodes$id %in% isolated_nodes,]
  
  ## Make network
  visNetwork(visN_nodes, visN_edges) %>%
    visNodes(color = list(border = "darkblue"), size = 10) %>%
    visIgraphLayout(randomSeed = 1) %>%
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
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
  
  })


observe({
  if (is.null(input$highlighted.node)) {
    return()
  }
  
  if (input$highlighted.node != "None") {
    visNetworkProxy("visNetwork_wholeNetwork") %>%
      visFocus(id = input$highlighted.node, scale = 1) %>%
      visUpdateNodes(nodes = data.frame("id" = input$highlighted.node,
                                        "color" = "red"))
  } else {
    visNetworkProxy("visNetwork_wholeNetwork") %>% visFit(nodes = NULL)
  }
  
})


### ====================================== Selected Individuals =====================================
### ==================================================================================================

output$visNetwork_wholeNetwork_selected_node_info <- renderUI({
  if (is.null(input$current_node_id)) {
    return()
  }
  
  selected.person.name <-
    people.df[people.df$iperson_id == as.numeric(input$current_node_id$nodes[[1]]), "Person.Name"]
  selected.person.name <-
    selected.person.name[!is.na(selected.person.name)]
  
  selected_emlo_id <-
    as.numeric(input$current_node_id$nodes[[1]])
  
  # Load connected individuals
  connected_life_events <- connections_to_selected_individual()

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
      trimws(selected.person.name),
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
        'connected_life_events_Cols',
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
        "select#connected_life_events_Cols + .selectize-control{width: 700px}"
      )
    ), width = 12))
  })

connected_individuals_events <- reactive({
  # Load connected individuals
  connected_life_events <- connections_to_selected_individual()
  if(is.null(connected_life_events)){
    return(NULL)
  }
  
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
    connected_life_events[, input$connected_life_events_Cols, drop = FALSE]
  # Replace "." with " " in colnames
  colnames(connected_life_events) <-
    gsub("[.]", " ", colnames(connected_life_events))
  # Convert factors to characters for sorting:
  factor_columns <- sapply(connected_life_events, is.factor)
  connected_life_events[factor_columns] <-
    lapply(connected_life_events[factor_columns], as.character)
  
  # Return to datatable
  connected_life_events
})

output$visNetwork_whole_network_selected_node <-
  renderDataTable({
    connected_individuals_events()
  }, escape = FALSE,
  # rownames = FALSE,
  # only make name column non-orderable
  options = if (length(which(
    colnames(connected_individuals_events()) %in% c("Primary Participant Name", "Secondary Participant Name")
  )) > 0) {
    list(columnDefs = list(list(
      targets = which(
        colnames(connected_individuals_events()) %in% c("Primary Participant Name", "Secondary Participant Name")
      ) - 1,
      orderable = FALSE
    )))
  })

is_simple_error <- function(x) inherits(x, "simpleError")


output$node_summary_DT_UI <- renderUI({
  
  if(!is.null(connected_individuals_events())){
    dataTableOutput("visNetwork_whole_network_selected_node")
  } else {
    wellPanel("The node you previously selected is no longer a member of the graph, your filtering has removed it. Please select a new node.")
  }

})
