library(igraph)
library(visNetwork)
library(plyr)
library(dplyr)
library(shiny)
library(ForceAtlas2)
library(igraph)
library(DT)

source("data-processing.R", local = T)

## =========================== igraph ===========================================
## ==============================================================================

ox_ox_igraph <-
  graph.data.frame(d = ox_ox_edges[, 1:2], vertices = ox_ox_nodes[, 1:2])
V(ox_ox_igraph)$title <- ox_ox_nodes$name


# ox_ox_depart_igraph <-
#   contract(ox_ox_igraph, mapping = as.numeric(
#     mapvalues(
#       ox_ox_nodes$department,
#       from = department_colours$department,
#       to = 1:nrow(department_colours)
#     )
#   ))
#
# V(ox_ox_depart_igraph)$name <- unique(ox_ox_nodes$department)
# V(ox_ox_depart_igraph)$title <- V(ox_ox_depart_igraph)$name
# V(ox_ox_depart_igraph)$color <-
#   mapvalues(
#     mapvalues(
#       unlist(V(ox_ox_depart_igraph)$name),
#       from = department_colours$department,
#       to = department_colours$colours
#     ),
#     from = department_colours$department,
#     to = department_colours$colours
#   )
#
# ox_ox_depart_igraph <- simplify(as.undirected(ox_ox_depart_igraph))

# 
# visIgraph(ox_ox_depart_igraph,
#           idToLabel = F,
#           layout = "layout_nicely")
# 

## =========================== Server Function ==================================
## ==============================================================================

shinyServer(function(input, output, sessions) {
  ## =========================== Invalidate Click Input ===========================
  ## ==============================================================================
  
  control_tracker <-
    reactiveValues(
      selected_node = 0,
      destructive_inputs = 0,
      both = 0,
      check = 1
    )
  
  observeEvent(c(input$people_or_departments), {
    control_tracker$destructive_inputs <-
      control_tracker$destructive_inputs + 1
  })
  
  observeEvent(c(input$people_or_departments, input$current_node_id), {
    control_tracker$both <- control_tracker$both + 1
  })
  
  observeEvent(c(input$current_node_id), {
    control_tracker$check <-
      list(control_tracker$destructive_inputs,
           control_tracker$both)
  })
  
  ## =========================== Generate Graph ====================================
  ## ==============================================================================
  
  
  ox_ox_igraph <- reactive({
    ox_ox_igraph <-
      graph.data.frame(d = ox_ox_edges[, 1:4], vertices = ox_ox_nodes[, 1:5])
    V(ox_ox_igraph)$title <- ox_ox_nodes$name
    V(ox_ox_igraph)$color <- ox_ox_nodes$color
    
    ox_ox_igraph
  })
  
  ox_ox_depart_igraph <- reactive({
    ox_ox_igraph <- ox_ox_igraph()
    
    ox_ox_depart_igraph <-
      contract(ox_ox_igraph, mapping = as.numeric(
        mapvalues(
          ox_ox_nodes$department,
          from = department_colours$department,
          to = 1:nrow(department_colours)
        )
      ))
    
    V(ox_ox_depart_igraph)$name <- unique(ox_ox_nodes$department)
    V(ox_ox_depart_igraph)$title <- V(ox_ox_depart_igraph)$name
    V(ox_ox_depart_igraph)$color <-
      mapvalues(unlist(V(ox_ox_depart_igraph)$name),
                from = department_colours$department,
                to = department_colours$colours)
    
    ox_ox_depart_igraph <-
      simplify(as.undirected(ox_ox_depart_igraph))
    ox_ox_depart_igraph
    
  })
  
  
  output$crukNetwork <- renderVisNetwork({
    if (is.null(input$layout_control)) {
      return()
    }
    
    switch(
      input$people_or_departments,
      "individuals" = {
        graph_to_plot <- ox_ox_igraph()
      },
      "departments" = {
        graph_to_plot <- ox_ox_depart_igraph()
      }
    )
    
    switch(
      input$layout_control,
      "layout_nicely" = {
        visIgraph(graph_to_plot,
                  idToLabel = F,
                  layout = "layout_nicely") %>%
          visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes);
                    ;}")
  },
  "layout_with_fr" = {
    #   visPhysics(
    #     solver = "forceAtlas2Based",
    #     forceAtlas2Based = list(avoidOverlap = 1),
    #     stabilization = list(iterations = 400)
    #   )
    visIgraph(graph_to_plot,
              idToLabel = F,
              layout = "layout_with_fr") %>%
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
  }
      )
    
      })
  
  individual_datatable <- reactive({
    selected_id <-
      ox_ox_nodes[ox_ox_nodes$name == input$current_node_id$nodes, "id"]
    subsetted_edges <-
      filter(ox_ox_edges, from == selected_id |
               to == selected_id)
    subsetted_edges$from <-
      mapvalues(subsetted_edges$from,
                from = ox_ox_nodes$id,
                to = ox_ox_nodes$name)
    subsetted_edges$to <-
      mapvalues(subsetted_edges$to,
                from = ox_ox_nodes$id,
                to = ox_ox_nodes$name)
    select(subsetted_edges,
           from,
           to,
           collaborations,
           publication.name,
           publication.date)
  })
  
  department_datatable <- reactive({
    filter(ox_ox_nodes, department == input$current_node_id$nodes) %>% select(id) %>% .[, 1] -> department_members
    
    subsetted_edges <-
      filter(ox_ox_edges,
             from %in% department_members |
               to %in% department_members)
    
    subsetted_edges$from <-
      mapvalues(subsetted_edges$from,
                from = ox_ox_nodes$id,
                to = ox_ox_nodes$name)
    subsetted_edges$to <-
      mapvalues(subsetted_edges$to,
                from = ox_ox_nodes$id,
                to = ox_ox_nodes$name)
    select(subsetted_edges,
           from,
           to,
           collaborations,
           publication.name,
           publication.date)
  })
  

  output$selected_node_table <- DT::renderDataTable({
    if (is.null(input$current_node_id$nodes)) {
      return()
    }
    
    if (control_tracker$destructive_inputs == 1) {
      if (is.null(input$current_node_id)) {
        return()
      } else {
        switch(
          input$people_or_departments,
          "individuals" = {
            individual_datatable()
          },
          "departments" = {
            department_datatable()
          }
        )
      }
      
    } else {
      if (control_tracker$destructive_inputs > control_tracker$check[1]) {
        return()
      } else {
        switch(
          input$people_or_departments,
          "individuals" = {
            individual_datatable()
          },
          "departments" = {
            department_datatable()
          }
        )
      }
    }

  })
  
  output$ui_containing_a_dt <- renderUI({

    if (is.null(input$current_node_id$nodes)) {
      wellPanel("Select a node for more details")
    }

    if (control_tracker$destructive_inputs == 1) {
      if (is.null(input$current_node_id)) {
        # return()
        wellPanel("Select a node for more details")
      } else {
        wellPanel(
        DT::dataTableOutput("selected_node_table")
        )
      }

    } else {
      if (control_tracker$destructive_inputs > control_tracker$check[1]) {
        # return()
        wellPanel("Select a node for more details")
      } else {
        wellPanel(
          DT::dataTableOutput("selected_node_table")
        )
      }
    }
  })
  
  })