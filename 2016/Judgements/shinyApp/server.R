library(shiny)
library(visNetwork)
library(igraph)
library(dplyr)
library(plyr)

source("data-processing.R",local = TRUE)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  ## Write a function that connects a case to each reference type that has greater than a cutoff number of references to each type of source
  references.per.court.fn <- function(court = NA, cutoff = 0){
    ## Delete argument assignment
    # court <- 2
    
    ## Find types that are referenced
    ## Logic for connection
    ## Count number of cases for each court that had more than n references of a particular type
    ## That count of cases is then used to decide how strongly connected the court is to that type of reference
    types.referenced.counts <- colSums(judgements.df[judgements.df$Court.ID == court,reference.type.names] > cutoff)
    
    types.referenced.names <- names(types.referenced.counts)[types.referenced.counts > 0]
    
    ## Where no references are made exit the function
    if(length(types.referenced.names) == 0){
      return()
    }
    
    ## Get node ids
    types.referenced.ids <- nodes.df[nodes.df$names %in% types.referenced.names,"id"]
    
    ## The actual case number is offset by the total number of reference types!
    ## See nodes.df definition for details
    court.node.id <- length(reference.type.names) + court
    
    ## Generate edge.list for the case
    edge.list <- vector()
    for (i in 1:length(types.referenced.ids)) {
      edge.list <- append(edge.list,types.referenced.ids[i])
      edge.list <- append(edge.list, court.node.id)
    }
    edge.list
  }
  
  make_references_per_court <- function(references.cutoff = 0){
    edges <- vector()
    ## Iterate through all cases:
    for(i in 1:nrow(courtids.df)){
      edges <- append(edges,references.per.court.fn(court = i, cutoff = references.cutoff))
    }
    edges
  }
  
  edges_for_graph <- reactive(make_references_per_court(input$minimum.references))
  
  output$judgement_bipartite_graph <- renderVisNetwork({
    
    all.edges <- edges_for_graph()
    
    ## Make bipartite_graph using igraph
    g <- make_bipartite_graph(
      ## This is the list of node types as enumerated through the list of all.edges:
      mapvalues(unique(all.edges), from = nodes.df$id, to = nodes.df$types),
      ## Map all.edges to be consecutive numbers
      mapvalues(all.edges, from = unique(all.edges), to = 1:length(unique(all.edges)))
    )
    # lay <- layout.bipartite(g, maxiter = 1000, hgap = 100, vgap = 100)
    # plot(g, layout=lay[,2:1],vertex.size = 5,vertex.label=V(g)$name)
    
    
    ## Add titles to the nodes
    V(g)$title <- nodes.df[mapvalues(as.vector(V(g)), to = unique(all.edges), from = 1:length(unique(all.edges))),]$names
    
    
    V(g)$label <- nodes.df[mapvalues(as.vector(V(g)), to = unique(all.edges), from = 1:length(unique(all.edges))),]$names
    
    ## Display graph
    visIgraph(g, idToLabel = FALSE) %>% visNodes(size = 10) %>%visIgraphLayout(layout = "layout.bipartite", type = "full")
    
  })

  
  
})
