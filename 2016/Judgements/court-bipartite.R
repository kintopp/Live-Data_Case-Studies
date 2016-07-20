##### ============= Import =======================================
##### ============================================================

judgements.df <- read.csv(file = "judgements.csv", header = TRUE, sep = ",")
caseids.df <- read.csv(file = "caseids.csv", header = TRUE, sep = ",")
courtids.df <- read.csv(file = "courtids.csv", header = TRUE, sep = ",")
## Remove the X column in judgements
judgements.df <- judgements.df[,!colnames(judgements.df) %in% "X"]
## Replace NA with 0
judgements.df[is.na(judgements.df)] <- 0
## Drop row where id == 0
judgements.df <- judgements.df[judgements.df$Case.ID != 0,]

## Fix column names

## Get colnames with "..." in so as to find the different reference types:
reference.types <- colnames(judgements.df)[grepl("\\.{3}", colnames(judgements.df))]
## Split and get the name of each reference type:
reference.types <- unlist(lapply(reference.types, function(x) sapply(strsplit(x, "\\.{3}"), "[", 2)))
reference.type.names <- reference.types
## Update the column names:
colnames(judgements.df)[grepl("\\.{3}", colnames(judgements.df))] <- reference.types

##### ============= References per Court =======================================
##### =========================================================================

## References have type 0
## Courts have type 1
## there are 68 types of reference
## there are 5 courts

## Make list of nodes for use on references.per.court.fn
nodes.names <- c(reference.type.names,as.character(courtids.df$court.name))
nodes.ids <- 1:(length(nodes.names))
nodes.types <- c(rep(0,length(reference.type.names)),rep(1,length(courtids.df$court.name)))
nodes.df <- data.frame("id" = nodes.ids,
                       "names" = nodes.names,
                       "types" = nodes.types, stringsAsFactors = F) 


court <- 5
cutoff <- 18

colSums(judgements.df[judgements.df$Court.ID == court,reference.type.names] > cutoff)

types.referenced.counts <- colSums(judgements.df[judgements.df$Court.ID == court,reference.type.names] > cutoff)

types.referenced.counts

types.referenced.names <- names(types.referenced.counts)[types.referenced.counts > 0]

types.referenced.names



## Logic for connection
## Count number of cases for each court that had more than n references of a particular type
## That count of cases is then used to decide how strongly connected the court is to that type of reference



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

references.per.court.fn(court = 5, cutoff = 18)

all.edges <- make_references_per_court(18)

all.edges



##### ============= Basic iGraph =======================================
##### =========================================================================

library(igraph)
## Make an igraph
judgement.basic.graph <- graph(all.edges, directed = F)
judgement.basic.graph <- simplify(judgement.basic.graph)
plot(judgement.basic.graph)
# Remove nodes without connections!
judgement.basic.graph <- delete.vertices(judgement.basic.graph,which(degree(judgement.basic.graph)<1))
plot(judgement.basic.graph)


##### ============= visNetwork Bipartite Graph =======================================
##### =========================================================================
library(plyr)
library(visNetwork)

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
## Display graph
visIgraph(g, idToLabel = FALSE) %>% visNodes(size = 10) %>%visIgraphLayout(layout = "layout.bipartite")

### Create edges.df for visNetwork
edges.df <- as.data.frame(get.edgelist(g))
colnames(edges.df) <- c("from","to")





##### ============= Bipartite Graph =======================================
##### =========================================================================

mapvalues(all.edges, from = unique(all.edges), to = 1:length(unique(all.edges)))

unique(all.edges)





