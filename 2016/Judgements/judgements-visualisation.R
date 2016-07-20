library(magrittr)
### Import
list.files()

judgements.df <- read.csv(file = "judgements.csv", header = TRUE, sep = ",")
caseids.df <- read.csv(file = "caseids.csv", header = TRUE, sep = ",")
courtids.df <- read.csv(file = "courtids.csv", header = TRUE, sep = ",")
## Remove the X column in judgements
judgements.df <- judgements.df[,!colnames(judgements.df) %in% "X"]
## Replace NA with 0
judgements.df[is.na(judgements.df)] <- 0
## Drop row where id == 0
judgements.df <- judgements.df[judgements.df$Case.ID != 0,]



## Get colnames with "..." in so as to find the different reference types:
reference.types <- colnames(judgements.df)[grepl("\\.{3}", colnames(judgements.df))]
## Split and get the name of each reference type:
reference.types <- unlist(lapply(reference.types, function(x) sapply(strsplit(x, "\\.{3}"), "[", 2)))
reference.type.names <- reference.types
## Update the column names:
colnames(judgements.df)[grepl("\\.{3}", colnames(judgements.df))] <- reference.types

## =============================== Setup Nodes ==================================
## ==============================================================================

## References have type 0
## Cases have type 1
## there are 68 types of reference
## there are 93 cases
nodes.names <- c(reference.type.names,as.character(caseids.df$case.name))
nodes.ids <- 1:(length(nodes.names))
nodes.types <- c(rep(0,length(reference.type.names)),rep(1,length(caseids.df$case.name)))
nodes.df <- data.frame("id" = nodes.ids,
                      "names" = nodes.names,
                      "types" = nodes.types, stringsAsFactors = F) 

## Note that the index for cases will now be calculated as:
## length(reference.type.names)+case.id

## ========================== Setup Edges ==========================
## =================================================================
library(igraph)
## make_graph expects a list of edges where consecutive elements are joined:
g <- make_graph(edges = c(1,2,3,1,4,5,5,1),directed = F)
plot(g)

## Write a function that connects a case to each reference type that has a non-zero count
references.per.case.fn <- function(case){
  ## Delete argument assignment
  # case <- 5
  
  ## Find types that are referenced
  types.referenced.names <- reference.type.names[unlist(judgements.df[case,reference.type.names], use.names = F) > 0]
  types.referenced.names
  
  ## Where no references are made exit the function
  if(length(types.referenced.names) == 0){
    return()
  }
  
  ## Get node ids
  types.referenced.ids <- nodes.df[nodes.df$names %in% types.referenced.names,"id"]
  
  ## The actual case number is offset by the total number of reference types!
  ## See nodes.df definition for details
  case.node.id <- length(reference.type.names) + case
  
  ## Generate edge.list for the case
  edge.list <- vector()
  for (i in 1:length(types.referenced.ids)) {
    edge.list <- append(edge.list,types.referenced.ids[i])
    edge.list <- append(edge.list, case.node.id)
  }
  edge.list
}

## Create an empty vector
all.edges <- vector()
## Iterate through all cases:
for(i in 1:nrow(judgements.df)){
  all.edges <<- append(all.edges,references.per.case.fn(i))
}

all.edges

sum(is.na(all.edges))

## ========================= Make a graph ============================


## Make an igraph
judgement.basic.graph <- graph(all.edges, directed = F)
judgement.basic.graph <- simplify(judgement.basic.graph)
plot(judgement.basic.graph)
# Remove nodes without connections!
judgement.basic.graph <- delete.vertices(judgement.basic.graph,which(degree(judgement.basic.graph)<1))
plot(judgement.basic.graph)

E(judgement.basic.graph) %>% unique()


## ============ Let's make this bipartite, please
## =================================================
library(plyr)
g <- make_bipartite_graph(
  ## This is the list of node types as enumerated through the list of all.edges:
  mapvalues(unique(all.edges), from = nodes.df$id, to = nodes.df$types),
  ## Map all.edges to be consecutive numbers
  mapvalues(all.edges, from = unique(all.edges), to = 1:length(unique(all.edges)))
)
lay <- layout.bipartite(g, maxiter = 1000, hgap = 10, vgap = 10)
plot(g, layout=lay[,2:1],vertex.size = 5,vertex.label = NA)

library(visNetwork)
visIgraph(g) %>% visNodes(size = 10) %>%visIgraphLayout(layout = "layout.bipartite")

## Project to a single mode

projection <- bipartite.projection(g)

plot(projection[[1]], main = "Case Attribute Network")
plot(projection[[2]], main = "Judgement Network")

visIgraph(projection[[1]]) %>% visHierarchicalLayout()

