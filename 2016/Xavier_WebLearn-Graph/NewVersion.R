# Commented out as already converted xlsx to csv
# library(xlsx)
# xlsx_import <- read.xlsx("concept.xlsx", sheetIndex = 1)
# write.csv(xlsx_import, file = "xavier-data.csv", row.names = FALSE)

# Import csv as faster than xlsx
xavier_data <- read.csv("xavier-data.csv")
# Drop all columns except Parent and Child
xavier_data <- xavier_data[,c("Parent","Child")]
xavier_data$Parent <- as.character(xavier_data$Parent)
xavier_data$Child <- as.character(xavier_data$Child)

# Find all unique nodes
unique_nodes <- unique(c(xavier_data$Parent, xavier_data$Child))
# Function to get the last item in the node's name
get_name <- function(item){
  l <- length(item)
  item[l]
}
# lapply get_name to get all unique_names
unique_names <- as.character()
invisible(
  lapply(strsplit(unique_nodes, "-"), function(x){
    unique_names <<- append(x = unique_names, values = get_name(x))
  }
  )
)
unique_names <- unique(unique_names)

## =========================== Replacement Rules ================================
## ==============================================================================

replacements_names <- data.frame(
  "name" = unique_names,
  "id" = 1:length(unique_names)
)

## =========================== Create Edges =====================================
## ==============================================================================


## Load zoo for rollappy
library(zoo)
edges_df <- data.frame(
  "from" = as.character(),
  "to" = as.character()
)
create_edges_from_names <- function(name){
  if(grepl("[-]",name)){
    df <- as.data.frame(rollapply(unlist(strsplit(name, split = "-")), 2, by = 1, c))
    colnames(df) <- c("from","to")
    edges_df <<- rbind(edges_df, df)
  } else 
    return()
}
## invisibly lapply
invisible(
  lapply(unique_nodes, function(x){
    create_edges_from_names(x)
  })
)
# Remove duplicates
edges_df <- edges_df[!duplicated(edges_df),]
# Convert to characters
edges_df$from <- as.character(edges_df$from)
edges_df$to <- as.character(edges_df$to)
# Remove self loops
edges_df <- edges_df[edges_df$from != edges_df$to,]

## =========================== Infer Level from data ============================
## ==============================================================================

unique_nodes

library(stringr) # for counting
highest_level <- function(node.name){
  if(any(grepl(paste0("^",node.name), unique_nodes))){
    1
  } else
    min(str_count(unique_nodes[grepl(paste0(".",node.name,"$"), unique_nodes)],"[-]"))+1
}

highest_level("Wise")
highest_level("AfricanStudies")

## =========================== Just Adjacent visNetwork =========================
## ==============================================================================

visN_nodes <- data.frame(
  "id" = 1:length(unique_names),
  "label" = unique_names,
  "title" = unique_names,
  "level" = unlist(lapply(unique_names, function(x){highest_level(x)}))
)

visN_edges <- data.frame(
  "from" = mapvalues(
    edges_df$from,
    from = unique_names,
    to = 1:length(unique_names)
  ) %>% as.numeric(),
  "to" = mapvalues(
    edges_df$to,
    from = unique_names,
    to = 1:length(unique_names)
  ) %>% as.numeric()
)

visNetwork(nodes = visN_nodes, edges = visN_edges) %>%
  visHierarchicalLayout() %>%
  visOptions(highlightNearest = TRUE) %>%
  visInteraction(hoverConnectedEdges = TRUE)

## =========================== From Records ====================================
## ==============================================================================

visN_nodes <- data.frame(
  "id" = 1:length(unique_names),
  "label" = unique_names,
  "title" = unique_names
)


visN_edges <- data.frame(
  "from" = mapvalues(
    as.character(xavier_data$Parent),
    from = unique_names,
    to = 1:length(unique_names)
  ) %>% as.numeric(),
  "to" = mapvalues(
    as.character(xavier_data$Child),
    from = unique_names,
    to = 1:length(unique_names)
  ) %>% as.numeric()
)

xavier_data$Child

visN_edges

