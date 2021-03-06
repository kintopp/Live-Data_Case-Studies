## With renaming

### Initial Import and Play

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

## =========================== visNetwork ==-====================================
## ==============================================================================

library(visNetwork)
library(plyr)

visN_nodes <- data.frame(
  "id" = 1:length(unique_nodes),
  "label" = unique_names,
  "title" = unique_names
)



visN_edges <- data.frame(
  "from" = mapvalues(
    xavier_data$Parent,
    from = unique_nodes,
    to = 1:length(unique_nodes)
  ) %>% as.numeric(),
  "to" = mapvalues(
    xavier_data$Child,
    from = unique_nodes,
    to = 1:length(unique_nodes)
  ) %>% as.numeric()
)

nrow(visN_edges)

xavier_data

visNetwork(nodes = visN_nodes, visN_edges) %>%
  visHierarchicalLayout() %>%
  visOptions(highlightNearest = TRUE) %>%
  visInteraction(hoverConnectedEdges = TRUE)



