### Initial Import and Play

# library(xlsx)
# xlsx_import <- read.xlsx("concept.xlsx", sheetIndex = 1)
# write.csv(xlsx_import, file = "xavier-data.csv", row.names = FALSE)

# Import
xavier_data <- read.csv("xavier-data.csv")
# Drop all columns except Parent and Child
xavier_data <- xavier_data[,c("Parent","Child")]
xavier_data$Parent <- as.character(xavier_data$Parent)
xavier_data$Child <- as.character(xavier_data$Child)

str(xavier_data)

# Find all unique nodes

unique_nodes <- unique(c(xavier_data$Parent, xavier_data$Child)) 

## =========================== visNetwork ==-====================================
## ==============================================================================

library(visNetwork)
library(plyr)

mapvalues(xavier_data$Parent, from = unique_nodes, to = 1:length(unique_nodes)) %>% as.numeric()
mapvalues(xavier_data$Child, from = unique_nodes, to = 1:length(unique_nodes)) %>% as.numeric()



visN_nodes <- data.frame(
  "id" = 1:length(unique_nodes),
  "label" = unique_nodes,
  "title" = unique_nodes
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

visNetwork(nodes = visN_nodes, visN_edges)



