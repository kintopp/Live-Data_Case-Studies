## =================== Get Data ============================ ##

library(xlsx)
weblearn_edges <- read.xlsx(file = "structure.xlsx", sheetIndex = 1)
weblearn_nodes <- read.xlsx(file = "nodes.xlsx", sheetIndex = 1)

colour_vector <- c("#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac")

colour_rules <- data.frame(
  "Category" = c("Archaeology", "Classics", "History", "Sias", "Spi", "wise"),
  "Colour" = c("#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac"),
  stringsAsFactors = F
)

levels(weblearn_nodes$shape) %>% dput()


shape = c("square", "triangle", "box", "circle", "dot", "star","ellipse", "database", "text", "diamond")

shape_rules <- data.frame(
  "Category" = c("Information", "landing", "LectureCapture", "staff", "teaching", "tutor",NA),
  "Shape" = c("square", "triangle", "box", "circle", "diamond", "star","dot"),
  stringsAsFactors = F
)



library(plyr) # for mapvalues

visN_nodes <- data.frame(
  "id" = as.character(weblearn_nodes$Node.id),
  "label" = weblearn_nodes$Name,
  "title" = paste0(
    "<p>Name: ",weblearn_nodes$Name,"</p>",
    "<p>","Lesson Tools Info",weblearn_nodes$Lesson.tools.info,"</p>"
  ),
  "group" = as.character(weblearn_nodes$color),
  "color" = mapvalues(as.character(weblearn_nodes$color), colour_rules$Category, colour_rules$Colour),
  "shape" =mapvalues(as.character(weblearn_nodes$shape), shape_rules$Category, shape_rules$Shape)
)

visN_edges <- data.frame(
  "from" = as.character(weblearn_edges$Parent),
  "to" = as.character(weblearn_edges$Child)
)

library(visNetwork)

visNetwork(nodes = visN_nodes, edges = visN_edges) %>%
  visOptions(highlightNearest = TRUE) %>%
  visInteraction(hoverConnectedEdges = TRUE) %>%
  visIgraphLayout(layout = "layout_components") %>%
  visGroups(groupname = levels(weblearn_nodes$color)[1], color = colour_vector[1]) %>%
  visGroups(groupname = levels(weblearn_nodes$color)[2], color = colour_vector[2]) %>%
  visGroups(groupname = levels(weblearn_nodes$color)[3], color = colour_vector[3]) %>%
  visGroups(groupname = levels(weblearn_nodes$color)[4], color = colour_vector[4]) %>%
  visGroups(groupname = levels(weblearn_nodes$color)[5], color = colour_vector[5]) %>%
  visGroups(groupname = levels(weblearn_nodes$color)[6], color = colour_vector[6]) %>%
  visLegend()

## =========================== Section Title ====================================
## ==============================================================================

library(igraph)

xavier_igraph <- graph.data.frame(visN_edges, vertices = visN_nodes)

## Calculate rank http://stackoverflow.com/a/36554882/1659890

shortest.paths(xavier_igraph)[,'Wise']+1
