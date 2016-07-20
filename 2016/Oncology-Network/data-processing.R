ox_ox_nodes <-
  read.csv("data/ox-ox-nodes.csv", stringsAsFactors = F)
ox_ox_edges <-
  read.csv("data/ox-ox-edges.csv", stringsAsFactors = F)


## colnames need to be lower case
colnames(ox_ox_edges) <- tolower(colnames(ox_ox_edges))
colnames(ox_ox_nodes) <- tolower(colnames(ox_ox_nodes))
## visNetwork wants from and to not source and target
colnames(ox_ox_edges)[colnames(ox_ox_edges) == c("source", "target")] <-
  c("from", "to")
## the vertex tooltip is added by way of the title column:
ox_ox_nodes$title <- ox_ox_nodes$name

ox_ox_nodes$title[duplicated(ox_ox_nodes$title)]

## =========================== Duplicate Names ==================================
## ==============================================================================

## Pad duplicate names
dupes <- ox_ox_nodes$name[duplicated(ox_ox_nodes$name)]
make_unique_names <- function(name) {
  small_vec <- as.character()
  for (i in 1:sum(dupes == name)) {
    small_vec <-
      append(x = small_vec, values = paste0(name, paste0(rep(" ", i), collapse = "")))
  }
  small_vec
}
ox_ox_nodes$name[duplicated(ox_ox_nodes$name)] <-
  unlist(lapply(unique(ox_ox_nodes$name[duplicated(ox_ox_nodes$name)]), function(x)
    make_unique_names(x)))

## =========================== Colours ==========================================
## ==============================================================================

department_colours <- data.frame(
  "department" = unique(ox_ox_nodes$department),
  "colours" = c("#023FA5", "#7D87B9", "#BEC1D4", "#D6BCC0", "#BB7784", "#8E063B", 
                "#4A6FE3", "#8595E1", "#B5BBE3", "#E6AFB9", "#E07B91", "#D33F6A", 
                "#11C638", "#8DD593", "#C6DEC7", "#EAD3C6", "#F0B98D", "#EF9708", 
                "#0FCFC0", "#9CDED6", "#D5EAE7", "#F3E1EB", "#F6C4E1", "#F79CD4")[1:length(unique(ox_ox_nodes$department))],
  stringsAsFactors = F
)

ox_ox_nodes$color <- mapvalues(ox_ox_nodes$department, from = department_colours$department, to = department_colours$colours)
