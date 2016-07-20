my_df <- data.frame("ColA" = c("a","b","j","b"),
                    "ColB" = c("u","a","e","e"),
                    "ColC" = c(NA,NA,"a","j")
                    )

list_of_graphs <- list()


row_to_add <- my_df[1,]
row_to_add <- row_to_add[!is.na(row_to_add)]
graph_to_add <- make_full_citation_graph(length(unique(row_to_add)), directed = FALSE)
graph_to_add <- set_vertex_attr(graph_to_add, "name", value = row_to_add)

list_of_graphs <- c(list_of_graphs,graph_to_add)

list_of_graphs

add_row_to_graph <- function(row){
  
  row_to_add <- my_df[row,]
  row_to_add <- row_to_add[!is.na(row_to_add)]
  
  graph_to_add <- make_full_citation_graph(length(unique(row_to_add)), directed = FALSE)
  graph_to_add <- set_vertex_attr(graph_to_add, "name", value = row_to_add)
  
  graph_to_add
  
}

lapply(1:3, function(x){add_row_to_graph(x)})


for (i in 1:nrow(my_df)) {
  add_row_to_graph(i)
}

dput(list_of_graphs)
