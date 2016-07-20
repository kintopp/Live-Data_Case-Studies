### ============= Download and Process Similar Chemicals CSV ========================= ###

# Code not run: Download datafiles from NPG and unzip:
# similar_chemicals.URL <- "http://www.nature.com/article-assets/npg/srep/2011/111215/srep00196/extref/srep00196-s2.zip"
# download.file(similar_chemicals.URL,destfile = "./data/srep00196-s2.zip")

similar_chemicals.df <- read.csv("./data/srep00196-s2.csv", skip = 3, col.names = c("Ingredient.1","Ingredient.2","Shared.Chemicals"))


### ============= Download and Process Recipes CSV ========================= ###

#€ Code not run: Download datafiles from NPG and unzip:
# recipes.URL <- "http://www.nature.com/article-assets/npg/srep/2011/111215/srep00196/extref/srep00196-s3.zip"
# download.file(recipes.URL, destfile = "./data/srep00196-s3.zip")

recipes.df <- read.table("./data/srep00196-s3.csv", sep = ",", as.is = TRUE, fill = TRUE, na.strings = "", skip = 3, col.names = c("Cuisine",paste0("V",1:32)))
recipes.df$Cuisine <- as.factor(recipes.df$Cuisine)

### ============= Ingredient Frequency Table per Cuisine ========================= ###

melted_recipes.df <- melt(recipes.df, id.vars = "Cuisine")

ingredient_frequency.df <- melted_recipes.df[,c(1,3)]
ingredient_frequency.df <- count(ingredient_frequency.df)

### ============ Length of Recipe ============================== ###

length_of_recipes.df <- data.frame("Cuisine" = recipes.df[,1],
                                   "Length" = rowSums(as.data.frame(!is.na(recipes.df))))

### ============ Connected Ingredients ============================== ###

str(recipes.df)
library(plyr)
one_row <- recipes.df[1,]
one_row <- one_row[!is.na(one_row)]

## make complete graph
make_full_citation_graph(length(unique(one_row)), directed = FALSE) %>% 
  set_vertex_attr("name", value = one_row)

g <- make_empty_graph()

g <- add_vertices(g, length(unique(one_row)), name = unique(one_row))


g <- g + path(one_row)

plot(g)

### ====== function to create graph

add_recipe_to_graph <- function(row){

  ## Only get the ingredients, by lopping off first column
  row_to_add <- recipes.df[row,2:ncol(recipes.df)]
  row_to_add <- row_to_add[!is.na(row_to_add)]

  ## Use make_full_citation_graph to create complete graph
  graph_to_add <- make_full_citation_graph(length(unique(row_to_add)), directed = FALSE)
  ## Add vertex names
  graph_to_add <- set_vertex_attr(graph_to_add, "name", value = row_to_add)
  ## Add edge cuisine - contained in the first column from reciples.df
  graph_to_add <- set_edge_attr(graph_to_add, "cuisine", value = rep(recipes.df[row,1], ecount(graph_to_add)))
  
  graph_to_add
  
}
## lapply generates appropriate list structure (for does not)
list_of_graphs <- lapply(1:20, function(x) add_recipe_to_graph(x))
## graph union takes forever
union_of_graphs <- graph.union(list_of_graphs)

plot(union_of_graphs)

E(g)$count
ecount(g)

### ========================= Replace ingredients with numbers

unique_ingredients <- unique(as.vector(unlist(recipes.df[,2:ncol(recipes.df)])))

unique_ingredients <- unique_ingredients[!is.na(unique_ingredients)]

unique_ingredients


foo <- as.numeric(mapvalues(recipes.df$V6, from = unique_ingredients, to = 1:length(unique_ingredients)))

foo

### ================


g <- make_empty_graph(directed = FALSE, n = 10) %>%
  set_vertex_attr("name", value = letters[1:10])

g2 <- g + path("a", "b", "c", "d")
plot(g2)

