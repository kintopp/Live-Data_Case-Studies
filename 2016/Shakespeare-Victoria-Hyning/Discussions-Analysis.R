library(jsonlite)
library(rjson)

## =========================== Import and Codify ====================================
## ==============================================================================

discussions_json <- fromJSON(file = "project-376-comments_2016-05-18.json")

col_1 <- sapply(discussions_json,"[[", 1)
col_2 <- sapply(discussions_json,"[[", 2)
col_3 <- sapply(discussions_json,"[[", 3)
col_4 <- sapply(discussions_json,"[[", 4)
col_5 <- sapply(discussions_json,"[[", 5)
col_6 <- sapply(discussions_json,"[[", 6)
col_7 <- sapply(discussions_json,"[[", 7)
col_8 <- sapply(discussions_json,"[[", 8)
col_9 <- sapply(discussions_json,"[[", 9)
col_10 <- sapply(discussions_json,"[[", 10)
col_11 <- sapply(discussions_json,"[[", 11)
col_12 <- sapply(discussions_json,"[[", 12)

## Unlist lists: http://stackoverflow.com/a/2995515/1659890
col_8[sapply(col_8, is.null)] <- NA
col_8 <- unlist(col_8)
col_9[sapply(col_9, is.null)] <- NA
col_9 <- unlist(col_9)


discussions_df <- data.frame(
  "board_id" = col_1,
  "board_title" = col_2,
  "board_description" = col_3,
  "discussion_id" = col_4,
  "discussion_title" = col_5,
  "comment_id" = col_6,
  "comment_body" = col_7,
  "comment_focus_id" = col_8,
  "comment_focus_type" = col_9,
  "comment_user_id" = col_10,
  "comment_user_login" = col_11,
  "comment_created_at" = col_12,
  check.names = F,
  stringsAsFactors = F
)

## =========================== user_id_frame ====================================
## ==============================================================================

unique_user_ids <- unique(discussions_df$comment_user_id)

unique_user_logins <- unique(discussions_df$comment_user_login)

user_df <- data.frame(
  "n.id" = 1:length(unique_user_ids),
  "comment_user_id" = unique_user_ids,
  "comment_user_login" = unique_user_logins
)

## =========================== Create Edges ====================================
## ==============================================================================



discussion_edges <- data.frame(
  "from" = as.numeric(),
  "to" = as.numeric(),
  "board_id" = as.numeric(),
  "board_title" = as.character(),
  "board_description" = as.character(),
  "discussion_id" = as.numeric(),
  "discussion_title" = as.character()
)

comment_edges_generator <- function(discussion_id) {
  comments_on_discussion <-
    discussions_df[discussions_df$discussion_id == discussion_id, ]
  
  commenters <- unique(comments_on_discussion$comment_user_id)
  # print(paste0("commenters: ",commenters))
  if (length(commenters) < 2) {
    return()
  } else {
    commenter_combinations <- combn(commenters, 2, simplify = T)
    # print(comments_on_discussion$board_id)
    comment_edges_for_page <- data.frame(
      "from" = commenter_combinations[1, ],
      "to" = commenter_combinations[2, ],
      "board_id" = rep(comments_on_discussion$board_id[1],ncol(commenter_combinations)),
      "board_title" = rep(comments_on_discussion$board_title[1],ncol(commenter_combinations)),
      "board_description" = rep(comments_on_discussion$board_description[1],ncol(commenter_combinations)),
      "discussion_id" = rep(comments_on_discussion$discussion_id[1],ncol(commenter_combinations)),
      "discussion_title" = rep(comments_on_discussion$discussion_title[1],ncol(commenter_combinations)),
      stringsAsFactors = F
    )
    comment_edges_for_page
    
    # if (length(commenters) == 1) {
    #   comment_edges_for_page <- data.frame(
    #     "from" = commenters,
    #     "to" = commenters,
    #     "board_id" = comments_on_discussion$board_id,
    #     "board_title" = comments_on_discussion$board_title,
    #     "board_description" = comments_on_discussion$board_description,
    #     "discussion_id" = comments_on_discussion$discussion_id,
    #     "discussion_title" = comments_on_discussion$discussion_title
    #   )
    #   comment_edges_for_page
  }
}

### Apply the function:

invisible(lapply(discussion_ids, function(x) {
  discussion_edges <<-
    rbind(discussion_edges, comment_edges_generator(x))
}))

## =========================== Section Title ====================================
## ==============================================================================

library(visNetwork)

visN_edges <- data.frame(
  "from" = discussion_edges$from,
  "to" = discussion_edges$to,
  "label" = discussion_edges$discussion_title
)

visN_nodes <- data.frame(
  "id" = user_df$comment_user_id,
  "label" = user_df$comment_user_login
)

visNetwork(nodes = visN_nodes, edges = visN_edges) %>%
  visIgraphLayout()


## =========================== BELOW LIVE DRAGONS: graph.union dragons ====================================
## ==============================================================================


str(discussions_df)

discussion_ids <- unique(discussions_df$discussion_id)

library(igraph)

connect_commenters_on_discussion_page <- function(discussion_id){
  commenters_on_page <- discussions_df[discussions_df$discussion_id == discussion_id,]
  commenters_on_page$comment_user_id
}

first_discussion <- connect_commenters_on_discussion_page(discussion_ids[1])


full_graph1 <- make_full_graph(length(first_discussion))
V(full_graph1)$name <- as.character(mapvalues(first_discussion,user_df$comment_user_id,user_df$n.id))


plot(full_graph1)


second_discussion <- connect_commenters_on_discussion_page(discussion_ids[430])
second_discussion

full_graph2 <- make_full_graph(length(second_discussion))
V(full_graph2)$name <-  as.character(mapvalues(second_discussion,user_df$comment_user_id,user_df$n.id))
full_graph2 %>% plot()




plot(union(full_graph1, full_graph2,byname = T))

## =========================== Section Title ====================================
## ==============================================================================
## Make the vertex names numbers:

library(plyr)
labels_for_full_graph_1 <- mapvalues(as.character(first_discussion), as.character(first_discussion), 1:5)
labels_for_full_graph_1

labels_for_full_graph_2 <- mapvalues(as.character(second_discussion), as.character(first_discussion), 1:5)
labels_for_full_graph_2

full_graph1 <- make_full_graph(length(first_discussion))
V(full_graph1)$name <- as.character(1:5)
plot(full_graph1)

full_graph2 <- make_full_graph(length(second_discussion))
V(full_graph2)$name <- as.character(c(5,6,7))
full_graph2 %>% plot()

plot(union(g1, g2, byname = T))


## =========================== Section Title ====================================
## ==============================================================================

g1 <- make_full_graph(5)
V(g1)$name <- as.character(1:5)

dput(V(g1)

g2 <- make_graph(c(1,5,2,4,3,5,2,3), n = 5, directed = F)
V(g2)$name <- as.character(c(1,4,5,8,10))
plot(g2)

plot(union(g1, g2, byname = T))

## =========================== Section Title ====================================
## ==============================================================================

dv1 = as_data_frame(g1, what = "vertices")
rownames(dv1) <- NULL
dv2 = as_data_frame(g2, what = "vertices")
dv2

rownames(dv2) <- NULL
de1 = as_data_frame(g1, what = "edges")
rownames(de1) <- NULL
de2 = as_data_frame(g1, what = "edges")
rownames(de1) <- NULL

gubm = function (g1, g2, dir=FALSE) {
  #stopifnot(!is.directed(g1) & !is.directed(g2))
  dv1 = as_data_frame(g1, what = "vertices")
  rownames(dv1) <- NULL
  dv2 = as_data_frame(g2, what = "vertices")
  rownames(dv2) <- NULL
  de1 = as_data_frame(g1, what = "edges")
  rownames(de1) <- NULL
  de2 = as_data_frame(g1, what = "edges")
  rownames(de1) <- NULL
  dv = merge(dv1, dv2)
  de = merge(de1, de2)
  g = igraph:::graph.data.frame(de, directed = dir, vertices = dv)
  return(g)
}

gubm(g1,g2)

as_data_frame(g1, what = "vertices")

merge()




