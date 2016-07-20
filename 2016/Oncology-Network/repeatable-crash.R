

V(ox_ox_depart_igraph)

igraph::ecount(ox_ox_depart_igraph)
igraph::vcount(ox_ox_depart_igraph)

E(ox_ox_depart_igraph)

foobar <- igraph::as_data_frame(x = ox_ox_depart_igraph, what = c("edges"))
foobar$from

str(foobar)
