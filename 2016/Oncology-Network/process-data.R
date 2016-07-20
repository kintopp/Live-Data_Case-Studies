### Import xls
library(xlsx)
cruk.edges.df <- read.xlsx("data/CRUK-oxford-graph-data.xls", "Edges")

cruk.nodes.df <- read.xlsx("data/CRUK-oxford-graph-data.xls", "Nodes")

write.csv(file="data/CRUK-oxford-edges.csv", cruk.edges.df, row.names = FALSE)
write.csv(file="data/CRUK-oxford-nodes.csv", cruk.nodes.df, row.names = FALSE)

