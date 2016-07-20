##### ============= Import =======================================
##### ============================================================

judgements.df <- read.csv(file = "data/judgements.csv", header = TRUE, sep = ",")
caseids.df <- read.csv(file = "data/caseids.csv", header = TRUE, sep = ",")
courtids.df <- read.csv(file = "data/courtids.csv", header = TRUE, sep = ",")
## Remove the X column in judgements
judgements.df <- judgements.df[,!colnames(judgements.df) %in% "X"]
## Replace NA with 0
judgements.df[is.na(judgements.df)] <- 0
## Drop row where id == 0
judgements.df <- judgements.df[judgements.df$Case.ID != 0,]

## Fix column names

## Get colnames with "..." in so as to find the different reference types:
reference.types <- colnames(judgements.df)[grepl("\\.{3}", colnames(judgements.df))]
## Split and get the name of each reference type:
reference.types <- unlist(lapply(reference.types, function(x) sapply(strsplit(x, "\\.{3}"), "[", 2)))
reference.type.names <- reference.types
## Update the column names:
colnames(judgements.df)[grepl("\\.{3}", colnames(judgements.df))] <- reference.types

##### ============= References per Court =======================================
##### =========================================================================

## References have type 0
## Courts have type 1
## there are 68 types of reference
## there are 5 courts

## Make list of nodes for use on references.per.court.fn
nodes.names <- c(reference.type.names,as.character(courtids.df$court.name))
nodes.ids <- 1:(length(nodes.names))
nodes.types <- c(rep(0,length(reference.type.names)),rep(1,length(courtids.df$court.name)))
nodes.df <- data.frame("id" = nodes.ids,
                       "names" = nodes.names,
                       "types" = nodes.types, stringsAsFactors = F) 