## =========================== Import and Tidy Data ====================================
## ==============================================================================

library(xlsx)

realm_cons_import <- read.csv(file = "realms_cons.csv", stringsAsFactors = F)

## Get columns which are measures (those with X[0-9] in their name)
measure_columns <- colnames(realm_cons_import)[grepl("X[0-9]", colnames(realm_cons_import))]

## Retrive all values and find uniques to correct misspellings
all_values <- realm_cons_import[, measure_columns]
unique_values <- unique(as.character(unlist(all_values)))
realm_cons_import[realm_cons_import == "high"] <- "High"
realm_cons_import[realm_cons_import == "HIgh"] <- "High"
realm_cons_import[realm_cons_import == "low"] <- "Low"


