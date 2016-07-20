## =========================== Import ====================================
## ==============================================================================

xlsx_convert_import <- function(inputFile = NA,
                                outputFile = NA) {
  if (file.exists(outputFile)) {
    imported_data <<- read.csv(outputFile)
  } else {
    library(xlsx)
    xlsx_import <- read.xlsx(inputFile, sheetIndex = 1)
    write.csv(xlsx_import, file = outputFile, row.names = FALSE)
    remove(xlsx_import)
    imported_data <<- read.csv(outputFile)
  }
}
imported_timeline_data <-
  xlsx_convert_import(inputFile = "timeline_data.xlsx", outputFile = "timeline_data.csv")

timeline_data <- imported_timeline_data
timeline_data$Valid.From <- as.Date(timeline_data$Valid.From)
timeline_data$Valid.To <- as.Date(timeline_data$Valid.To)

timeline_data <-
  timeline_data[order(timeline_data$Valid.From, timeline_data$Name.of.Policy), ]
# timeline_data$Unique.ID <- as.factor(nrow(timeline_data):1)

## =========================== Sort by earliest date ====================================
## ==============================================================================

aggregate(data = timeline_data, Name.of.Policy ~ Valid.From, FUN = sort)
## Find earliest date for each policy
earliest_date_by_Name_of_Policy <-
  timeline_data[timeline_data$Valid.From == ave(timeline_data$Valid.From, timeline_data$Name.of.Policy, FUN =
                                                  min),]
## Force order by date then Name.of.Policy
earliest_date_by_Name_of_Policy <-
  earliest_date_by_Name_of_Policy[order(
    earliest_date_by_Name_of_Policy$Valid.From,
    earliest_date_by_Name_of_Policy$Name.of.Policy
  ), ]

## Reorder the levels in Name.of.Policy for the gantt chart
timeline_data$Name.of.Policy <-
  factor(timeline_data$Name.of.Policy, levels = rev(levels(
    earliest_date_by_Name_of_Policy$Name.of.Policy
  )))
