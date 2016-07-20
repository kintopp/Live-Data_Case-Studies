## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

## At the end of this script a subsetted version the data file is saved as:
## data/letters_subsetted.csv
## If this file already exists, this file is not called

## ================ Import Letter Series ==========================================
## ================================================================================
## Commented out due to slow import ###
# library(xlsx)
# letters_maxqda_import <- read.xlsx("Letters MaxQDA Format.xls", sheetIndex = 1)
# write.csv(letters_maxqda_import, file = "latters-MaxQDA-Format.csv")

# Import csv file
letters_maxqda_import <- read.csv(file ="data/latters-MaxQDA-Format.csv", stringsAsFactors = FALSE)

### ======= Location df
locations_vec <- unique(c(letters_maxqda_import$Sender.Location,letters_maxqda_import$Receiver.Location))

### For historic reasons, rename as entries_with_locations (Academic wants letters without a receive location to be included)

entries_with_locations <- letters_maxqda_import

# # drop rows with empty longitudes/latitudes
# entries_with_locations <- letters_maxqda_import[!is.na(letters_maxqda_import$Sender.Location.GIS.Latitude) &
#                                                   !is.na(letters_maxqda_import$Sender.Location.GIS.Longitude) &
#                                                   !is.na(letters_maxqda_import$Receiver.Location.GIS.Latitude) &
#                                                   !is.na(letters_maxqda_import$Receiver.Location.GIS.Longitude),]

entries_with_locations$Letter.Series <- as.factor(entries_with_locations$Letter.Series)

### ============= Add columns with combined coords for simpler processing later
entries_with_locations$Sender.LatLong.String <- paste(
  entries_with_locations$Sender.Location.GIS.Latitude,
  entries_with_locations$Sender.Location.GIS.Longitude)
entries_with_locations$Receiver.LatLong.String <- paste(
  entries_with_locations$Receiver.Location.GIS.Latitude,
  entries_with_locations$Receiver.Location.GIS.Longitude)
## as.character for easier processing
entries_with_locations$Receiver.LatLong.String <- as.character(entries_with_locations$Receiver.LatLong.String)
entries_with_locations$Sender.LatLong.String <- as.character(entries_with_locations$Sender.LatLong.String)

## Drop element where send == receive
entries_with_locations <- entries_with_locations[entries_with_locations$Sender.LatLong.String != entries_with_locations$Receiver.LatLong.String,]


## Interpret dates as dmy and force as GMT
entries_with_locations$Date <- force_tz(dmy(entries_with_locations$Date, quiet = TRUE), tzone = "GMT")
## Find any dates in the future
future.test <- entries_with_locations$Date > as.POSIXct("2016/01/01")
## Remove these dates!
entries_with_locations <- entries_with_locations[!mapvalues(future.test, c(FALSE,NA,TRUE),c(FALSE,FALSE,TRUE)),]

## Filter by geographic element:
get.country <- function(location_string){
  if(location_string == "" | location_string == "Schiff Sorrento"){
    "NA"
  } else {
    # strsplit(location_string, ",")[1]
    sapply(strsplit(location_string, ","), "[[", 1)
  }
}
entries_with_locations$Sender.Country <- unlist(lapply(entries_with_locations$Sender.Location, function(x) get.country(x)))
entries_with_locations$Receiver.Country <- unlist(lapply(entries_with_locations$Receiver.Location, function(x) get.country(x)))

## ================ Extract only entries with letters from the USA ================
## ================================================================================

## Restrict letters_sent_from_usa to only those where the the letter was sent from the USA
letters_sent_from_usa <-
  entries_with_locations[entries_with_locations$Sender.Country == "USA", ]

## Filter out those entries without a state, i.e. New York (NY)
letters_sent_from_usa <-
  letters_sent_from_usa[grepl("[(][A-Z]{2}[)]", letters_sent_from_usa$Sender.Location), ]

## Drop NA entries
letters_sent_from_usa <- letters_sent_from_usa[!is.na(letters_sent_from_usa$Sender.Location.GIS.Longitude),]


## Create a vector containing send states"
get_states_from_column <- function(data){
  str_extract(string = data, pattern = "[(][A-Z]{2}[)]") %>%
    str_split(pattern = "[(]|[)]") %>%
    sapply("[[", 2)
}

## Add to the letters_sent_from_usa
letters_sent_from_usa$Sender.State <- get_states_from_column(letters_sent_from_usa$Sender.Location)

## ================ Make a location_name_df for easy lookup =======================
## ================================================================================

## Make a set of location -> name replacements
location_name_df <- data.frame("LatLong" = c(letters_sent_from_usa$Sender.LatLong.String,letters_sent_from_usa$Receiver.LatLong.String),
                               "Location.Name" = c(letters_sent_from_usa$Sender.Location, letters_sent_from_usa$Receiver.Location))
## Drop those without specific location
location_name_df <- location_name_df[location_name_df$LatLong != "NA NA",]

### ============= Find duplicate locations
location_name_df <- location_name_df[!duplicated(location_name_df),]
location_name_df$LatLong <- as.character(location_name_df$LatLong)
location_name_df$Location.Name <- as.character(location_name_df$Location.Name)
duplicate_locations <- subset(location_name_df, LatLong %in% location_name_df[duplicated(location_name_df$LatLong),]$LatLong)
# Remove duplicates
location_name_df <- location_name_df[!duplicated(location_name_df$LatLong),]


## =========================== Export Data to .RData =================
## ==============================================================================


letters_sent_from_usa <- letters_sent_from_usa[,c("Document.name", "Sender.Location.GIS.Latitude", 
                                              "Sender.Location.GIS.Longitude", "Receiver.Location.GIS.Latitude", 
                                              "Receiver.Location.GIS.Longitude", "NA.GER", "Letter.Series", 
                                              "Relationship.Sender.Receiver", "Social.Strata.Sender", "Social.Strata.Receiver", 
                                              "Date", "Sender.LatLong.String", "Receiver.LatLong.String")
                                           ]

save(letters_sent_from_usa, file = "data/letters_sent_from_usa.RData")
save(location_name_df, file = "data/location_name_df.RData")


## =========================== Write Success File ====================================
## ==============================================================================

write.csv(x = "Data Processed", file = "data/data-processed.csv")




