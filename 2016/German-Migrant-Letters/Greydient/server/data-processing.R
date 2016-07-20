## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

### ============= Data Processing Task 1 ========================= ###

### Commented out due to slow import ###
# library(xlsx)
# letters_maxqda_import <- read.xlsx("Letters MaxQDA Format.xls", sheetIndex = 1)
# write.csv(letters_maxqda_import, file = "latters-MaxQDA-Format.csv")
# Import csv file
letters_maxqda_import <- read.csv(file ="data/latters-MaxQDA-Format.csv", stringsAsFactors = FALSE)
### ======= Location df

locations_vec <- unique(c(letters_maxqda_import$Sender.Location,letters_maxqda_import$Receiver.Location))

### For historic reasons, rename as entries.with.locations (Academic wants letters without a receive location to be included)

entries.with.locations <- letters_maxqda_import

# # drop rows with empty longitudes/latitudes
# entries.with.locations <- letters_maxqda_import[!is.na(letters_maxqda_import$Sender.Location.GIS.Latitude) &
#                                                   !is.na(letters_maxqda_import$Sender.Location.GIS.Longitude) &
#                                                   !is.na(letters_maxqda_import$Receiver.Location.GIS.Latitude) &
#                                                   !is.na(letters_maxqda_import$Receiver.Location.GIS.Longitude),]

### ============= Add columns with combined coords for simpler processing later
entries.with.locations$Sender.LatLong.String <- paste(
  entries.with.locations$Sender.Location.GIS.Latitude,
  entries.with.locations$Sender.Location.GIS.Longitude)
entries.with.locations$Receiver.LatLong.String <- paste(
  entries.with.locations$Receiver.Location.GIS.Latitude,
  entries.with.locations$Receiver.Location.GIS.Longitude)
## as.character for easier processing
entries.with.locations$Receiver.LatLong.String <- as.character(entries.with.locations$Receiver.LatLong.String)
entries.with.locations$Sender.LatLong.String <- as.character(entries.with.locations$Sender.LatLong.String)

## Drop element where send == receive
entries.with.locations <- entries.with.locations[entries.with.locations$Sender.LatLong.String != entries.with.locations$Receiver.LatLong.String,]

## Interpret dates as dmy and force as GMT
entries.with.locations$Date <- force_tz(dmy(entries.with.locations$Date, quiet = TRUE), tzone = "GMT")
## Find any dates in the future
future.test <- entries.with.locations$Date > as.POSIXct("2016/01/01")
## Remove these dates!
entries.with.locations <- entries.with.locations[!mapvalues(future.test, c(FALSE,NA,TRUE),c(FALSE,FALSE,TRUE)),]




### ===== OLD =====
# ## find ill-defined dates
# suppressWarnings(dates_as_dates <- dmy(entries.with.locations$Date, quiet = TRUE))
# ## Drop ill-defined dates
# entries.with.locations <- entries.with.locations[!is.na(dates_as_dates),]
# ## Interpret dates as dmy and force as GMT
# entries.with.locations$Date <- force_tz(dmy(entries.with.locations$Date, quiet = TRUE), tzone = "GMT")
# ## Remove date in the future!
# entries.with.locations <- entries.with.locations[entries.with.locations$Date < as.POSIXct("2016/01/01"),]

## Make a set of location -> name replacements
location_name_df <- data.frame("LatLong" = c(entries.with.locations$Sender.LatLong.String,entries.with.locations$Receiver.LatLong.String),
                               "Location.Name" = c(entries.with.locations$Sender.Location, entries.with.locations$Receiver.Location))
## Drop those without specific location
location_name_df <- location_name_df[location_name_df$LatLong != "NA NA",]

### ============= Find duplicate locations
location_name_df <- location_name_df[!duplicated(location_name_df),]
location_name_df$LatLong <- as.character(location_name_df$LatLong)
location_name_df$Location.Name <- as.character(location_name_df$Location.Name)
duplicate_locations <- subset(location_name_df, LatLong %in% location_name_df[duplicated(location_name_df$LatLong),]$LatLong)
# Remove duplicates
location_name_df <- location_name_df[!duplicated(location_name_df$LatLong),]


### ============= Letter Series

entries.with.locations$Letter.Series <- as.factor(entries.with.locations$Letter.Series)


### ============= Data Processing Task 2 ========================= ###
