## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

# library(xlsx)
# 
# xlsx_import <- read.xlsx("data/Edited_Place of Birth_MartinHadley.xlsx", sheetIndex = 1)
# write.csv(xlsx_import, file = "data/Place of Birth_MartinHadley.csv", row.names = FALSE, quote = TRUE)

place_of_birth <-
  read.csv(file = "data/Place of Birth_MartinHadley.csv", stringsAsFactors = FALSE)

## =========================== Add combined Lat Long Column =====================
## ==============================================================================

combined_lat_long <- paste(place_of_birth$Latitude,place_of_birth$Longitude)

combined_lat_long[combined_lat_long == "NA NA"] <- NA

place_of_birth$Combined.Lat.Long <- combined_lat_long

## Drop missing locations:

place_of_birth <- place_of_birth[!is.na(place_of_birth$Combined.Lat.Long),]

# ## =========================== Drop Missing and Split Locations =================
# ## ==============================================================================
# 
# 
# place_of_birth <- place_of_birth[!is.na(place_of_birth$Lat.Long), ]
# print("before trimws")
# place_of_birth$Lat.Long <- trimws(place_of_birth$Lat.Long)
# print("after trimws")
# ## Kill commas
# place_of_birth$Lat.Long <-
#   str_replace(place_of_birth$Lat.Long,
#               pattern = ",",
#               replacement = "")
# print("after str_replace")
# 
# place_of_birth$Lat.Long
# 
# 
# 
# lat_long_splittings <- strsplit(place_of_birth$Lat.Long, "\\s+")
# print("after strsplit")
# 
# 
# #
# # place_of_birth[grepl("28.61", place_of_birth$Lat.Long),]$Lat.Long %>% dput()
# #
# # grepl("51.861667", place_of_birth$Lat.Long)
# # lat_long_splittings[grepl("51.861667", lat_long_splittings)]
# #
# # place_of_birth$Lat.Long
# 
# dput(lat_long_splittings)
# 
# place_of_birth$Lat <- unlist(lapply(lat_long_splittings, "[[", 1))
# print("after sapply 1")
# place_of_birth$Lon <- unlist(lapply(lat_long_splittings, "[[", 2))
# print("after sapply 2")

## =========================== Trim White Space ====================================
## ==============================================================================


## Strip white space on columns
place_of_birth$PoB..Town.or.Parish. <- trimws(place_of_birth$PoB..Town.or.Parish.)
print("after trimws pob")
place_of_birth$Name <- trimws(place_of_birth$Name)
print("after trimws name")
place_of_birth$Parent.names <- trimws(place_of_birth$Parent.names)
print("after trimws parentnames")



## =========================== Tally Locations ==================================
## ==============================================================================


latlong_location_tally <-
  as.data.frame(table(place_of_birth$Combined.Lat.Long),stringsAsFactors = FALSE)

place_of_birth$latlong.location.tally <-
  as.numeric(
    mapvalues(
      place_of_birth$Combined.Lat.Long,
      from = latlong_location_tally$Var1,
      to = latlong_location_tally$Freq
    )
  )



## ======== Find LatLongs used against multiple locations ======================
## ==============================================================================

# latlong_location_tally$Freq <- rescale(latlong_location_tally$Freq, to = c(5,20))
latlongs_with_multiple_locations <- data.frame(
  "LatLong" = as.character(),
  "PoB..Town.or.Parish." = as.character()
)

invisible(lapply(unique(place_of_birth$Combined.Lat.Long), function(x) {
  locations_with_latlong <-
    unique(place_of_birth[place_of_birth$Combined.Lat.Long == x, "PoB..Town.or.Parish."])
  if (length(locations_with_latlong) > 1) {
    latlongs_with_multiple_locations <<- rbind(
      latlongs_with_multiple_locations,
      data.frame(
        "LatLong" = rep(x, length(locations_with_latlong)),
        "PoB..Town.or.Parish." = locations_with_latlong
      )
    )
  }
}))



write.csv(file = "data/latlongs_with_multiple_location_names.csv", x = latlongs_with_multiple_locations[!duplicated(latlongs_with_multiple_locations),], row.names = F)

## =========================== Drop Missing Dates ==============================
## ==============================================================================

place_of_birth <- place_of_birth[!is.na(place_of_birth$DoB), ]
## Find those entries with (Bapt.) for encoding in a new data column
place_of_birth$Known_Baptism <- grepl("Bapt.", place_of_birth$DoB)
## Function to fix dates with (Bapt.) in
correct_bapt_dates <- function(x) {
  if (grepl("Bapt.", x)) {
    unlist(strsplit(x, "\\s+"))[1]
  } else
    x
}
## Update DoB column
place_of_birth$DoB <-
  unlist(lapply(place_of_birth$DoB, function(x) {
    correct_bapt_dates(x)
  }))

## Use dmy to kill dates which are not properly formatted
place_of_birth$DoB <-
  force_tz(dmy(place_of_birth$DoB), tzone = "GMT")
## Drop those where date is unknown
place_of_birth <- place_of_birth[!is.na(place_of_birth$DoB), ]
