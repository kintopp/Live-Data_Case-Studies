## =========================== Import and Clean ====================================
## ==============================================================================

addresses_df <- read.csv(file = "offshore_leaks_csvs-20160510/Addresses.csv", stringsAsFactors = F)

# Remove quotation marks, # and replace ; with ,
addresses_df$address <- gsub('\\\"', "", addresses_df$address)
addresses_df$address <- gsub('#', "", addresses_df$address)
addresses_df$address <- gsub(';', ",", addresses_df$address)

# Remove escaped formatting characters (newline, etc)
addresses_df$address <- gsub("[\a\b\f\n\r\t\v]", "", addresses_df$address)


## =========================== Dumbly Map over all addresses ====================================
## ==============================================================================
## From http://stackoverflow.com/a/22888218/1659890

## Load libraries
library(httr)
library(rjson)


## Take first 10 addresses
data_for_analysis <- data.frame(
  address = addresses_df$address[1:10],
  icij_id = addresses_df$icij_id[1:10],
  stringsAsFactors = F
)

data <- paste0("[",paste(paste0("\"",data_for_analysis$address,"\""),collapse=","),"]")
url  <- "http://www.datasciencetoolkit.org/street2coordinates"
response <- POST(url,body=data)
json  <- fromJSON(content(response,type="text"))
geocode <- do.call(rbind,sapply(json,
                                function(x) c(long=x$longitude,lat=x$latitude)))
# geocode contains the geocodings:

## =========================== Build Functions for treating each entry in turn ====================================
## ==============================================================================

## Processes json from street2coords
street2coords_json <- function(street_address){
  data <- paste0("[","\"",street_address,"\"","]")
  url  <- "http://www.datasciencetoolkit.org/street2coordinates"
  response <- POST(url,body=data)
  json  <- fromJSON(content(response,type="text"))
  json
}

## Convert output to df
street2coords_convert_to_df <- function(response = NA, icij_id = NA){
  output <- do.call(rbind, lapply(response,
                                  function(x)
                                    c(
                                      longitude = x$longitude, latitude = x$latitude
                                    )))
  output <- as.data.frame(output)
  output$StreetAddress <- as.character(rownames(output))
  output$icij_id <- icij_id
  rownames(output) <- NULL
  output
}

## Stub function for contininuously trimming address in attempt to find coordinates
street2coords_trim <- function(address){
  
}

## Failure output
street2coords_failure <- function(address = NA, icij_id = NA){
  data.frame(
    "StreetAddress" = address,
    "longitude" = NA,
    "latitude" = NA,
    "icij_id" = icij_id
  )
}

## High level function for finding latitude and longitude:
find_long_lat <- function(street_address = NA, icij_id = NA){
  
  response <- street2coords_json(street_address)
  
  # If null then address not found
  if(is.null(response[[1]])){
    street2coords_failure(street_address, icij_id)
  } else
    street2coords_convert_to_df(response, icij_id)
  # geocode <- do.call(rbind,sapply(json,
  #                                 function(x) c(long=x$longitude,lat=x$latitude)))
  # geocode
}

## Generate a sample data set
data_for_analysis <- data.frame(
  "street_address" = addresses_df$address[1:200],
  icij_id = addresses_df$icij_id[1:200],
  stringsAsFactors = F
)

## Create an empty data.frame for results
results_df <- data.frame(
  "StreetAddress" = as.character(),
  "longitude" = as.character(),
  "latitude" = as.character(),
  "icij_id" = as.character()
)

## For loop to generate results:
for(i in 1:nrow(data_for_analysis)){
  results_df <<- rbind(results_df, do.call(find_long_lat, data_for_analysis[i,]))
}

## Return result
results_df

## Number of failures:

sum(is.na(results_df$longitude))

## Number of successes:

sum(!is.na(results_df$longitude))


