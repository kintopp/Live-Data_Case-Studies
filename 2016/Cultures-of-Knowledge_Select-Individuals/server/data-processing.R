## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================


### ============= Import People Sheet ========================= ###

people.df <- read.csv("data/master_people_sheet.csv")
## Convert name to character vector for simplicity later
people.df$Person.Name <- as.character(people.df$Person.Name)
## Sort by name
people.df <- people.df[order(people.df$Person.Name),]

## ===== Add surnames
## Empty character vector
surnames.list <- as.character()
## If there's a comma assume first word is surname, otherwise keep whole name
add.surname.fn <- function(name){
  if(grepl(",",name)){
    surnames.list <<- append(surnames.list, sapply(strsplit(name, ","), "[[", 1))
  } else {
    surnames.list <<- append(surnames.list,name)
  }
}

## lapply invisibly
invisible(lapply(people.df$Person.Name,function(x)add.surname.fn(x)))
## Append surname column to people.df
people.df$Surname <- surnames.list

## ==== Handle duplicates with different names
## Some people are included multiple times this code identifies them but is commented out
# unique_emlos_people_df <- unique(people.df$iperson_id)
# sums_list <- as.numeric()
# lapply(unique_emlos_people_df, function(x){
#   sums_list <<- append(x = sums_list,sum(people.df$iperson_id == x))
# })
# people_listed_multiple_times <- unique_emlos_people_df[which(sums_list>1)]
# people_listed_multiple_times <- people.df[people.df$iperson_id %in% people_listed_multiple_times,]

## This is a black list of repeated names
names_to_remove <- c("nat: philosopher. Boyle / Boyle Robert. 1627-91.",
                     "Sec: of State. Nicholas / Nicholas Sir Edward. 1593-1669.",
                     "Schonaich (fl. 1660)",
                     "von Schönaich (family)",
                     "Budeus (fl. 1636-1642)",
                     "Žerotín, Karel of",
                     "Tossanus, Maria, also Toussaint")
people.df <- people.df[!people.df$Person.Name %in% names_to_remove,]


### ============= Import Places Sheet =========================
places.df <- read.csv("data/master_places_sheet.csv")

### ============= Import All Life Events ========================= 

life.events.df <- read.csv("data/master_life_events_sheet.csv")
life.events.df$Primary.Participant.Emlo_ID <- as.character(life.events.df$Primary.Participant.Emlo_ID)
life.events.df$Secondary.Participant.Emlo_ID <- as.character(life.events.df$Secondary.Participant.Emlo_ID)

### ============= Non-self Referential Events (i.e. not births) =============

multiparty.interactions <- life.events.df[!is.na(life.events.df$Secondary.Participant.Emlo_ID),]
multiparty.interactions <- droplevels(multiparty.interactions)

## =========================== Canonical List of People ====================================
## ==============================================================================


all_unique_emlo_ids <- unique(c(people.df$iperson_id,multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID))

emlos_not_in_people <- setdiff(all_unique_emlo_ids, unique(people.df$iperson_id))

throw_away <- data.frame(
  "iperson_id" = as.numeric(c(multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID)),
  "Person.Name" = c(as.character(multiparty.interactions$Primary.Participant.Name),as.character(multiparty.interactions$Secondary.Participant.Name)),
  stringsAsFactors = F)
throw_away <- throw_away[!duplicated(throw_away),]

missing_persons <- throw_away[throw_away$iperson_id %in% emlos_not_in_people,]

people.df <- rbind.fill(people.df, missing_persons)

## ============= Get list of non-persons in life events and people.df ===========
## ==============================================================================

non_people_in_life_events <- life.events.df[life.events.df$Secondary.Participant.Type != "Person",]

non_people_in_life_events_emlo_ids <- unique(non_people_in_life_events$Secondary.Participant.Emlo_ID)
non_people_in_life_events_emlo_ids <- non_people_in_life_events_emlo_ids[!is.na(non_people_in_life_events_emlo_ids)]

non_people_in_people_df <- intersect(non_people_in_life_events_emlo_ids, people.df$iperson_id)

# life.events.df <- life.events.df[life.events.df$Secondary.Participant.Type == "Person",]


