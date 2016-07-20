## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================




### =============== Process initial focus documents into CSV files =================================

data.dir <- "./data/small_focus/"

all_xlsx_files <- list.files(data.dir)

## Load xlsx package

library(xlsx)

## =============== Extract People Sheets====================

master_people_sheet <- data.frame("Person.Name" = character(),
                                  "iperson_id" = character(),
                                  "Person_aliases" = character(),
                                  "other_info" = character(),
                                  "Birth_year" = numeric(),
                                  "Death_year" = numeric())


collate_people_fn <- function(xlsx.file){
  imported.sheet <- read.xlsx(paste0(data.dir,"/",xlsx.file),"people", 
                            startRow = 1)
  # Drop empty third column
  imported.sheet <- imported.sheet[,c(1,2,4:7)]
  colnames(imported.sheet) <- c("Person.Name","iperson_id","Person_aliases","other_info","Birth_year","Death_year")
  master_people_sheet <<- rbind(master_people_sheet, imported.sheet)
}


## lapply function over all files, note takes several minutes
invisible(lapply(all_xlsx_files, function(x) collate_people_fn(x)))

## Drop rows are all records are NA
master_people_sheet <- master_people_sheet[!!rowSums(!is.na(master_people_sheet)),]

## remove records with is.na(iperson_id)
master_people_sheet <- master_people_sheet[!is.na(master_people_sheet$iperson_id),]

## Remove all duplicate rows:

master_people_sheet <- unique(master_people_sheet)

## There are duplicate person ids... these need fixing

## Export to csv
write.csv(master_people_sheet,"data/master_people_sheet.csv", na = "", row.names = FALSE)

## Import as CSV
imported_master_people_sheet <- read.csv(file = "data/master_people_sheet.csv")
imported_master_people_sheet <- imported_master_people_sheet[imported_master_people_sheet$iperson_id > 0, ]
imported_master_people_sheet <- imported_master_people_sheet[!!rowSums(!is.na(imported_master_people_sheet)),]
## Export fixed CSV
write.csv(imported_master_people_sheet,"data/master_people_sheet.csv",row.names = FALSE)


## =============== Extract Places Sheets ================

master_places_sheet <- data.frame("Location_name" = character(),
                                  "Location_id" = character(),
                                  "Copy.of.type.ahead" = character())


collate_places_fn <- function(xlsx.file){
  imported.sheet <- read.xlsx(paste0(data.dir,"/",xlsx.file),"places", 
                              startRow = 1, colIndex = c(1,2,4))
  colnames(imported.sheet) <- c("Location_name","Location_id","Copy.of.type.ahead")
  master_places_sheet <<- rbind(master_places_sheet, imported.sheet)
}

invisible(lapply(all_xlsx_files, function(x)collate_places_fn(x)))

## Drop rows are all records are NA
master_places_sheet <- master_places_sheet[!!rowSums(!is.na(master_places_sheet)),]

## remove records with is.na(location_id)
master_places_sheet <- master_places_sheet[!is.na(master_places_sheet$Location_id),]

## Remove all duplicate rows:
master_places_sheet <- unique(master_places_sheet)

## Export to csv
write.csv(master_places_sheet,"data/master_places_sheet.csv", na = "", row.names = FALSE)

## Import as CSV
imported_master_places_sheet <- read.csv(file = "data/master_places_sheet.csv")
imported_master_places_sheet <- imported_master_places_sheet[imported_master_places_sheet$Location_id > 0, ]
imported_master_places_sheet <- imported_master_places_sheet[!!rowSums(!is.na(imported_master_places_sheet)),]
## Export fixed CSV
write.csv(imported_master_places_sheet,"data/master_places_sheet.csv",row.names = FALSE)

### =============== Extract Life Events Sheets ==========================

master_life_events_sheets <- c("Event.Label" = character(),
                             "Category" = character(),
                             "Event.or.Relationship.Type" = character(),
                             "Event.Name.or.Description" = character(),
                             "Primary.Participant.Emlo_ID" = numeric(),
                             "Primary.Participant.Name"  = character(),
                             "Primary.Participant.Role" = character(),
                             "Secondary.Participant.Emlo_ID" = numeric(),
                             "Secondary.Participant.Name" = character(),
                             "Secondary.Participant.Type" = character(),
                             "Secondary.Participant.Role" = character(),
                             "DateOne.Year" = numeric(),
                             "DateOne.Month" = numeric(),
                             "DateOne.Day" = numeric(),
                             "DateOne.Uncertainty" = character(),
                             "DateTwo.Year" = numeric(),
                             "DateTwo.Month" = numeric(),
                             "DateTwo.Day" = numeric(),
                             "DateTwo.Uncertainty" = character(),
                             "Date.Type" = character(),
                             "Location.Name" = character(),
                             "Location.Details" = character(),
                             "Location.Type.Ahead" = character(),
                             "Location.Region" = character(),
                             "Location.Country" = character(),
                             "Location.Type" = character(),
                             "Textual.Source.Source" = character(),
                             "Textual.Source.Details" = character(),
                             "Who.Entered.Date" = character(),
                             "Whose.Notes" = character(),
                             "Additional.Notes" = character())


collate_lifevents_fn <- function(xlsx.file){
  imported.sheet <- read.xlsx(paste0(data.dir,"/",xlsx.file),"life events", 
                              startRow = 443, # data headings are on row 443
                              colIndex = 1:31)# data appears only in first 31 columns
  colnames(imported.sheet) <- c("Event.Label","Category","Event.or.Relationship.Type","Event.Name.or.Description",
                                "Primary.Participant.Emlo_ID","Primary.Participant.Name","Primary.Participant.Role",
                                "Secondary.Participant.Emlo_ID","Secondary.Participant.Name","Secondary.Participant.Type",
                                "Secondary.Participant.Role","DateOne.Year","DateOne.Month","DateOne.Day","DateOne.Uncertainty",
                                "DateTwo.Year","DateTwo.Month","DateTwo.Day","DateTwo.Uncertainty","Date.Type",
                                "Location.Name","Location.Details","Location.Type.Ahead","Location.Region","Location.Country",
                                "Location.Type","Textual.Source.Source","Textual.Source.Details","Who.Entered.Date",
                                "Whose.Notes","Additional.Notes")
  master_life_events_sheets <<- rbind(master_life_events_sheets, imported.sheet)
}

invisible(lapply(all_xlsx_files, function(x)collate_lifevents_fn(x)))

## Drop rows are all records are NA
master_life_events_sheets <- master_life_events_sheets[!!rowSums(!is.na(master_life_events_sheets)),]

## remove records with is.na(location_id)
master_life_events_sheets <- master_life_events_sheets[!is.na(master_life_events_sheets$Primary.Participant.Emlo_ID),]

## Remove all duplicate rows:
master_life_events_sheets <- unique(master_life_events_sheets)

## Export to csv
write.csv(master_life_events_sheets,"data/master_life_events_sheet.csv", na = "", row.names = FALSE)

## Import as CSV
imported_master_life_events_sheets <- read.csv(file = "data/master_life_events_sheet.csv")
imported_master_life_events_sheets <- imported_master_life_events_sheets[imported_master_life_events_sheets$Primary.Participant.Emlo_ID > 0, ]
imported_master_life_events_sheets <- imported_master_life_events_sheets[!!rowSums(!is.na(imported_master_life_events_sheets)),]
## Export fixed CSV
write.csv(imported_master_life_events_sheets,"data/master_life_events_sheet.csv",row.names = FALSE)

### =============== People Not Mentioned in People Sheets ==========================

## Import master people sheet

people.df <- read.csv(file = "data/master_people_sheet.csv",na = "")

## Import life.events

life.events.df <- read.csv(file = "data/master_life_events_sheet.csv", na = "")

## Extract only those entries where the Secondary.Participant.Type is "Person"
life.events.df <- life.events.df[life.events.df$Secondary.Participant.Type == "Person",]

##  ==== Get unique EMLO ids from life.events 
primaries <- life.events.df[,c("Primary.Participant.Emlo_ID","Primary.Participant.Name")]
secondaries <- life.events.df[,c("Secondary.Participant.Emlo_ID","Secondary.Participant.Name")]
colnames(primaries) <- c("iperson_id","Person.Name")
colnames(secondaries) <- c("iperson_id","Person.Name")
emlos.in.life.events <- rbind(primaries, secondaries)
emlos.in.life.events <- emlos.in.life.events[!duplicated(emlos.in.life.events),]


## ===== Setdiff of life.events EMLOs and people.df EMLOS
## There are 246 individuals who are listed in life.events but not in people.df
people.missing.from.people.df <- setdiff(emlos.in.life.events$iperson_id,people.df$iperson_id)
## There are 150 people who are in both sources
people.in.both.sources <- intersect(emlos.in.life.events$iperson_id,people.df$iperson_id)
## There are

## ==== Add new people to people.df

people.df <- rbind.fill(people.df, subset(emlos.in.life.events, iperson_id %in% people.missing.from.people.df))

## ==== Look for duplicates

people.tally <- table(people.df$iperson_id)
people.tally <- as.data.frame(people.tally)
duplicated.individuals <- people.tally[people.tally$Freq > 1,]$Var1
duplicated.individuals <- subset(people.df, iperson_id %in% as.vector(duplicated.individuals))
duplicated.individuals[order(duplicated.individuals$iperson_id),]
## Export duplicates
write.csv(duplicated.individuals, "data/duplicated.csv", row.names = FALSE)
## Manually selected rows for removal:
rows.to.remove <- c(1423,1444,1550,1428,1427)
# Delete these rows
people.df <- people.df[-rows.to.remove,]
# drop empty rows
people.df <- people.df[!is.na(people.df$iperson_id),]


## === Export to master_people

write.csv(people.df,"data/master_people_sheet.csv",row.names = FALSE)




