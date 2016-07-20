###### ======= Control Vars ===================

show_timeslider <- TRUE
show_letters_where_receive_unknown <- TRUE


##### ======= Functions ========================

my_func <- function(start.date){
  subset(entries_with_locations, Date >= start.date)
}


my_func(as.POSIXct("1900/01/01"))

as.POSIXct("1900/01/01")

entries_with_locations$Date


letters_sent_between_dates <-
  function(start.year = NA,
           end.year = NA,
           data = NA) {
    subset(data,
           Date >= as.POSIXct(paste0(as.character(start.year), "/01/01")) &
             Date <= as.POSIXct(paste0(as.character(end.year), "/12/31")))
  }

letters_sent_between_dates(start.year = 1910, end.year = 1920, data = entries_with_locations)



paste0(1900, "/01/01") %>% as.POSIXct(".")

paste0(1900, "/12/31") %>% as.POSIXct(".")

## =====

entries_with_locations$Date[!is.na(entries_with_locations$Date)] %>% min()
