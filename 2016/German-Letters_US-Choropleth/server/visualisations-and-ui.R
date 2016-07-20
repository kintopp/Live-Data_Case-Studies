## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================

## ================ Timeline UI Elements ==========================================
## ================================================================================

output$time_period_of_interest_UI <- renderUI({
  if (is.null(input$show_timeslider)) {
    return()
  }
  
  if (is.null(input$show_letters_where_receive_unknown)) {
    return()
  }
  
  dates <- letters_sent_from_usa$Date[!is.na(letters_sent_from_usa$Date)]
  
  if (input$show_timeslider == TRUE) {
    sliderInput(
      "time_period_of_interest",
      "Time period of interest:",
      min = min(dates),
      max = max(dates),
      step = 1,
      value = c(min(dates), max(dates)),
      width = "800px",
      timeFormat = "%F"
    )
  }
  
})

output$show_timeslider_UI <- renderUI({
  checkboxInput("show_timeslider", label = "Filter letters by date sent?", value = TRUE)
})

output$show_letters_where_receive_unknown_UI <- renderUI({
  checkboxInput("show_letters_where_receive_unknown",
                label = "Include letters where receive location unknown?",
                value = TRUE)
})

## ================ Letter Filters (Dates etc) ====================================
## ================================================================================


output_of_input <- "1834-04-12"

letters_sent_from_usa$Date >= "1834-04-12"

subset(letters_sent_from_usa,
       Date >= "1834-04-12",
       Date <= "1900-04-12")
