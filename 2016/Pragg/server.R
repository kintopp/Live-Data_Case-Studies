## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

library(highcharter)
library(shiny)
library(tidyr)

shinyServer(function(input, output) {
  source(file = "data-processing.R", local = TRUE)
  
  ## =========================== Livian Dist ====================================
  ## ==============================================================================
  
  livian_order_by <- function(column) {
    if (input$livian_dist_stack_type == "normal") {
      livian_dist[rev(order(livian_dist[, column])), ]
    } else
      
      if (input$livian_dist_orderby == "Book") {
        livian_dist[rev(order(livian_dist[, column])), ]
      } else
        livian_dist[rev(order(livian_dist[, column] / {
          livian_dist[, "Rome.and.the.West"] + livian_dist[, "East.of.the.Adriatic"]
        })), ]
    
  }
  
  output$livian_dist_highchart <- renderHighchart({
    ordered_livian <- livian_order_by(input$livian_dist_orderby)
    
    hc <- highchart() %>%
      hc_xAxis(categories = as.character(ordered_livian$Book),
               title = list(text = "Book")) %>%
      hc_add_series(
        name = "Rome and the West",
        data = ordered_livian$Rome.and.the.West,
        index = switch(
          input$livian_dist_orderby,
          "Rome.and.the.West" = 1,
          "East.of.the.Adriatic" = 0,
          "Book" = 1
        ),
        color = "#f1a340"
      ) %>%
      hc_add_series(
        name = "East of the Adriatic",
        data = ordered_livian$East.of.the.Adriatic,
        index = switch(
          input$livian_dist_orderby,
          "Rome.and.the.West" = 0,
          "East.of.the.Adriatic" = 1,
          "Book" = 0
        ),
        color = "#998ec3"
      )
    
    hc %>% hc_chart(type = "bar") %>% hc_plotOptions(series = list(stacking = input$livian_dist_stack_type))
  })
  
  ## =========================== Legions ==========================================
  ## ==============================================================================
  
  output$legions_highchart <- renderHighchart({
    legions_east <- legions_data_long[legions_data_long$Country == "East",]
    legions_west <- legions_data_long[legions_data_long$Country == "West",] 
    
    hc <- highchart() %>% 
      hc_xAxis(categories = 200:91, 
               # categories = as.character(legions_data_long$Year), 
               title = list(text = "Date (BC)")) %>% 
      hc_add_series(name = "East", data = legions_east$Value, pointPadding = 0.1, color = "#f1a340") %>% 
      hc_add_series(name = "West", data = legions_west$Value, color = "#998ec3") %>%
      hc_yAxis(title = list(text = "Number of Legions"))
    
    hc %>% hc_chart(type = "column", zoomType = "x")
  })
  
  ## =========================== Section Title ====================================
  ## ==============================================================================
  
  output$triumphs_highchart <- renderHighchart({
    hc <- highchart() %>% 
      hc_xAxis(categories = triumphs_data$Date.BC, 
               title = list(text = "Date in ten year periods (BC)")) %>% 
      hc_add_series(name = "Number of triumps per decade", data = triumphs_data$no..of.triumphs,color = "#7fbf7b") %>% 
      hc_yAxis(title = list(text = "Number of triumphs per decade"))
    hc %>% hc_chart(type = "column", zoomType = "x")
  })
  
})