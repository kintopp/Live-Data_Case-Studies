## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

library(shiny)
library(highcharter)

navbarPage(
  "Prag Visualisations",
  tabPanel(
    "Livian Distribution",
    fluidPage(
      wellPanel("Lorem ipsum"),
      fluidRow(column(
        selectInput(
          "livian_dist_stack_type",
          label = "Show as:",
          choices = list(
            "Absolute Number of XX:" = "normal",
            "Relative Percentage of XX" = "percent"
          )
        ),
        width = 6
      ),
      column(
        selectInput(
          "livian_dist_orderby",
          label = "Order bars by:",
          choices = list(
            "Book" = "Book",
            "Rome and the West" = "Rome.and.the.West",
            "East of the Adriatic" = "East.of.the.Adriatic"
          )
        ),
        width = 6
      )),
      highchartOutput("livian_dist_highchart")
    )
  ),
  tabPanel("History",
           fluidPage(
             wellPanel("Lorem ipsum"),
             tabsetPanel(
             tabPanel("Legions", highchartOutput("legions_highchart")),
             tabPanel("Triumphs", highchartOutput("triumphs_highchart"))
           )))
)