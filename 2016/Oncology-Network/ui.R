library(shiny)
library(igraph)
library(visNetwork)

shinyUI(fluidPage(
  wellPanel("Extreme Proof of Concept of the CRUK Network at Oxford University"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "layout_control",
        choices = c(
          "layout_nicely",
          "layout_with_fr"
        ),
        selected = "layout_nicely",
        multiple = FALSE,
        label = "Layout"
      ),
      selectInput("people_or_departments",
                  label = "Display:",
                  choices = c("individuals","departments")),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      visNetworkOutput("crukNetwork", width = "100%")
    )
  ),
  uiOutput("ui_containing_a_dt")
  # wellPanel(
  #   DT::dataTableOutput("selected_node_table")
  # )
))