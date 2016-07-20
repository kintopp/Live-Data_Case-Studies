library(shiny)
library(visNetwork)
library(igraph)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Source used by Courts"),
  
  wellPanel(
    HTML("<p>This visualisation demonstrates how five different courts referenced judicial content and other court decisions.</p>
         <p>There are five courts and a total of 68 different types of judicial content they could refer to</p>
         <p>In the dataset behind this visualisation there are multiple cases per court, courts are <em>connected</em> to a judicial source of at least one decision refers to at least n sources of that type</p>")
  ),
  
  # Left-Panel containing controls
  sidebarLayout(
    sidebarPanel(

      sliderInput("minimum.references", 
                  "Minimum number of references required:", 
                  value = 2,
                  min = 1, 
                  max = 50)
    ),
    
    mainPanel(
      visNetworkOutput("judgement_bipartite_graph")
    )
  )
))