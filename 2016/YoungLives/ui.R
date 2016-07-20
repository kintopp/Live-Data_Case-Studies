library(shiny)
library(plotly)
shinyUI(fluidPage(uiOutput("propertyTypeSelector"),
                  uiOutput("measureSelector"),
                  plotlyOutput("measurePlot")))