library(shiny)
library(plotly)

ethopia_schooling_youngOld <- read.csv(file = "data/ethopia_education_schooling_young-old-comparison.csv")
ethopia_schooling_region <- ethopia_schooling_youngOld[ethopia_schooling_youngOld$Property.Type == "schooling region",]

measure_list <- c("percentage.in.school", 
                  "highest.grade.completed.2006", "Percentage.children.over.age.for.grade", 
                  "percentage.attending.private.schools..", "Sample.Size")

measure_list <- setNames(measure_list, trimws(gsub("\\.", " ", measure_list)))

shinyServer(
  function(input, output, session){
    
    output$propertyTypeSelector <- renderUI({
      selectInput("propertyTypeSelected", label = "Select Property Type",
                  choices = levels(ethopia_schooling_youngOld$Property.Type))
    })
    
    output$measureSelector <- renderUI({
      selectInput("measureSelected", label = "Select Measure",
                  choices = measure_list)
    })
    
    output$measurePlot <- renderPlotly({
      
      if(is.null(input$measureSelected)){
        return()
      }
      
      if(is.null(input$propertyTypeSelected)){
        return()
      }
      
      filtered_data <- ethopia_schooling_youngOld[ethopia_schooling_youngOld$Property.Type == input$propertyTypeSelected,]
      
      
      plot_ly(data = filtered_data,
              type = "bar",
              x = Property,
              y = eval(as.name(input$measureSelected)),
              group = Cohort,
              orientation = "v") %>%
        layout(xaxis = list(title = input$propertyTypeSelected),
               yaxis = list(title = trimws(gsub("\\.", " ", input$measureSelected))))
    })
    
  }
)