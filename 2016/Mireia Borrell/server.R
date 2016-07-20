library(ggplot2)
library(shiny)
library(DT)


source("data-processing.R", local = T)

shinyServer(
  function(input, output, session){
    
    output$timeline <- renderPlot({
      
      ggplot(timeline_data, aes(x=Valid.From, xend=Valid.To, y=Name.of.Policy, yend=Name.of.Policy, colour=Type)) +
        geom_segment(size=3) + 
        xlab("Date") + ylab("Name of Policy")
      
    })
    
    selected_row_of_ordered_policies <- reactive({
      print("ffoo")
      row <- round(length(levels(timeline_data$Name.of.Policy)) + 1 - input$timeline_click$y)
      row
    })
    

    output$data_table <- DT::renderDataTable({
      if (is.null(input$timeline_click)){
        return()
      }
      
      selected_row_of_ordered_policies <- selected_row_of_ordered_policies()
      selected_policy <- earliest_date_by_Name_of_Policy[selected_row_of_ordered_policies,]$Name.of.Policy
      timeline_data[timeline_data$Name.of.Policy == selected_policy,]
    })
    
  }
)