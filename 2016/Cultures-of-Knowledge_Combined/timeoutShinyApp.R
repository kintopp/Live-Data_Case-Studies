# shinyServer(function(input, output, session) {
#   output$plot <- renderPlot({
#     progress <- shiny::Progress$new(session, min=1, max=15)
#     on.exit(progress$close())
# 
#     progress$set(message = 'Calculation in progress',
#                  detail = 'This may take a while...')
# 
#     for (i in 1:15) {
#       progress$set(value = i)
#       Sys.sleep(0.5)
#     }
#     plot(cars)
#   })
# })
# ## End(Not run)