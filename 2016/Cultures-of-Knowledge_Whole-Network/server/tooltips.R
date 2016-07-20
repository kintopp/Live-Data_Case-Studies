## =========================== Tooltips ====================================
## ==============================================================================

output$tooltip_highlight_node <- renderUI({
  tags$span(
    popify(bsButton("pointlessButton", "i", style = "info", size = "large"),
           "A Pointless Button",
           "This button is <b>pointless</b>. It does not do <em>anything</em>!")
  )
})