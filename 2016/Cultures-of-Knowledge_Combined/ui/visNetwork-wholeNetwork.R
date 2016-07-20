


tabPanel(
  "visNetwork - Whole Network",
  wellPanel(HTML(
    paste(
      "<h1>Proof of Concept</h1>",
      "<p>The graph below is a proof of concept for visualising EMLO data.</p>",
      "<p>The following features are provided:</p>",
      "<ul>",
      "<li>Tooltips on vertices (showing name of individual, this can be customised)</li>",
      "<li>The controls on the left allow the user to select the categories/event types they are interested in</li>",
      "<li>Click a node highlights connected individuals, scroll down to see information</li>",
      "</ul>",
      sep = ""
    )
  )),
  
  fluidRow(
    column(
      width = 4,
      # uiOutput("visNetwork_wholeNetwork_highlighted_node_UI"),
      wellPanel(
        uiOutput("visNetwork_wholeNetwork_highlighted_node_UI"),
        uiOutput("visNetwork_wholeNetwork_show_timeslider_UI"),
        uiOutput("visNetwork_wholeNetwork_dateRangeInput_UI"),
        uiOutput("visNetwork_wholeNetwork_time_period_of_interest_UI"),
        uiOutput("visNetwork_wholeNetwork_HighlightedCategoryUI"),
        uiOutput("visNetwork_wholeNetwork_ExcludedCategoriesUI"),
        uiOutput("visNetwork_wholeNetwork_NumberOfExcluded")
      ),
      conditionalPanel(
        "typeof input.current_node_id !== 'undefined'",
        uiOutput("visNetwork_wholeNetwork_selected_node_info")
      )
    ),
    column(
      width = 8,
      visNetworkOutput(
        "visNetwork_wholeNetwork",
        width = "100%",
        height = "600px"
      )
    )
  ),
  
  conditionalPanel(
    "typeof input.current_node_id !== 'undefined'",
    uiOutput(
      "visNetwork_whole_network_connected_life_events_columns_to_show_UI"
    ),
    dataTableOutput("visNetwork_whole_network_selected_node")
  )
  
  
)