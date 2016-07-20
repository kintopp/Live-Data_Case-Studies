

# fluidPage(
#   fluidRow(
#     wellPanel(HTML(
#       "<h2>The ‘New’ Germans: Rethinking Integration by understanding the
#          Historical Experience of German Migrants in the US</h2>"
#     )),
#     column(uiOutput("show_timeslider_UI"),
#            width = 2),
#     column(uiOutput("time_period_of_interest_UI"),
#            width = 10)
#   ),
#   plotlyOutput("world_map", width = "100%", height = "800px")
# )
#

fluidPage(
    HTML(
      "<h2>What does immigrant integration (not) mean? Reconsidering integration through the eyes of German immigrants in the US </h2>"
    ),
  #     fluidRow(
  #     column(HTML("Filter letters by date sent?"), width = 3),
  #     column(uiOutput("show_timeslider_UI"), width = 2)
  #     ),
  wellPanel(
    uiOutput("show_letters_where_receive_unknown_UI"),
  uiOutput("show_timeslider_UI"),
  # uiOutput("legend_type_UI"),
  fluidRow(column(
    uiOutput("time_period_of_interest_UI"),
    width = 12
  )),
  HTML("<p>This map displays the origins of a subsample of 1,000 letters used for the research project “Political Remittances: Understanding the Political Impacts of Migration”. 
The project is funded through the Leverhulme Trust (Ref-No RPG-338) and based at the University of Oxford, Department of Politics and International Relations. The research 
       conducted by Gwendolyn Sasse and Félix Krawatzek uses a total collection of 6,000 letters sent between the US and Germany between around 1830 and 1970. Permission to use the 
       collection has been granted by Prof. Ursula Lehmkuhl, University of Trier, and the Research Library Gotha.
       </p>",
       "<p>The right-hand side column displays the different places from which letters of a particular series were sent. You can unselect any single or multiple series by clicking on 
       the name of the series. You can also get an understanding of the frequency of letters over time by clicking the above “Filter letters by date sent”. 
       An article in “The Converstaion” (HYPERLINK) allows you to gain some insight into the research linked to this visualisation.
       </p>",
       "<p>Note that this map is a first working version on ways to visualise the overall corpus. Any comments or suggestions you may have are very welcome. Please get in touch with 
       felix.krawatzek(at)politics.ox.ac.uk.
       </p>")
  ),
  plotlyOutput("world_map", width = "100%", height = "1400px")
  )
