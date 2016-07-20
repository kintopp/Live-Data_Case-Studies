## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

fluidPage(
  HTML(
    "<h2>Writing home: how German immigrants found their place in the US</h2>"
  ),
  #     fluidRow(
  #     column(HTML("Filter letters by date sent?"), width = 3),
  #     column(uiOutput("show_timeslider_UI"), width = 2)
  #     ),
  wellPanel(
    # uiOutput("show_letters_where_receive_unknown_UI"),
    # uiOutput("show_timeslider_UI"),
    # uiOutput("legend_type_UI"),
    # fluidRow(column(
    #   uiOutput("time_period_of_interest_UI"),
    #   width = 12
    # )),
    HTML("<p>This map displays the origins of a subsample of 1,000 letters used for the research project “Political Remittances: Understanding the Political Impacts of Migration”. 
         The project is funded through the Leverhulme Trust (Ref-No RPG-338) and based at the University of Oxford, Department of Politics and International Relations. The research 
         conducted by Gwendolyn Sasse and Félix Krawatzek uses a total collection of 6,000 letters sent between the US and Germany between around 1830 and 1970. Permission to use the 
         collection has been granted by Prof. Ursula Lehmkuhl, University of Trier, and the Research Library Gotha.
         </p>",
         "<p>The right-hand side column displays the different places from which letters of a particular series were sent. You can unselect any single or multiple series by clicking on 
         the name of the series. You can also get an understanding of the frequency of letters over time by clicking the above “Filter letters by date sent”. 
         An article in <a href='https://theconversation.com/writing-home-how-german-immigrants-found-their-place-in-the-us-53342'>The Conversation</a> allows you to gain some insight into the research linked to this visualisation.
         </p>",
         "<p>Note that this map is a first working version on ways to visualise the overall corpus. Any comments or suggestions you may have are very welcome. Please get in touch with 
         felix.krawatzek(at)politics.ox.ac.uk.
         </p>")
    ),
  
  fluidRow(
    column(
      plotlyOutput("america_period_1_Map"), width = 4),
    column(
      plotlyOutput("america_period_2_Map"), width = 4),
    column(
      plotlyOutput("america_period_3_Map"), width = 4)
  )
  
  # fluidRow(
  #   column(
  #     "", width = 2, offset = 1),
  #   column(
  #     plotlyOutput("america_period_1_Map"), width = 8, offset = 2),
  #   column(
  #     "", width = 2, offset = 1)
  # ),
  # fluidRow(
  #   column(
  #     "", width = 2, offset = 1),
  #   column(
  #     plotlyOutput("america_period_2_Map"), width = 8, offset = 2),
  #   column(
  #     "", width = 2, offset = 1)
  # ),
  # fluidRow(
  #   column(
  #     "", width = 2, offset = 1),
  #   column(
  #     plotlyOutput("america_period_3_Map"), width = 8, offset = 2),
  #   column(
  #     "", width = 2, offset = 1)
  # )
  
  # fluidRow(
  #   column(
  #     plotlyOutput("america_period_1_Map"),
  #     plotlyOutput("america_period_2_Map"),
  #     width = 6
  #   ),
  #   column(
  #     plotlyOutput("america_period_3_Map"),
  #     plotlyOutput("america_period_4_Map"),
  #     width = 6
  #   )
  # )
  ## america map ##
  # uiOutput("americamap_via_renderUI")
    )