
tabPanel("Recipe Visualisations",
         wellPanel(HTML(
           paste(
             "<h1>Visualising the Difference Between Cultural Cuisines </h1>",
             "<p>This Shiny app analyses and visualises the recipes data made available in 
<a href='http://www.nature.com/srep/2011/111215/srep00196/full/srep00196.html'>Flavor network and the principles of food pairing</a>,
             published by Nature Publishing Group. 
            The original data and findings of the research are fully attributed to Nature Publishing Group and the authors; Yong-Yeol Ahn, 
            Sebastian E. Ahnert, James P. Bagrow, Albert-László Barabási</p>",
             "<p>The data concerns recipes from different cuisines</p>",
             sep = ""
           )
         )),
         plotlyOutput("recipes_per_cuisine_plot", width = "100%", height = "300px"),
         plotlyOutput("length_of_recipe_boxwhisker", width = "100%")
         # plotlyOutput("length_of_recipe_histogram", width = "100%", height = "300px")
         )