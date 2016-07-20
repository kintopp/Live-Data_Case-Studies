### ============= Useful Visualisations Tools ========================= ###

## ggplot Color Function from http://stackoverflow.com/a/8197703/1659890
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


### ============= Landing Tab Visualisations and UI ========================= ###


output$recipes_per_cuisine_plot <- renderPlotly({
  
  cuisine_tally <- count(recipes.df$Cuisine)
  colnames(cuisine_tally) <- c("Cuisine","Freq")

  p <- plot_ly(
    cuisine_tally,
    x = Freq,
    y = Cuisine,
    name = "",
    type = "bar",
    orientation = "h"
  )
  layout(
    p,
    title = "Distribution of recipes across cuisines",
    xaxis = list(title = "Number of recipes (log)", domain = list(0.1, 1), type = "log"),
    yaxis = list(title = "")
  )
})

### === Recipe Lengths

output$length_of_recipe_boxwhisker <- renderPlotly({
## Box whisker chart
  
plot_ly(length_of_recipes.df, y = Length, color = Cuisine, type = "box",
        orientation = "v",
        boxmean = TRUE,
        showlegend = FALSE) %>%
    layout(margin = list(b = 100, r = 30, t = 50),
           yaxis = list(title = "Number of Ingredients"),
           title = "Distribution of Recipe Lengths Across Cuisines")
  
})


### ============= visNetwork ========================= ###
### ====================================== ###




### ============= UNUSED ========================= ###
### ====================================== ###

output$length_of_recipe_histogram <- renderPlotly({
  p <-
    plot_ly(
      x = length_of_recipes.df[length_of_recipes.df$Cuisine == "NorthAmerican",2],
      opacity = 0.6,
      type = "histogram",
      name = "North American",
      histnorm = "percent"
      # xbins = list(start = 1, end = 500, size = 2)
    )
  p <- add_trace(p, x = length_of_recipes.df[length_of_recipes.df$Cuisine == "African",2], 
                 name = "African",opacity = 0.6
  )
  p <- add_trace(p, x = length_of_recipes.df[length_of_recipes.df$Cuisine == "EastAsian",2], 
                 name = "East Asian",opacity = 0.6
  )
  layout(
    p, barmode = "overlay", xaxis = list(title = "Number of Ingredients"),
    yaxis = list(title = "Percentage of Recipes", domain = c(0,1)),
    title = "Distribution of Recipe Length<br> (Bin Width: 2 USD, Cutoff: 500 USD)",
    margin = list(t = 100)
  )
})


ggplot(length_of_recipes.df, aes(Cuisine, Length)) + geom_violin() +
  coord_flip()

ggplotly(ggplot(length_of_recipes.df, aes(Cuisine, Length)) + geom_boxplot())


### ============= ggplot histogram ========================= ###

ggplot(data = length_of_recipes.df, aes(Length, ..density.., fill = Cuisine)) + 
  geom_histogram(binwidth = 2)

ggplotly(ggplot(data = length_of_recipes.df, aes(Length, ..density.., fill = Cuisine)) + 
  geom_histogram(binwidth = 1) +
  scale_fill_brewer(palette = "Set1"))

ggplotly(ggplot(data = length_of_recipes.df, aes(Length, ..density.., fill = Cuisine)) + 
  geom_histogram(binwidth = 2))

str
library(ggplot2movies)
m <- ggplot(movies, aes(rating))
m + geom_histogram(aes(weight = votes), binwidth = 0.1) + ylab("votes")


