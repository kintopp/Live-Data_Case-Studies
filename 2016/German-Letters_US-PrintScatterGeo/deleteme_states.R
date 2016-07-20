## Restrict entries_with_locations to only those where the the letter was sent from the USA
entries_with_locations <-
  entries_with_locations[entries_with_locations$Sender.Country == "USA", ]

## Filter out those entries without a state, i.e. New York (NY)
entries_with_locations <-
  entries_with_locations[grepl("[(][A-Z]{2}[)]", entries_with_locations$Sender.Location), ]

## Create a vector containing send states"
get_states_from_column <- function(data){
  str_extract(string = data, pattern = "[(][A-Z]{2}[)]") %>% 
    str_split(pattern = "[(]|[)]") %>% 
    sapply("[[", 2)
}

## Add to the entries_with_locations
entries_with_locations$Sender.State <- get_states_from_column(entries_with_locations$Sender.Location)


entries_with_locations$Date


## ======================================= Tally Number in each state =============================================

state_tallies <- as.data.frame(table(entries_with_locations$Sender.State))
colnames(state_tallies) <- c("State","Letters.Sent")


## ======================================  Choroplepth ================

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = "999999",
  showsubunits = TRUE,
  subunitcolor = toRGB("black")
)

plot_ly(
  data = state_tallies,
  z = Letters.Sent,
  autocolorscale = TRUE,
  locations = State,
  type = 'choropleth',
  locationmode = 'USA-states',
  # color = Letters.Sent,
  colors = 'Purples',
  # marker = list(line = l),
  colorbar = list(title = "Number of Letters", tickmode = "auto"),
  hoverinfo = "text",
  text = paste0(
    "Location Name: ",
    State,
    "<br>",
    "Letters sent from location: ",
    Letters.Sent
  )
) %>%
  layout(title = 'Letters sent by German Migrants in the USA', geo = g)



