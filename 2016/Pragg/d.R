brow




plot_ly(data = livian_dist, type = "bar",
        x = Book,
        y = Rome.and.the.West) %>%
  add_trace(data = livian_dist,
            type = "bar",
            x = Book,
            y = East.of.the.Adriatic) %>%
  layout(barmode = "stack",barnorm = "percent")
