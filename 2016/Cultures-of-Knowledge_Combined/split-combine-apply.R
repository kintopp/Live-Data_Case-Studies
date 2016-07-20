### Make a nested list that contains three edges with relevant events for each individual

foo <- list("from" = c(1,2,7), "to" = c(4,5,8), "totalA" = c(10,12,32))

foo$events <- list(data.frame("x" = c(1,3), "y" = c(5,6)),
                   data.frame("x" = c(1,3), "y" = c(54,60)),
                   data.frame("x" = c(1,3), "y" = c(5,6)))

str(foo)


## Add data to the previous list

foo$from <- append(foo$from, 33)
foo$to <- append(foo$to, 45)
foo$totalA <- append(foo$totalA, 400)
foo$events <- append(foo$events, list(data.frame("x" = c(1,3), "y" = c(50,60))))

str(foo)

llply(foo$events, subset, y > 50)

## Split by y > 50
filtered.events <- llply(foo$events, subset, y > 50)
str(filtered.events)

unlist(filtered.events, recursive = FALSE)[[3]]

unlist(filtered.events, recursive = TRUE)

## Old

foo <- rbind(foo, list("from" = c(10,20,70), "to" = c(40,50,80), "totalA" = c(10,12,32),
                       "events" = list("one" = data.frame("x" = c(1,3), y = c(5,6)),
                                       "two" = data.frame("x" = c(1,3), y = c(5,6)),
                                       "three" = data.frame("x" = c(1,3), y = c(5,6)))))

str(foo)

foo$from

require(plyr)
llply(foo$events, subset, y > 5)





foo$b

foo[]
