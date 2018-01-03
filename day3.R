#Day 3
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

input <- 277678

#part 1 ----
getLocation <- function(input) {
  n <- 1
  x <- 0
  y <- 0
  step <- 0
  while (n <= input) {
    step <- step + 1
    direction <- ifelse(step%%2 == 1, 1, -1)
    for (time in 1:step) {
      n <- n + 1
      x <- x + 1*direction
      if (n==input) return(list(x=x, y=y))
    }
    for (time in 1:step){
      n <- n + 1
      y <- y + 1*direction
      if (n==input) return(list(x=x, y=y))
    }
  }
}

loc <- getLocation(input)
print(abs(loc$x) + abs(loc$y))


#part 2 ----
getAnswer <- function(input) {
  n <- 1
  x <- 0
  y <- 0
  space <- data.table(N=n, X=x, Y=y)
  step <- 0
  while (TRUE) {
    step <- step + 1
    direction <- ifelse(step%%2 == 1, 1, -1)
    for (time in 1:step) {
      x <- x + 1*direction
      n <- space[abs(X - x) <= 1 & abs(Y - y) <= 1, sum(N)]
      space <- rbindlist(list(space, list(N=n, X=x, Y=y)))
      if (n >= input) return(n)
    }
    for (time in 1:step) {
      y <- y + 1*direction
      n <- space[abs(X - x) <= 1 & abs(Y - y) <= 1, sum(N)]
      space <- rbindlist(list(space, list(N=n, X=x, Y=y)))
      if (n >= input) return(n)
    }
  }
}

print(getAnswer(input))
