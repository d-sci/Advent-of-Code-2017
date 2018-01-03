#Day 5
setwd("C:/Users/David.simons/Documents/advent of code")

#part 1 ----
maze <- as.integer(readLines("day5.txt"))

n <- length(maze)
x <- 1
steps <- 0

while (x <= n) {
  stepSize <- maze[x]
  maze[x] <-  maze[x] + 1
  x <- x + stepSize
  steps <- steps + 1
}

print(steps)


#part 2 ----
maze <- as.integer(readLines("day5.txt"))

n <- length(maze)
x <- 1
steps <- 0

while (x <= n) {
  stepSize <- maze[x]
  maze[x] <-  ifelse(stepSize >= 3, stepSize - 1, stepSize + 1)
  x <- x + stepSize
  steps <- steps + 1
}

print(steps)