#Day 6
setwd("C:/Users/David.simons/Documents/advent of code")

blocks <- as.integer(unlist(strsplit(readLines("day6.txt"), "\t")))
n <- length(blocks)
state <- do.call(paste,as.list(blocks))
history <- character(0)
cycles <- 0

while (!state %in% history) {
  history <- c(history, state)
  cycles <- cycles + 1
  biggest <- which.max(blocks)
  i <- biggest
  while (blocks[biggest] > 0) {
    i <- i + 1
    if (i > n) i <- i - n
    blocks[i] <- blocks[i] + 1
    blocks[biggest] <- blocks[biggest] - 1
  }
  state <- do.call(paste, as.list(blocks))
}

#part 1
print(cycles)

#part 2
print(cycles - match(state, history) + 1)
