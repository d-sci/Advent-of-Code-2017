#Day 13
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

input <-  strsplit(readLines("day13.txt"), ": ")

#part 1 ----  
wall <- rbindlist(lapply(input, function(line) {
  list(depth = as.integer(line[1]), range = as.integer(line[2]))
}))

wall[, severity := ifelse(depth %% ((range-1)*2) == 0, depth*range, NA)]
print(sum(wall$severity, na.rm=T))


#part 2 ----

#actually much faster if list of lists vs dt!
wall <- lapply(input, function(line) {
  list(depth = as.integer(line[1]), range = as.integer(line[2]))
})

caught <- function(delay) {
  for (x in wall) {
    if ((x$depth + delay) %% ((x$range-1)*2) == 0) return(T) #speed vs dt likely due to early stopping
  }
  return(F)
}

delay <- 0
while (caught(delay)) delay <- delay + 1 #kinda "brute force" but relatively efficient; takes ~24s
print(delay)



