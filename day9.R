#Day 9
setwd("C:/Users/David.simons/Documents/advent of code")

input <- readLines("day9.txt")

#part 1----
getTotal <- function(input) {
  input <- gsub("!.", "", input) #remove invalidation
  input <- gsub("<.*?>","", input) #remove garbage (minimally)
  input <- gsub(",", "", input) #remove commas
  chars <- unlist(strsplit(input, NULL))
  total <- 0
  level <- 0
  for (c in chars) {
    if (c == "{") {
      level <- level + 1
    } else {
      total <- total + level
      level <- level - 1
    }
  }
  total
}

print(getTotal(input))


#part 2 ----
countGarbage <- function(input) {
  input <- gsub("!.", "", input) #remove invalidation
  chars <- unlist(strsplit(input, NULL))
  total <- 0
  open <- F
  for (c in chars) {
    if (c == "<" && !open) open <- T
    else if (c == ">" && open) open <- F
    else if (open) total <- total + 1
  }
  total
}

print(countGarbage(input))

