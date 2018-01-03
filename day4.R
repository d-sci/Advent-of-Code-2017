#Day 4
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

phrases <- readLines("day4.txt")
asWords <- strsplit(phrases, " ")

#part 1 ----
validate <- sapply(asWords, function(w) length(w) == length(unique(w)))
print(sum(validate))


#part2 ----
validate <- sapply(asWords, function(w){
  asLetters <- lapply(strsplit(w, NULL), sort)
  return(length(asLetters) == length(unique(asLetters)))
})

print(sum(validate))
