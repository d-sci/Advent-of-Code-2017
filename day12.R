#Day 12
setwd("C:/Users/David.simons/Documents/advent of code")

input <- strsplit(readLines("day12.txt"), " <-> ")
mapping <- lapply(input, function(line) unlist(strsplit(line[2], ", ")))
names(mapping) <- sapply(input, '[', 1)


#part 1 ----
findGroup <- function(base){
  group <- base
  toCheck <- base
  while (length(toCheck) > 0) {
    x <- mapping[[toCheck[1]]]
    toCheck <- c(toCheck, x[(!x %in% toCheck) & (!x %in% group)])
    group <- c(group, x[!x %in% group])
    toCheck <- toCheck[-1]
  }
  group
}

print(length(findGroup("0")))


#part 2 ----
groups <- list(findGroup("0"))
for (item in names(mapping)) {
  presence <- sapply(groups, function(g) item %in% g)
  if (!any(presence)) groups <- c(groups, list(findGroup(item)))
}

print(length(groups))

