#Day 7
setwd("C:/Users/David.simons/Documents/advent of code")

input <- strsplit(readLines("day7.txt"), " ")

progs <- lapply(input, function(p){
  name <- p[1]
  weight <- as.integer(gsub("[()]", "", p[2]))
  if (length(p) > 2) {
    carrying <- gsub(",", "", p[4:length(p)])
  } else carrying <- NULL
  list(name=name, weight=weight, carrying=carrying)
})
names(progs) <- sapply(progs, function(p) p$name)

#part 1 ----
carried <- unlist(sapply(progs, function(p) p$carrying))
base <- names(progs)[!names(progs) %in% carried]
print(base)


#part 2 ----
getTotalWeight <- function(name){
  total <- progs[[name]]$weight
  for (x in progs[[name]]$carrying) {
    total <- total + getTotalWeight(x)
  }
  return(total)
}

for (n in names(progs)) {
  progs[[n]]$totalWeight <- getTotalWeight(n)
}

findProblem <- function(progs) {
  for (i in seq_along(progs)) {
    if (!is.null(progs[[i]]$carrying)) {
      carryWeights <- sapply(progs[[i]]$carrying, function(x) progs[[x]]$totalWeight)
      if (any(carryWeights != mean(carryWeights))) {
        return(carryWeights)
      }
    }
  }
}

print(findProblem(progs))
#then more manually check..
print(sapply(progs[["tulwp"]]$carrying, function(x) progs[[x]]$totalWeight)) #all his are fine; he's the problem
print(progs[["tulwp"]]$weight - 8) #final answer
