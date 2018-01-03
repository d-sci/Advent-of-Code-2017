#Day 14
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

#part 1 ----
inputs <- paste0("xlqgujun-", 0:127)
hashInputs <- lapply(inputs, function(input) c(R.oo::charToInt(unlist(strsplit(input,NULL))), 17,31,73,47,23))

getHash <- function(lengths, lst=0:255){
  #get sparse hash (as lst)
  n <- length(lst)
  pos <- 1
  skip <- 0
  for (x in rep(lengths, 64)) {
    start <- pos
    end <- pos + x - 1
    if (end <= n) select <- start:end else select <- c(start:n, 1:(end-n))
    lst[select] <- rev(lst[select])
    pos <- pos + x + skip
    while (pos > n) pos <- pos - n
    skip <- (skip + 1) %% n
  }
  #do extra hash stuff
  denseHash <- sapply(split(lst, rep(1:16, each=16)), function(sub) Reduce(bitwXor, sub))
  do.call(paste0, as.list(as.character(as.hexmode(denseHash), width=2)))
}

mapping <- sapply(hashInputs, function(x) BMS::hex2bin(getHash(x)))

print(sum(mapping))


#part 2 ----

#use data.table for convenience; ends up being fast enough for this
grouping <- data.table(i = rep(1:128, each=128), j = rep(1:128, 128), group = NA_integer_)
grouping <- grouping[, filled := mapping[i,j]==1, by=1:nrow(grouping)][filled==T]

maxGroup <- 0

while (grouping[is.na(group), .N] > 0) {
  #assign new group to first missing guy
  maxGroup <- maxGroup + 1
  x <- grouping[is.na(group), which=T][1]
  grouping[x, group := maxGroup] 
  #tag his whole group
  x <- grouping[x]
  toCheck <- grouping[is.na(group) & ((i==x$i & abs(j-x$j)<=1) | (j==x$j & abs(i-x$i)<=1)), which=T]
  while (length(toCheck) > 0) {
    y <- toCheck[1]
    grouping[y, group := maxGroup]
    y <- grouping[y]
    more <- grouping[is.na(group) & ((i==y$i & abs(j-y$j)<=1) | (j==y$j & abs(i-y$i)<=1)), which=T]
    toCheck <- c(toCheck[-1], more[!more %in% toCheck])
  }
}

print(maxGroup)
