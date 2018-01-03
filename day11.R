#Day 11
setwd("C:/Users/David.simons/Documents/advent of code")

path <- factor(unlist(strsplit(readLines("day11.txt"), ",")), c("se", "sw", "ne", "nw", "s", "n"))

#part 1 ----
getDistance <- function(path){
  counts <- table(path)
  #s = se + sw (expand first to cancel later)
  counts[["se"]] <- counts[["se"]] + counts[["s"]]
  counts[["sw"]] <- counts[["sw"]] + counts[["s"]]
  counts[["s"]] <- 0
  #n = ne + nw (expand first to cancel later)
  counts[["ne"]] <- counts[["ne"]] + counts[["n"]]
  counts[["nw"]] <- counts[["nw"]] + counts[["n"]]
  counts[["n"]] <- 0
  #se + nw = 0
  if (counts[["se"]] > counts[["nw"]]) {
    counts[["se"]] <- counts[["se"]] - counts[["nw"]]
    counts[["nw"]] <- 0
  } else {
    counts[["nw"]] <- counts[["nw"]] - counts[["se"]]
    counts[["se"]] <- 0
  }
  #sw + ne = 0
  if (counts[["sw"]] > counts[["ne"]]) {
    counts[["sw"]] <- counts[["sw"]] - counts[["ne"]]
    counts[["ne"]] <- 0
  } else {
    counts[["ne"]] <- counts[["ne"]] - counts[["sw"]]
    counts[["sw"]] <- 0
  }
  #possibly done now if both wests or both easts "won". but if both norths or both souths "won" can combine them
  #se + sw = s
  counts[["s"]] <- min(counts[["sw"]], counts[["se"]])
  counts[c("sw","se")] <- counts[c("sw","se")] - min(counts[["sw"]], counts[["se"]])
  #ne + nw = n
  counts[["n"]] <- min(counts[["nw"]], counts[["ne"]])
  counts[c("nw","ne")] <- counts[c("nw","ne")] - min(counts[["nw"]], counts[["ne"]])
  
  # no more cancellations possible
  return(sum(counts))
}

print(getDistance(path))


#part 2 ----
tracking <- sapply(seq_along(path), function(i) getDistance(path[1:i])) #inefficent, but easiest way to re-use part 1
print(max(tracking))
