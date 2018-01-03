#Day 24
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

chunks <- rbindlist(lapply(strsplit(readLines("day24.txt"), "/"), function(x) as.list(as.numeric(x))))
chunks[, id := 1:nrow(chunks)]

#part 1 ----

#recursively find strengths of all maximally long bridges
strongestBridge <- function(startWith, used) {
  possible <- chunks[(V1==startWith | V2==startWith) & !(id %in% used)]
  if (nrow(possible)==0) return(chunks[id %in% used, sum(V1) + sum(V2)]) #base case; we've gone as far as possible
  
  strengths <- sapply(seq(nrow(possible)), function(i){
    val <- if (possible[i, V1]==startWith) possible[i, V2] else possible[i, V1]
    strongestBridge(val, c(used, possible[i,id]))
  })
  
  return(max(strengths))
}

#recursion is slow in R but I'm satisfied
print(strongestBridge(0, NULL))


#part 2 ----

#same as part 1, but need to save both strength and length
longestBridge <- function(startWith, used) {
  possible <- chunks[(V1==startWith | V2==startWith) & !(id %in% used)]
  if (nrow(possible)==0) return(list(len=length(used), strength=chunks[id %in% used, sum(V1) + sum(V2)])) #base case; we've gone as far as possible
  
  bridges <- rbindlist(lapply(seq(nrow(possible)), function(i){
    val <- if (possible[i, V1]==startWith) possible[i, V2] else possible[i, V1]
    longestBridge(val, c(used, possible[i,id]))
  }))
  
  return(list(len=bridges[,max(len)], strength=bridges[len==max(len), max(strength)]))
}

#recursion is slow in R but I'm satisfied
print(longestBridge(0, NULL)$strength)



