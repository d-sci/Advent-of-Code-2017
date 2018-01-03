#Day 10
setwd("C:/Users/David.simons/Documents/advent of code")

#part 1 ----
lengths <- c(34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167)

getResult <- function(lengths, lst=0:255){
  n <- length(lst)
  pos <- 1
  skip <- 0
  for (x in lengths) {
    start <- pos
    end <- pos + x - 1
    if (end <= n) select <- start:end else select <- c(start:n, 1:(end-n))
    lst[select] <- rev(lst[select])
    pos <- pos + x + skip
    while (pos > n) pos <- pos - n
    skip <- skip + 1
  }
  lst[1]*lst[2]
}

print(getResult(lengths))


#part 2 ----
input <- "34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167"
lengths <- c(R.oo::charToInt(unlist(strsplit(input,NULL))), 17,31,73,47,23)

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

print(getHash(lengths))


