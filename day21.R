#Day 21
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

rules <- strsplit(readLines("day21.txt"), " => ")
rules <- rbindlist(lapply(rules, function(rule){
  in1 <-  rule[1]
  out <- rule[2]
  base <- c(unlist(strsplit(gsub("/","", in1),NULL)), "/")
  if (length(base)==10) {
    in2 <- do.call(paste0, as.list(base[c(7,4,1,10,8,5,2,10,9,6,3)]))
    in3 <- do.call(paste0, as.list(base[c(9:7,10,6:4,10,3:1)]))
    in4 <- do.call(paste0, as.list(base[c(3,6,9,10,2,5,8,10,1,4,7)]))
    in5 <- do.call(paste0, as.list(base[c(3:1,10,6:4,10,9:7)]))
    in6 <- do.call(paste0, as.list(base[c(1,4,7,10,2,5,8,10,3,6,9)]))
    in7 <- do.call(paste0, as.list(base[c(7:9,10,4:6,10,1:3)]))
    in8 <- do.call(paste0, as.list(base[c(9,6,3,10,8,5,2,10,7,4,1)]))
  } else if (length(base)==5) {
    in2 <- do.call(paste0, as.list(base[c(3,1,5,4,2)]))
    in3 <- do.call(paste0, as.list(base[c(4,3,5,2,1)]))
    in4 <- do.call(paste0, as.list(base[c(2,4,5,1,3)]))
    in5 <- do.call(paste0, as.list(base[c(2,1,5,4,3)]))
    in6 <- do.call(paste0, as.list(base[c(1,3,5,2,4)]))
    in7 <- do.call(paste0, as.list(base[c(3,4,5,1,2)]))
    in8 <- do.call(paste0, as.list(base[c(4,2,5,3,1)]))
  }
  list(in1=in1,in2=in2,in3=in3,in4=in4,in5=in5,in6=in6,in7=in7,in8=in8,out=out)
}))

breakUp <- function(b){
  board <- strsplit(b, "/")[[1]]
  sz <- nchar(board[1])
  if (sz %in% 2:3) return(b) #short-circuit
  
  if (sz%%2 == 0) {
    subboards <- vector("character", sz*sz/4)
    r <- 1
    i <- 0
    while (r < sz) {
      c <- 1
      while (c < sz) {
        i <- i + 1
        subboards[i] <- paste(substr(board[r],c,c+1), substr(board[r+1],c,c+1), sep="/")
        c <- c + 2
      }
      r <- r + 2
    }
  } else {
    subboards <- vector("character", sz*sz/9)
    r <- 1
    i <- 0
    while (r < sz) {
      c <- 1
      while (c < sz) {
        i <- i + 1
        subboards[i] <- paste(substr(board[r],c,c+2), substr(board[r+1],c,c+2), substr(board[r+2],c,c+2), sep="/")
        c <- c + 3
      }
      r <- r + 3
    }
  }
  subboards
}

reform <- function(subboards){
  subboards <- strsplit(subboards, "/")
  nBlockRow <- sqrt(length(subboards))
  blockRow <- 1
  nRow <- nBlockRow * length(subboards[[1]])
  rows <- vector("character", nRow)
  i <- 0
  while (blockRow <= length(subboards)) {
    current <- subboards[blockRow:(blockRow+nBlockRow-1)]
    for (r in seq_along(current[[1]])) {
      i <- i + 1
      rows[i] <- do.call(paste0, lapply(current, function(x) x[r]))
    }
    blockRow <- blockRow + nBlockRow
  }
  
  do.call(paste, c(as.list(rows), sep="/"))
}

board <- ".#./..#/###"
rounds <- 0
nRounds <- 18 #part 1 is 5 rounds part 2 is 18 rounds

while (rounds < nRounds) {
  subboards <- breakUp(board)
  subboards <- sapply(subboards, function(x){
    rules[x==in1 | x==in2 | x==in3 | x==in4 | x==in5 | x==in6 | x==in7 | x==in8, out]
  }, USE.NAMES = F)
  board <- reform(subboards)
  rounds <- rounds + 1
  cat(sprintf("Completed round %d of %d\n", rounds, nRounds))
}

print(sum(unlist(strsplit(board, NULL))=="#")) #4-5 min for 18 rounds...i'll take it (R is slow at recursion)
