#Day 19
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

#storing maze as dt not super fast, but convenient and speed is still tolerable
input <- strsplit(readLines("day19.txt"), NULL)
maze <- data.table(i = rep(1:length(input), each=length(input[[1]])),
                   j = rep(1:length(input[[1]]), times=length(input)),
                   val = unlist(input))

I <- 1
J <- maze[i==1 & val == "|", j]
dir <- "d"
msg <- character()
done <- F
steps <- 0

while (!done) {
  # check if at a letter
  ch <- maze[i==I & j==J, val]
  if (ch %in% LETTERS) msg <- c(msg, ch)
  
  #usually take one step/cycle. will add additional if crossing
  steps <- steps + 1 
  
  # set new position (I and J)
  if (dir == "d") {
    
    if (maze[i==I+1 & j==J, val] %in% c("|", "+", LETTERS)) {
      I <- I + 1
    } else if (maze[i==I+1 & j==J, val] == "-" && maze[i==I+2 & j==J, val] %in% c("|", "+", LETTERS)) {
      I <- I + 2
      steps <- steps + 1 #since 2 steps at once here
    } else {
      possible <- maze[i==I & abs(J-j)==1 & val %in% c("-", "+")]
      if (nrow(possible)==1) {
        dir <- if (possible$j > J) "r" else "l"
        J <- possible$j
      } else done <- T
    }
    
  } else if (dir == "u") {
    
    if (maze[i==I-1 & j==J, val] %in% c("|", "+", LETTERS)) {
      I <- I - 1
    } else if (maze[i==I-1 & j==J, val] == "-" && maze[i==I-2 & j==J, val] %in% c("|", "+", LETTERS)) {
      I <- I - 2
      steps <- steps + 1 #since 2 steps at once here
    } else {
      possible <- maze[i==I & abs(J-j)==1 & val %in% c("-", "+")]
      if (nrow(possible)==1) {
        dir <- if (possible$j > J) "r" else "l"
        J <- possible$j
      } else done <- T
    }
    
  } else if (dir == "l") {
    
    if (maze[i==I & j==J-1, val] %in% c("-", "+", LETTERS)) {
      J <- J - 1
    } else if (maze[i==I & j==J-1, val] == "|" && maze[i==I & j==J-2, val] %in% c("-", "+", LETTERS)) {
      J <- J - 2
      steps <- steps + 1 #since 2 steps at once here
    } else {
      possible <- maze[abs(i-I)==1 & j==J & val %in% c("|", "+")]
      if (nrow(possible)==1) {
        dir <- if (possible$i > I) "d" else "u"
        I <- possible$i
      } else done <- T
    }
    
  } else if (dir == "r") {
    
    if (maze[i==I & j==J+1, val] %in% c("-", "+", LETTERS)) {
      J <- J + 1
    } else if (maze[i==I & j==J+1, val] == "|" && maze[i==I & j==J+2, val] %in% c("-", "+", LETTERS)) {
      J <- J + 2
      steps <- steps + 1 #since 2 steps at once here
    } else {
      possible <- maze[abs(i-I)==1 & j==J & val %in% c("|", "+")]
      if (nrow(possible)==1) {
        dir <- if (possible$i > I) "d" else "u"
        I <- possible$i
      } else done <- T
    }
    
  }
}

#part  1
print(do.call(paste0, as.list(msg)))

#part 2
print(steps)




