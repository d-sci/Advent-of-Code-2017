#Day 22
setwd("C:/Users/David.simons/Documents/advent of code")
library(data.table)

#use data.table just to read in input easily; will convert to matrix
input <- readLines("day22.txt")
input <- rbindlist(mapply(function(row, n){
  data.table(i = n, j = seq(nchar(row)), val = strsplit(row, NULL)[[1]])
},input, seq_along(input), SIMPLIFY = F))


#part 1 ----
input[, inf := val=="#"] #mark infected as boolean

#much faster to keep as (pre-allocated) matrix vs data.table with I and J (hope 1000x1000 is enough)
space <- matrix(data=F, nrow=1000, ncol=1000)
for (x in 1:nrow(input)) {
  space[input[x,i+500], input[x,j+500]] <- input[x,inf]
}

#start in the middle of input going up
I <- median(input$i)+500
J <- median(input$j)+500
dir <- 0 #0 = up,  1 = right, 2 = down, 3 = left

counter <- 0
bursts <- 10000
for (burst in seq(bursts)) {
  thisInf <- space[I,J]
  dir <- ifelse(thisInf, (dir+1) %% 4, (dir-1) %% 4)
  if (!thisInf) counter <- counter + 1
  space[I,J] <- !thisInf
  if (dir==0) I <- I - 1 else if (dir==2) I <- I + 1 else if (dir==1) J <- J + 1 else if (dir==3) J <- J - 1
}

print(counter)


# part 2 ----
input[, val := ifelse(val=="#", 1, -1)] #mark states: -1 = clean, 0 = weakened, 1 = infected, 2 = flagged

#much faster to keep as (pre-allocated) matrix vs data.table with I and J (bit bigger here to be safe)
space <- matrix(data=-1, nrow=5000, ncol=5000)
for (x in 1:nrow(input)) {
  space[input[x,i+2000], input[x,j+2000]] <- input[x,val]
}

#start in the middle of input going up
I <- median(input$i)+2000
J <- median(input$j)+2000
dir <- 0 #0 = up,  1 = right, 2 = down, 3 = left

counter <- 0
bursts <- 10000000
for (burst in seq(bursts)) {
  thisVal <- space[I,J]
  dir <- (dir + thisVal) %% 4
  if (thisVal==0) counter <- counter + 1
  space[I,J] <- ifelse(thisVal==2, -1, thisVal+1)
  if (dir==0) I <- I - 1 else if (dir==2) I <- I + 1 else if (dir==1) J <- J + 1 else if (dir==3) J <- J - 1
}

print(counter)

