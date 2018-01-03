#Day 17
setwd("C:/Users/David.simons/Documents/advent of code")

#part 1 ----
step <- 386
buffer <- 0:1
pos <- 2

for (i in 2:2017) {
  pos <- (pos + step) %% i #i is current length of array!
  if (pos == 0) {
    buffer <- c(buffer, i)
    pos <- i + 1
  } else {
  buffer <- c(buffer[1:pos], i, buffer[(pos+1):i])
  pos <- pos + 1
  }
}

print(buffer[match(2017, buffer) + 1])


#part 2 ----

#just need to know what ends up in buffer[2] (since 0 will always stay at beginning)
#i.e. need to know last number to set pos to 1
#still brute-force ish but much faster than actually creating the buffer (~2min total)
current <- buffer[2]
for (i in 2018:50e6) {
  pos <- (pos + step) %% i
  if (pos == 1) current <- i
  pos <- ifelse(pos == 0, i + 1, pos + 1)
}

print(current)

