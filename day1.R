#Day 1
setwd("C:/Users/David.simons/Documents/advent of code")

input <- as.integer(unlist(strsplit(readLines("day1.txt"), NULL)))

#part 1 ----
total <- 0
n <- length(input)
for (i in 1:(n-1)) {
  if (input[i] == input[i+1]) total <- total + input[i]
}
if (input[n] == input[1]) total <- total + input[n]

print(total)


#part 2 ----
total <- 0
n <- length(input)
for (i in 1:(n-1)) {
  j <- (i + n/2) %% n
  if (j == 0) j <- n
  if (input[i] == input[j]) total <- total + input[i]
}

print(total)